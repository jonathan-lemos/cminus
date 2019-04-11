case class Quadruple(index: Int, op: String, par1: String = "", par2: String = "", par3: String = "") {
	override def toString: String = f"$index%-5d$op%-15s$par1%-15s$par2%-15s$par3%-15s"
}
private case class Ctx(symtab: SymTab, var index: Int = 1, var id: Int = 0) {
	def genTmp: String = {
		val ret = s"_t$id"
		id += 1
		ret
	}
}

object CodeGenerator {
	def genParenExpression(pe: ParenExpressionNode, c: Ctx): (Seq[Quadruple], String, Option[String]) = genExpression(pe.expr, c)

	def genNum(nn: NumNode): (Seq[Quadruple], String, Option[String]) = nn.value match {
		case Left(i) => (Seq(), i.toString, None)
		case Right(d) => (Seq(), d.toString, None)
	}

	def genVar(vn: VarNode, c: Ctx): (Seq[Quadruple], String, Option[String]) = vn.arrayInd match {
		case Some(e) =>
			val (expr, tmpvar) = evalIndex(evalExpr(genExpression(e, c), c), c, vn.identifier)
			(expr, tmpvar, None)

		case None => (Seq(), vn.identifier, None)
	}

	def genCall(cn: CallNode, c: Ctx): (Seq[Quadruple], String, Option[String]) = {
		val exprs = cn.args.map(p => evalExpr(genExpression(p, c), c))
		val params = exprs.map(e => {
			val ret = Quadruple(c.index, "arg", "", "", e._2)
			c.index += 1
			ret
		})

		val (call, tmp) = if (c.symtab.getFunc(cn.identifier).getOrElse(throw new IllegalStateException(s"Function ${cn.identifier} not in symtab")).ret == "void") {
			(Seq(Quadruple(c.index, "call", cn.identifier, params.length.toString)), "")
		}
		else {
			val tmp = c.genTmp
			(Seq(Quadruple(c.index, "call", cn.identifier, params.length.toString, tmp)), tmp)
		}

		c.index += 1
		(exprs.flatMap(_._1) ++ params ++ call, tmp, None)
	}

	def genFactor(f: Factor, c: Ctx): (Seq[Quadruple], String, Option[String]) = f match {
		case cn: CallNode => genCall(cn, c)
		case vn: VarNode => genVar(vn, c)
		case nn: NumNode => genNum(nn)
		case pe: ParenExpressionNode => genParenExpression(pe, c)
	}

	def genTerm(t: TermNode, c: Ctx): (Seq[Quadruple], String, Option[String]) = t.right match {
		case Some(s) =>
			val (mulop, rhs) = s
			val (lhsQuad, lhsTmp, _) = genFactor(t.left, c)
			val (rhsQuad, rhsTmp, _) = genTerm(rhs, c)
			val tmp = c.genTmp
			val op = if  (mulop == "*") "mul" else "div"
			val ret = (lhsQuad ++ rhsQuad ++ Seq(Quadruple(c.index, op, lhsTmp, rhsTmp, tmp)), tmp, None)
			c.index += 1
			ret

		case None => genFactor(t.left, c)
	}

	def genAdditiveExpression(ae: AdditiveExpressionNode, c: Ctx): (Seq[Quadruple], String, Option[String]) = ae.right match {
		case Some(s) =>
			val (addop, rhsExpr) = s
			val (lhsQuad, lhsTmp, _) = genTerm(ae.left, c)
			val (rhsQuad, rhsTmp, _) = genAdditiveExpression(rhsExpr, c)
			val tmp = c.genTmp
			val op = if (addop == "+") "add" else "sub"
			val ret = (lhsQuad ++ rhsQuad ++ Seq(Quadruple(c.index, op, lhsTmp, rhsTmp, tmp)), tmp, None)
			c.index += 1
			ret

		case None => genTerm(ae.left, c)
	}

	def genSimpleExpression(se: SimpleExpressionNode, c: Ctx): (Seq[Quadruple], String, Option[String]) = se.right match {
		case Some(s) =>
			val (lhsQuad, lhsTmp, _) = genAdditiveExpression(se.left, c)
			val (relop, rhsExpr) = s
			val (rhsQuad, rhsTmp, _) = genAdditiveExpression(rhsExpr, c)
			val tmp = c.genTmp
			val ret = (lhsQuad ++ rhsQuad ++ Seq(Quadruple(c.index, "comp", lhsTmp, rhsTmp, tmp)), tmp, Some(relop))
			c.index += 1
			ret

		case None => genAdditiveExpression(se.left, c)
	}

	def genAssignmentExpression(ae: AssignmentExpressionNode, c: Ctx): (Seq[Quadruple], String, Option[String]) = {
		val (pre, tmp) = ae.index match {
			case Some(e) => evalIndex(evalExpr(genExpression(e, c), c), c, ae.identifier)
			case None => (Seq(), "")
		}
		val (expr, tmpvar) = evalExpr(genExpression(ae.right, c), c)
		val v = if (tmp == "") ae.identifier else tmp
		val assgn = Seq(Quadruple(c.index, "assgn", tmpvar, "", v))
		c.index += 1
		(pre ++ expr ++ assgn, v, None)
	}

	def genExpression(e: Expression, c: Ctx): (Seq[Quadruple], String, Option[String]) = e match {
		case ae: AssignmentExpressionNode => genAssignmentExpression(ae, c)
		case se: SimpleExpressionNode => genSimpleExpression(se, c)
		case ae: AdditiveExpressionNode => genAdditiveExpression(ae, c)
		case t: TermNode => genTerm(t, c)
		case f: Factor => genFactor(f, c)
	}

	def genExpressionStatement(es: ExpressionStatementNode, c: Ctx): Seq[Quadruple] = {
		es.expression match {
			case Some(e) =>
				val (expr, _) = evalExpr(genExpression(e, c), c)
				expr
			case None => Seq()
		}
	}

	def evalCondition(e: (Seq[Quadruple], String, Option[String]), c: Ctx, jumpTo: Int): Seq[Quadruple] = {
		val (condition, tmpvar, relop) = e
		relop match {
			case Some(s) =>
				val op = s match {
					case ">"  => "brle"
					case ">=" => "brlt"
					case "<"  => "brge"
					case "<=" => "brgt"
					case "==" => "brne"
					case "!=" => "breq"
				}
				val cmp = Seq(Quadruple(c.index, op, tmpvar, "", jumpTo.toString))
				c.index += 1
				condition ++ cmp
			case None =>
				val tmp = c.genTmp
				val cmp = Seq(Quadruple(c.index, "comp", tmpvar, "0", tmp), Quadruple(c.index + 1, "breq", tmpvar, "", jumpTo.toString))
				c.index += 2
				condition ++ cmp
		}
	}

	def evalIndex(e: (Seq[Quadruple], String), c: Ctx, id: String): (Seq[Quadruple], String) = {
		val (quad, tmpvar) = e
		if (quad.isEmpty && tmpvar.matches("""\d+""")) {
			val tmp = c.genTmp
			val ret = Seq(Quadruple(c.index, "disp", id, (tmpvar.toInt * 4).toString, tmp))
			c.index += 1
			(ret, tmp)
		}
		else {
			val tmp = c.genTmp
			val tmp2 = c.genTmp
			val ret = (quad ++ Seq(Quadruple(c.index, "mul", tmpvar, 4.toString, tmp), Quadruple(c.index + 1, "disp", id, tmp, tmp2)), tmp2)
			c.index += 2
			ret
		}
	}

	def evalExpr(e: (Seq[Quadruple], String, Option[String]), c: Ctx): (Seq[Quadruple], String) = {
		val (expr, tmpvar, relop) = e
		relop match {
			case Some(s) =>
				val op = s match {
					case ">"  => "brle"
					case ">=" => "brlt"
					case "<"  => "brge"
					case "<=" => "brgt"
					case "==" => "brne"
					case "!=" => "breq"
				}

				val tmp = c.genTmp
				val ifTrue = Seq(Quadruple(c.index, op, tmpvar, "", (c.index + 3).toString), Quadruple(c.index + 1, "assign", "1", "", tmp), Quadruple(c.index + 2, "br", "", "", (c.index + 4).toString))
				c.index += 3
				val ifFalse = Seq(Quadruple(c.index, "assign", "0", "", tmp))
				c.index += 1
				(expr ++ ifTrue ++ ifFalse, tmp)
			case None =>
				(expr, tmpvar)
		}
	}

	def genReturnStatement(rs: ReturnStatementNode, c: Ctx): Seq[Quadruple] = {
		rs.expression match {
			case Some(e) =>
				val (expr, tmpvar) = evalExpr(genExpression(e, c), c)
				val ret = expr ++ Seq(Quadruple(c.index, "return", "", "", tmpvar))
				c.index += 1
				ret
			case None => Seq()
		}
	}

	def genIterationStatement(is: IterationStatementNode, c: Ctx): Seq[Quadruple] = {
		val indexOld = c.index
		val idOld = c.id
		evalCondition(genExpression(is.condition, c), c, 0)

		val stmt = if (is.statement.isInstanceOf[CompoundStatementNode]) {
			val stmtPt1 = Seq(Quadruple(c.index, "block"))
			c.index += 1
			val stmtPt2 = genStatement(is.statement, c)
			val stmtPt3 = Seq(Quadruple(c.index, "end", "block"))
			c.index += 1
			stmtPt1 ++ stmtPt2 ++ stmtPt3
		}
		else {
			genStatement(is.statement, c)
		}

		val indexTmp = c.index
		val idTmp = c.id
		c.index = indexOld
		c.id = idOld
		val condition = evalCondition(genExpression(is.condition, c), c, indexTmp + 1)
		c.index = indexTmp
		c.id = idTmp

		val ret = condition ++ stmt ++ Seq(Quadruple(c.index, "br", "", "", indexOld.toString))
		c.index += 1
		ret
	}

	def genSelectionStatement(ss: SelectionStatementNode, c: Ctx): Seq[Quadruple] = {
		val indexOld = c.index
		val idOld = c.id
		evalCondition(genExpression(ss.condition, c), c, 0)

		val stmt = if (ss.ifStatement.isInstanceOf[CompoundStatementNode]) {
			val stmtPt1 = Seq(Quadruple(c.index, "block"))
			c.index += 1
			val stmtPt2 = genStatement(ss.ifStatement, c)
			val stmtPt3 = Seq(Quadruple(c.index, "end", "block"))
			c.index += 1
			stmtPt1 ++ stmtPt2 ++ stmtPt3
		}
		else {
			genStatement(ss.ifStatement, c)
		}

		val indexTmp = c.index
		val idTmp = c.id
		c.index = indexOld
		c.id = idOld
		val condition = evalCondition(genExpression(ss.condition, c), c, indexTmp + (if (ss.elseStatement.isDefined) 1 else 0))
		c.index = indexTmp
		c.id = idTmp

		ss.elseStatement match {
			case Some(s) =>
				c.index += 1 // br

				val elsestmt = if (s.isInstanceOf[CompoundStatementNode]) {
					val elsestmtPt1 = Seq(Quadruple(c.index, "block"))
					c.index += 1
					val elsestmtPt2 = genStatement(s, c)
					val elsestmtPt3 = Seq(Quadruple(c.index, "end", "block"))
					c.index += 1
					elsestmtPt1 ++ elsestmtPt2 ++ elsestmtPt3
				}
				else {
					genStatement(s, c)
				}

				condition ++ stmt ++ Seq(Quadruple(indexTmp, "br", "", "", c.index.toString)) ++ elsestmt

			case None => condition ++ stmt
		}
	}

	def genStatement(s: Statement, c: Ctx): Seq[Quadruple] = {
		s match {
			case a: CompoundStatementNode => genCompoundStatement(a, c)
			case a:	SelectionStatementNode => genSelectionStatement(a, c)
			case a: IterationStatementNode => genIterationStatement(a, c)
			case a: ReturnStatementNode => genReturnStatement(a, c)
			case a: ExpressionStatementNode => genExpressionStatement(a, c)
		}
	}

	def genCompoundStatement(cs: CompoundStatementNode, c: Ctx): Seq[Quadruple] = {
		val localDecs = cs.vardecls.flatMap(s => genVarDecl(s, c))
		val statements = cs.statements.flatMap(s => genStatement(s, c))
		localDecs ++ statements
	}

	def genFunDecl(fd: FunDeclNode, c: Ctx): Seq[Quadruple] = {
		val head = Seq(Quadruple(c.index, "func", fd.identifier, fd.returnType, fd.params.length.toString))
		c.index += 1
		val params = fd.params.map(s => {
			val ret = Quadruple(c.index, "param", "", "", s.identifier)
			c.index += 1
			ret
		}) ++ fd.params.map(s => {
			val ret = Quadruple(c.index, "alloc", "4", "", s.identifier)
			c.index += 1
			ret
		})
		val cps = genCompoundStatement(fd.body, c)
		val tail = Seq(Quadruple(c.index, "end", "func", fd.identifier))
		c.index += 1
		head ++ params ++ cps ++ tail
	}

	def genVarDecl(vd: VarDeclNode, c: Ctx): Seq[Quadruple] = vd.arrayLen match {
		case Some(i) => vd.typename match {
			case "int" =>
				val ret = Seq(Quadruple(c.index, "alloc", (i * 4).toString, "", vd.identifier))
				c.index += 1
				ret
			case "float" =>
				val ret = Seq(Quadruple(c.index, "alloc", (i * 4).toString, "", vd.identifier))
				c.index += 1
				ret
		}
		case None =>
			val ret = Seq(Quadruple(c.index, "alloc", "4", "", vd.identifier))
			c.index += 1
			ret
	}

	def genDeclaration(d: Declaration, c: Ctx): Seq[Quadruple] = d match {
		case vd: VarDeclNode => genVarDecl(vd, c)
		case fd: FunDeclNode => genFunDecl(fd, c)
	}

	def apply(root: ProgramNode, s: SymTab): Seq[Quadruple] = {
		val ctx = Ctx(s)
		root.declarations.flatMap(s => genDeclaration(s, ctx))
	}
}

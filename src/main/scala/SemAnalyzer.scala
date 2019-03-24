import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

sealed trait SymTabType
final case class RegType(typ: String, arrayLen: Option[Int] = None) extends SymTabType {
	override def toString: String = arrayLen match {
		case Some(len) => s"$typ[$len]"
		case None => typ
	}
}
final case class FuncType(ret: String, param: Seq[ParamNode]) extends SymTabType {
	override def toString: String = s"$ret(" + param.foldLeft("")(_ + "," + _.typename).substring(1) + "})"
}

class SemAnalyzerException(s: String, line: Int) extends IllegalArgumentException(s) {
	def prettyPrint(): Unit = {
		Color.printRed("Error")
		Color.printYellow(s"(Line $line): ")
		Color.printPurple(s + "\n")
	}
}

private final class SymTab {
	private val varList = new ListBuffer[mutable.HashMap[String, RegType]]
	private val funcList = new mutable.HashMap[String, FuncType]
	private var curRetType: Option[RegType] = None
	private var retTypeHit: Boolean = false

	varList.+=:(new mutable.HashMap[String, RegType])

	def add(id: String, t: SymTabType): Boolean = t match {
		case r: RegType =>
			if (varList.head.contains(id)) false
			else { varList.head += ((id, r)); true }
		case f: FuncType =>
			if (funcList.contains(id)) false
			else { funcList += ((id, f)); pushScope(); curRetType = Some(RegType(f.ret)); retTypeHit = f.ret == "void"; true }
	}

	def pushScope(): Unit = varList.+=:(new mutable.HashMap[String, RegType])
	def popScope(): Unit = { varList.remove(0); () }

	def getReg(id: String): Option[RegType] = {
		for (t <- varList) {
			if (t.contains(id)) {
				return Some(t(id))
			}
		}
		None
	}

	def getFunc(id: String): Option[FuncType] = if (funcList.contains(id)) Some(funcList(id)) else None

	def checkReturnType(r: RegType): Boolean = curRetType match {
		case Some(rt) =>
			if (r == rt) { this.retTypeHit = true; true}
			else false
		case None => false
	}

	def returnHit: Boolean = retTypeHit
}

object SemAnalyzer {
	def compareTypesExpr(lhs: RegType, rhs: RegType, line: Int): Try[RegType] = {
		if (lhs.typ == "void" || rhs.typ == "void") return Failure(new SemAnalyzerException("Type void is not allowed in an expression", line))
		if (lhs.arrayLen.isDefined || rhs.arrayLen.isDefined) return Failure(new SemAnalyzerException("Array type is not allowed in an expression", line))
		if (lhs.typ != rhs.typ) return Failure(new SemAnalyzerException(s"Type '${lhs.typ}' does not match type '${rhs.typ}'", line))
		Success(RegType(lhs.typ))
	}

	def compareTypesParam(exp: ParamNode, act: RegType, line: Int): Try[Unit] = {
		val actPn = ParamNode(exp.line, act.typ, exp.identifier, act.arrayLen.isDefined)
		if (exp != actPn) return Failure(new SemAnalyzerException(s"Expected parameter of type '$exp' but found unrelated type '$actPn'", line))
		Success(())
	}

	def analyzeParenExpression(pe: ParenExpressionNode, st: SymTab): Try[RegType] = analyzeExpression(pe.expr, st)

	def analyzeNumNode(n: NumNode): Try[RegType] = n.value match {
		case Left(_) => Success(RegType("int"))
		case Right(_) => Success(RegType("float"))
	}

	def analyzeVar(v: VarNode, st: SymTab): Try[RegType] = {
		val typ = st.getReg(v.identifier) getOrElse(return Failure(new SemAnalyzerException(s"Identifier '${v.identifier}' was not previously declared", v.line)))
		v.arrayInd match {
			case Some(expr) =>
				if (typ.arrayLen.isEmpty) return Failure(new SemAnalyzerException(s"Cannot take array index of non-array '${v.identifier}'", v.line))
				analyzeExpression(expr, st) match {
					case Success(rt) if rt.typ == "int" => Success(RegType(typ.typ))
					case Success(rt) => Failure(new SemAnalyzerException(s"Expected int expression but found '$rt' expression", v.line))
					case Failure(e) => Failure(e)
				}
			case None => Success(typ)
		}
	}

	def analyzeCall(cn: CallNode, st: SymTab): Try[RegType] = {
		val ftype = st.getFunc(cn.identifier) getOrElse(return Failure(new SemAnalyzerException(s"Identifier '${cn.identifier}' was not previously declared.", cn.line)))
		if (ftype.param.length != cn.args.length) return Failure(new SemAnalyzerException(s"Expected ${ftype.param.length} parameters but got ${cn.args.length} parameters", cn.line))
		cn.args.zip(ftype.param).foreach(t => {
			analyzeExpression(t._1, st) match {
				case Success(rt) => compareTypesParam(t._2, rt, cn.line) match {case Failure(e) => return Failure(e); case _ =>}
				case Failure(e) => return Failure(e)
			}
		})
		Success(RegType(ftype.ret))
	}

	def analyzeFactor(f: Factor, st: SymTab): Try[RegType] = f match {
		case cn: CallNode => analyzeCall(cn, st)
		case vn: VarNode => analyzeVar(vn, st)
		case n: NumNode => analyzeNumNode(n)
		case p: ParenExpressionNode => analyzeParenExpression(p, st)
	}

	def analyzeTerm(t: TermNode, st: SymTab): Try[RegType] = {
		val lhsType: RegType = analyzeFactor(t.left, st) match {case Success(l) => l; case Failure(e) => return Failure(e)}
		t.right match {
			case Some((_, a)) => analyzeTerm(a, st) match {
				case Success(rhsType) => compareTypesExpr(lhsType, rhsType, t.line)
				case Failure(e) => Failure(e)
			}
			case None => Success(lhsType)
		}
	}

	def analyzeAdditiveExpression(ae: AdditiveExpressionNode, st: SymTab): Try[RegType] = {
		val lhsType: RegType = analyzeTerm(ae.left, st) match {case Success(l) => l; case Failure(e) => return Failure(e)}
		ae.right match {
			case Some((_, a)) => analyzeAdditiveExpression(a, st) match {
				case Success(rhsType) => compareTypesExpr(lhsType, rhsType, ae.line)
				case Failure(e) => Failure(e)
			}
			case None => Success(lhsType)
		}
	}

	def analyzeSimpleExpression(se: SimpleExpressionNode, st: SymTab): Try[RegType] = {
		val lhsType: RegType = analyzeAdditiveExpression(se.left, st) match {case Success(l) => l; case Failure(e) => return Failure(e)}
		se.right match {
			case Some((_, ae)) => analyzeAdditiveExpression(ae, st) match {
				case Success(rhsType) => compareTypesExpr(lhsType, rhsType, se.line)
				case Failure(e) => Failure(e)
			}
			case None => Success(lhsType)
		}
	}

	def analyzeAssignmentExpression(ae: AssignmentExpressionNode, st: SymTab): Try[RegType] = {
		val lhsType: RegType = st.getReg(ae.identifier) match {
			case Some(r: RegType) => ae.index match {
				case Some(_) if r.arrayLen.isDefined => RegType(r.typ)
				case Some(_) => return Failure(new SemAnalyzerException(s"Cannot index non-array '${ae.identifier}'", ae.line))
				case None if r.arrayLen.isEmpty => RegType(r.typ)
				case None => return Failure(new SemAnalyzerException(s"Cannot assign to array '${ae.identifier}'", ae.line))
			}
			case None => return Failure(new SemAnalyzerException(s"Undeclared identifier '${ae.identifier}' used", ae.line))
		}
		analyzeExpression(ae.right, st) match {
			case Success(rhsType) => compareTypesExpr(lhsType, rhsType, ae.line)
			case Failure(e) => Failure(e)
		}
	}

	def analyzeExpression(e: Expression, st: SymTab): Try[RegType] = e match {
		case ae: AssignmentExpressionNode => analyzeAssignmentExpression(ae, st)
		case se: SimpleExpressionNode => analyzeSimpleExpression(se, st)
		case ae: AdditiveExpressionNode => analyzeAdditiveExpression(ae, st)
		case t: TermNode => analyzeTerm(t, st)
		case f: Factor => analyzeFactor(f, st)
	}

	def analyzeExpressionStatement(es: ExpressionStatementNode, st: SymTab): Try[Unit] = {
		es.expression match {
			case Some(expr) => analyzeExpression(expr, st) match {
				case Success(_) => Success(())
				case Failure(e) => Failure(e)
			}
			case None => Success(())
		}
	}

	def analyzeReturnStatement(rs: ReturnStatementNode, st: SymTab): Try[Unit] = {
		rs.expression match {
			case Some(expr) => analyzeExpression(expr, st) match {
				case Failure(e) => Failure(e)
				case Success(rt) =>
					if (st.checkReturnType(rt)) Success(())
					else Failure(new SemAnalyzerException(s"Return type '${rt.typ}' does not match expected return type", rs.line))
			}
			case None =>
				if (st.checkReturnType(RegType("void"))) Success(())
				else Failure(new SemAnalyzerException(s"Return type 'void' does not match expected return type", rs.line))
		}
	}

	def analyzeIterationStatement(is: IterationStatementNode, st: SymTab): Try[Unit] = {
		analyzeExpression(is.condition, st) match {case Failure(e) => return Failure(e); case _ =>}
		analyzeStatement(is.statement, st) match {case Failure(e) => return Failure(e); case _ =>}
		Success(())
	}

	def analyzeSelectionStatement(ss: SelectionStatementNode, st: SymTab): Try[Unit] = {
		analyzeExpression(ss.condition, st) match {case Failure(e) => return Failure(e); case _ =>}
		analyzeStatement(ss.ifStatement, st) match {case Failure(e) => return Failure(e); case _ =>}
		ss.elseStatement match {
			case Some(s) => analyzeStatement(s, st) match {case Failure(e) => Failure(e); case _ => Success(())}
			case None => Success(())
		}
	}

	def analyzeStatement(s: Statement, st: SymTab): Try[Unit] = s match {
		case c: CompoundStatementNode => analyzeCompoundStatement(c, st)
		case s: SelectionStatementNode => analyzeSelectionStatement(s, st)
		case i: IterationStatementNode => analyzeIterationStatement(i, st)
		case r: ReturnStatementNode => analyzeReturnStatement(r, st)
		case e: ExpressionStatementNode => analyzeExpressionStatement(e, st)
	}

	def analyzeCompoundStatement(cs: CompoundStatementNode, st: SymTab): Try[Unit] = {
		st.pushScope()
		for (v <- cs.vardecls) analyzeVarDecl(v, st) match {case Failure(e) => return Failure(e); case _ =>}
		for (s <- cs.statements) analyzeStatement(s, st) match {case Failure(e) => return Failure(e); case _ =>}
		st.popScope()
		Success(())
	}

	def analyzeVarDecl(vd: VarDeclNode, st: SymTab): Try[Unit] = {
		if (vd.arrayLen.isDefined && vd.arrayLen.get < 0) return Failure(new SemAnalyzerException(s"Cannot declare array of negative size (${vd.identifier})", vd.line))
		val lhs = RegType(vd.typename, vd.arrayLen)
		if (lhs.typ == "void") return Failure(new SemAnalyzerException(s"Cannot instantiate a variable of type 'void'.", vd.line))
		if (!st.add(vd.identifier, lhs)) return Failure(new SemAnalyzerException(s"Identifier ${vd.identifier} already declared.", vd.line))
		vd.right.map(analyzeExpression(_, st) match {
			case Success(e) => compareTypesExpr(lhs, RegType(e.typ, e.arrayLen), vd.line) match {
				case Success(_) =>
				case Failure(ex) => return Failure(ex)
			}
			case Failure(e) => return Failure(e)
		})
		Success(())
	}

	def analyzeFunDecl(fd: FunDeclNode, st: SymTab): Try[Unit] = {
		if (!st.add(fd.identifier, FuncType(fd.returnType, fd.params))) return Failure(new SemAnalyzerException(s"Duplicate function name '${fd.identifier}'", fd.line))
		fd.params.foreach(p =>
			if (!st.add(p.identifier, RegType(p.typename, if (p.array) Some(0) else None))) return Failure(new SemAnalyzerException(s"Duplicate param name '${p.identifier}'", fd.line))
		)
		analyzeCompoundStatement(fd.body, st) match {case Failure(e) => return Failure(e); case _ =>}
		if (!st.returnHit) return Failure(new SemAnalyzerException(s"Missing return statement", fd.line))
		st.popScope()
		Success(())
	}

	def analyzeProgramNode(pn: ProgramNode, st: SymTab): Try[Unit] = {
		if (pn.declarations.isEmpty) return Failure(new SemAnalyzerException("A program must have at least one declaration", pn.line))
		pn.declarations.last match {
			case fd: FunDeclNode =>
				if (fd.identifier != "main") return Failure(new SemAnalyzerException("The last declaration must be a function named 'main'", fd.line));
			case _ => return Failure(new SemAnalyzerException("The last declaration must be a function named 'main'", pn.line));
		}
		for (p <- pn.declarations) {
			(p match {
				case v: VarDeclNode => analyzeVarDecl(v, st)
				case f: FunDeclNode => analyzeFunDecl(f, st)
			}) match {case Failure(e) => return Failure(e); case _ =>}
		}
		Success(())
	}

	def apply(root: ProgramNode): Try[Unit] = {
		val symtab = new SymTab
		analyzeProgramNode(root, symtab)
	}
}

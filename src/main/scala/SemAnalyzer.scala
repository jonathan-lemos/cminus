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
	private val list = new ListBuffer[mutable.HashMap[String, SymTabType]]
	private var retType: Option[RegType] = None
	private var rtHit: Boolean = false
	list += new mutable.HashMap[String, SymTabType]

	def has(id: String, t: SymTabType): Boolean = {
		for (c <- list) if (c.contains(id) && c(id) == t) return true
		false
	}

	def hasId(id: String): Boolean = {
		for (c <- list) if (c.contains(id)) return true
		false
	}

	def getIdType(id: String): Option[SymTabType] = {
		for (c <- list) if (c.contains(id)) return Some(c(id))
		None
	}

	def add(id: String, t: SymTabType): Unit = {list.last += ((id, t)); () }
	def pushScope(): Unit = list += new mutable.HashMap[String, SymTabType]
	def popScope(): Unit = { if (list.nonEmpty) list.remove(list.length - 1); () }
	def setRt(r: Option[RegType]): Unit = { this.retType = r; rtHit = if (r.isDefined) r.get.typ == "void" else false }
	def getRt: Option[RegType] = this.retType
	def getHitRt: Boolean = this.rtHit
	def setHitRt(b: Boolean): Unit = this.rtHit = b
}

object SemAnalyzer {
	def compareTypesExpr(lhs: RegType, rhs: RegType, line: Int): Try[RegType] = {
		if (lhs.typ == "void" || rhs.typ == "void") return Failure(new SemAnalyzerException("Type void is not allowed in an expression", line))
		if (lhs.arrayLen.isDefined || rhs.arrayLen.isDefined) return Failure(new SemAnalyzerException("Array type is not allowed in an expression", line))
		if (lhs.typ == "float" || rhs.typ == "float") return Success(RegType("float"))
		Success(RegType("int"))
	}

	def compareTypesParam(exp: ParamNode, paramLine: Int, act: RegType): Try[Unit] = {
		val actPn = ParamNode(exp.line, act.typ, exp.identifier, act.arrayLen.isDefined)
		if (exp != actPn) Failure(new SemAnalyzerException(s"Expected parameter of type '$exp' but found unrelated type '$actPn'", paramLine))
		Success(())
	}

	def analyzeParenExpression(pe: ParenExpressionNode, st: SymTab): Try[RegType] = analyzeExpression(pe.expr, st)

	def analyzeNumNode(n: NumNode): Try[RegType] = n.value match {case Left(_) => Success(RegType("int")); case Right(_) => Success(RegType("float"))}

	def analyzeVar(v: VarNode, st: SymTab): Try[RegType] = {
		val typ = st.getIdType(v.identifier) getOrElse(return Failure(new SemAnalyzerException(s"Identifier '${v.identifier}' was not previously declared", v.line))) match {
			case r: RegType => r
			case _: FuncType => return Failure(new SemAnalyzerException("Expected var but found function", v.line))
		}
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
		val ftype = st.getIdType(cn.identifier) getOrElse(return Failure(new SemAnalyzerException(s"Identifier '${cn.identifier}' was not previously declared.", cn.line))) match {
			case ft: FuncType => ft
			case _: RegType => return Failure(new SemAnalyzerException(s"Identifier '${cn.identifier}' does not refer to a function", cn.line))
		}
		if (ftype.param.length != cn.args.length) return Failure(new SemAnalyzerException(s"Expected ${ftype.param.length} parameters but got ${cn.args.length} parameters", cn.line))
		cn.args.zip(ftype.param).foreach(t => {
			analyzeExpression(t._1, st) match {
				case Success(rt) => compareTypesParam(t._2, cn.line, rt) match {case Failure(e) => return Failure(e); case _ =>}
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
		val lhsType: RegType = st.getIdType(ae.identifier) match {
			case Some(r: RegType) => ae.index match {
				case Some(_) if r.arrayLen.isDefined => RegType(r.typ)
				case Some(_) => return Failure(new SemAnalyzerException(s"Cannot index non-array '${ae.identifier}'", ae.line))
				case None if r.arrayLen.isEmpty => RegType(r.typ)
				case None => return Failure(new SemAnalyzerException(s"Cannot assign to array '${ae.identifier}'", ae.line))
			}
			case Some(_: FuncType) => return Failure(new SemAnalyzerException(s"Cannot assign to function '${ae.identifier}'", ae.line))
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
				case Success(rt) => st.getRt match {
					case Some(s) => compareTypesExpr(rt, s, rs.line) match {case Failure(e) => Failure(e); case Success(_) => st.setHitRt(true); Success(())}
					case None => Failure(new SemAnalyzerException("Return statement outside of function", rs.line))
				}
			}
			case None => st.getRt match {
				case Some(s) if s.typ == "void" => Success(())
				case Some(_) => Failure(new SemAnalyzerException(s"Expected return type void but found expression", rs.line))
				case None => Failure(new SemAnalyzerException("Return statement outside of function", rs.line))
			}
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
		if (st.hasId(vd.identifier)) return Failure(new SemAnalyzerException(s"Identifier '${vd.identifier}' was already declared.", vd.line))
		if (lhs.typ == "void") return Failure(new SemAnalyzerException(s"Cannot instantiate a variable of type 'void'.", vd.line))
		vd.right.map(analyzeExpression(_, st) match {
			case Success(e) => compareTypesExpr(lhs, RegType(e.typ, e.arrayLen), vd.line) match {
				case Success(_) =>
				case Failure(ex) => return Failure(ex)
			}
			case Failure(e) => return Failure(e)
		})
		st.add(vd.identifier, RegType(vd.typename, vd.arrayLen))
		Success(())
	}

	def analyzeFunDecl(fd: FunDeclNode, st: SymTab): Try[Unit] = {
		if (st.hasId(fd.identifier)) return Failure(new SemAnalyzerException(s"Identifier '${fd.identifier}' was already declared.", fd.line))
		st.add(fd.identifier, FuncType(fd.returnType, fd.params))
		st.pushScope()
		fd.params.foreach(p => {
			if (st.hasId(p.identifier)) return Failure(new SemAnalyzerException(s"Param identifier '${p.identifier}' was already declared.", fd.line))
			st.add(p.identifier, RegType(p.typename, if (p.array) Some(0) else None))
		})
		st.setRt(Some(RegType(fd.returnType, None)))
		analyzeCompoundStatement(fd.body, st) match {case Failure(e) => return Failure(e); case _ =>}
		if (!st.getHitRt) return Failure(new SemAnalyzerException(s"Expected return statement of type '${fd.returnType}' but no return statement found.", fd.line))
		st.popScope()
		st.setRt(None)
		Success(())
	}

	def analyzeProgramNode(pn: ProgramNode, st: SymTab): Try[Unit] = {
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

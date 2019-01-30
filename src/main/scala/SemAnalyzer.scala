/*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

private sealed trait SymTabType
private final case class RegType(typ: String, arrayLen: Option[Int] = None) extends SymTabType {
	override def toString: String = arrayLen match {
		case Some(len) => s"$typ[$len]"
		case None => typ
	}
}
private final case class FuncType(ret: String, param: Seq[ParamNode]) extends SymTabType {
	override def toString: String = s"$ret(" + param.foldLeft("")(_ + "," + _.typename).substring(1) + "})"
}

class SemAnalyzerException(s: String) extends IllegalArgumentException(s)

private final class SymTab {
	private val list = new ListBuffer[mutable.HashMap[String, SymTabType]]
	var retType: Option[RegType] = None
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

	def add(id: String, t: SymTabType): Unit = list.last += ((id, t))
	def pushScope(): Unit = list += new mutable.HashMap[String, SymTabType]
	def popScope(): Unit = if (list.nonEmpty) list.remove(list.length - 1)
}

object SemAnalyzer {
	def compareTypes(lhs: RegType, rhs: RegType): Try[RegType] = {
		if (lhs.typ == "void" || rhs.typ == "void") Failure(new SemAnalyzerException("Type void is not allowed in an expression"))
		if (lhs.typ == "float" || rhs.typ == "float")
	}

	def analyzeFactor(f: Factor, st: SymTab): Try[RegType] = {

	}

	def analyzeTerm(t: TermNode, st: SymTab): Try[RegType] = {
		val lhsType: RegType = analyzeFactor(t.left, st) match {case Success(l) => l; case Failure(e) => return Failure(e)}
		t.right match {
			case Some((_, a)) => analyzeTerm(a, st) match {
				case Success(rhsType) if
			}
		}
	}

	def analyzeAdditiveExpression(ae: AdditiveExpressionNode, st: SymTab): Try[RegType] = {
		val lhsType: RegType = analyzeTerm(ae.left, st) match {case Success(l) => l; case Failure(e) => return Failure(e)}
		ae.right match {
			case Some((_, a)) => analyzeAdditiveExpression(a, st) match {
				case Success(rhsType) if rhsType == lhsType => Success(rhsType)
				case Success(rhsType) => Failure(new SemAnalyzerException(s"Cannot compare types $lhsType and $rhsType"))
				case Failure(e) => Failure(e)
			}
			case None => Success(lhsType)
		}
	}

	def analyzeSimpleExpression(se: SimpleExpressionNode, st: SymTab): Try[RegType] = {
		val lhsType: RegType = analyzeAdditiveExpression(se.left, st) match {case Success(l) => l; case Failure(e) => return Failure(e)}
		se.right match {
			case Some((_, ae)) => analyzeAdditiveExpression(ae, st) match {
				case Success(rhsType) if rhsType == lhsType => Success(rhsType)
				case Success(rhsType) => Failure(new SemAnalyzerException(s"Cannot compare types $lhsType and $rhsType"))
				case Failure(e) => Failure(e)
			}
			case None => Success(lhsType)
		}
	}

	def analyzeAssignmentExpression(ae: AssignmentExpressionNode, st: SymTab): Try[RegType] = {
		analyzeExpression(ae.right, st) match {
			case Success(rt) => st.getIdType(ae.identifier) match {
				case Some(r) if r == rt => Success(rt)
				case Some(r) => Failure(new SemAnalyzerException(s"${ae.identifier} is of type $r but expression of type $rt assigned to it"))
				case None => Failure(new SemAnalyzerException(s"Undeclared identifier ${ae.identifier} used"))
			}
			case Failure(e) => Failure(e)
		}
	}

	def analyzeExpression(e: Expression, st: SymTab): Try[RegType] = e match {
		case ae: AssignmentExpressionNode =>
	}

	def analyzeExpressionStatement(es: ExpressionStatementNode, st: SymTab): Try[Unit] = {
		es.expression match {
			case Some(expr) => analyzeExpression (expr, st) match {
				case Success(_) => Success()
				case Failure(e) => Failure(e)
			}
			case None => Success()
		}
	}

	def analyzeReturnStatement(rs: ReturnStatementNode, st: SymTab): Try[Unit] = {
		rs.expression match {
			case Some(expr) => analyzeExpression(expr, st) {
				case Failure(e) => return Failure(e)
				case Success(rt) => st.retType match {
					case Some(s) => if (rt == s) Success() else Failure(new SemAnalyzerException(s"Expected return type $s but got return type $rt"))
					case None => Failure(new SemAnalyzerException("Return statement outside of function"))
				}
			}
			case None => st.retType match {
				case Some(s) if s.typ == "void" => Success()
				case Some(_) => Failure(new SemAnalyzerException(s"Expected return type void but found expression"))
				case None => Failure(new SemAnalyzerException("Return statement outside of function"))
			}
		}
	}

	def analyzeIterationStatement(is: IterationStatementNode, st: SymTab): Try[Unit] = {
		analyzeExpression(is.condition, st) match {case Failure(e) => return Failure(e); case _ =>}
		analyzeStatement(is.statement, st) match {case Failure(e) => return Failure(e); case _ =>}
		Success()
	}

	def analyzeSelectionStatement(ss: SelectionStatementNode, st: SymTab): Try[Unit] = {
		analyzeExpression(ss.condition, st) match {case Failure(e) => return Failure(e); case _ =>}
		analyzeStatement(ss.ifStatement, st) match {case Failure(e) => return Failure(e); case _ =>}
		ss.elseStatement match {case Some(s) => analyzeStatement(s, st) match {case Failure(e) => return Failure(e); case _ =>}}
		Success()
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
		Success()
	}

	def analyzeVarDecl(vd: VarDeclNode, st: SymTab): Try[Unit] = {
		if (st.hasId(vd.identifier)) Failure(new SemAnalyzerException(s"Identifier ${vd.identifier} was already declared."))
		if (vd.typename == "void") Failure(new SemAnalyzerException(s"Cannot instantiate a variable of type void."))
		st.add(vd.identifier, RegType(vd.typename, vd.arrayLen))
		Success()
	}

	def analyzeFunDecl(fd: FunDeclNode, st: SymTab): Try[Unit] = {
		st.add(fd.identifier, FuncType(fd.returnType, fd.params))
		st.retType = Some(RegType(fd.returnType, None))
		analyzeCompoundStatement(fd.body, st) match {case Failure(e) => return Failure(e); case _ =>}
		st.retType = None
		Success()
	}

	def analyzeProgramNode(pn: ProgramNode, st: SymTab): Try[Unit] = {
		for (p <- pn.declarations) {
			p match {
				case v: VarDeclNode => analyzeVarDecl(v, st) match {case Failure(e) => return Failure(e); case _ =>}
				case f: FunDeclNode => analyzeFunDecl(f, st) match {case Failure(e) => return Failure(e); case _ =>}
			}
		}
		Success()
	}

	def apply(root: ProgramNode): Try[Unit] = {
		val symtab = new SymTab
	}
}
*/

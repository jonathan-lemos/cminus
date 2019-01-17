import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success, Try}

class ParseTree //TODO

object ASTType extends Enumeration {
	val ASSIGNMENT_EXPRESSION, ADDITIVE_EXPRESSION, CALL, COMPOUND_STATEMENT, EXPRESSION_STATEMENT, EXPRESSION, FACTOR, FUNDECL, IF, ITERATION_STATEMENT, NUMBER, PARAM, PROGRAM, RELOP_EXPRESSION, RETURN_STATEMENT, SELECTION_STATEMENT, SIMPLE_EXPRESSION, STATEMENT, TERM, VARDECL, VARIABLE, WHILE = Value
}

/**
  * CFG for this language:
  *
  */

case class   ASTNode(typ: ASTType.Value)
case class   ProgramNode(declarations: Seq[Declaration])                                                             extends ASTNode(ASTType.PROGRAM)
sealed trait Declaration
case class   VarDeclNode(typename: String, identifier: String, arrayLen: Option[Int] = None)                         extends ASTNode(ASTType.VARDECL) with Declaration
case class   FunDeclNode(returnType: String, identifier: String, params: Seq[ParamNode], body: Seq[Statement])       extends ASTNode(ASTType.FUNDECL) with Declaration
case class   ParamNode(typename: String, identifier: String)                                                         extends ASTNode(ASTType.PARAM)
sealed trait Statement
case class   SelectionStatementNode(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends ASTNode(ASTType.SELECTION_STATEMENT) with Statement
case class   IterationStatementNode(condition: Expression, statement: Statement)                                     extends ASTNode(ASTType.ITERATION_STATEMENT) with Statement
case class   ReturnStatementNode(expression: Option[Expression])                                                     extends ASTNode(ASTType.RETURN_STATEMENT) with Statement
case class   ExpressionStatementNode(expression: Option[Expression])                                                 extends ASTNode(ASTType.EXPRESSION_STATEMENT) with Statement
sealed trait Expression
case class   AssignmentExpressionNode(identifier: String, right: Expression)                                         extends ASTNode(ASTType.ASSIGNMENT_EXPRESSION) with Expression
case class   SimpleExpressionNode(expression: Either[RelopExpressionNode, AdditiveExpressionNode])                   extends ASTNode(ASTType.SIMPLE_EXPRESSION) with Expression
case class   RelopExpressionNode(left: AdditiveExpressionNode, right: Option[(String, AdditiveExpressionNode)])      extends ASTNode(ASTType.RELOP_EXPRESSION) with Expression
case class   AdditiveExpressionNode(left: Option[(AdditiveExpressionNode, String)], right: TermNode)                 extends ASTNode(ASTType.ADDITIVE_EXPRESSION) with Expression
case class   TermNode(left: Option[(TermNode, String)], right: FactorNode)                                           extends ASTNode(ASTType.TERM) with Expression
case class   CallNode(identifier: String, args: Expression)                                                          extends ASTNode(ASTType.CALL) with Expression with Factor
sealed trait Factor
case class   FactorNode(factor: Expression)                                                                          extends ASTNode(ASTType.FACTOR) with Expression with Factor
case class   NumberNode(num: String)                                                                                 extends ASTNode(ASTType.NUMBER) with Expression with Factor

class TokStream(private val tok: Seq[Token]) {
	private val str = tok.to[ListBuffer[Token]]
	def peek: Option[Token] = str.headOption
	def extract: Token = {
		if (this.empty) {
			throw new IllegalArgumentException("This TokStream is empty")
		}
		val ret = str.head
		str.remove(0)
		Some(ret)
	}
	def extractOption: Option[Token] = {
		if (this.empty) {
			None
		}
		val ret = str.head
		str.remove(0)
		Some(ret)
	}
	def nonEmpty: Boolean = str.nonEmpty
	def empty: Boolean = str.isEmpty
	def skipLine: Unit = {
		if (this.empty) return
		while (this.peek.nonEmpty)
	}
}

object Parser {
	private val rules: HashMap[String, String] = HashMap(
		("program", "declaration-list"),
		("declaration-list", "declaration-list declaration|declaration"),
		("declaration", "var-declaration|fun-declaration"),
		("var-declaration", "TYPE ID ;|TYPE ID [ NUM ] ;"),
		("fun-declaration", "TYPE ID ( params ) compound-stmt"),
		("params", "param-list|void"),
		("param-list", "param-list , param|param"),
		("param", "TYPE ID|TYPE ID [ ]"),
		("compound-stmt", "{ local-declarations statement-list }"),
		("local-declarations", "local-declarations var-declaration|NULL"),
		("statement-list", "statement-list statement|NULL"),
		("statement", "expression-stmt|compound-stmt|selection-stmt|iteration-stmt|return-stmt"),
		("expression-stmt", "expression ;|;"),
		("selection-stmt", "if ( expression ) statement|if ( expression ) statement else statement"),
		("iteration-stmt", "while ( expression ) statement"),
		("return-stmt", "return ;|return expression ;"),
		("expression", "var = expression|simple-expression"),
		("var", "ID|ID [ expression ]"),
		("simple-expression", "additive-expression RELOP additive-expression|additive-expression"),
		("additive-expression", "additive-expression ADDOP term|term"),
		("term", "term MULOP factor|factor"),
		("factor", "( expression )|var|call|NUM"),
		("call", "ID ( args )"),
		("args", "arg-list|empty"),
		("arg-list", "arg-list , expression|expression"),
	)

	def readCompoundStmt(stream: TokStream): Try[Seq[Statement]] = {

	}

	def readFunDecl(typename: String, identifier: String, stream: TokStream): Try[FunDeclNode] = {

	}

	def readVarDeclArray(typename: String, identifier: String, stream: TokStream): Try[VarDeclNode] = {
		// remaining tokens to match are NUM ]

		var num = 0.0
		// match NUM
		stream.peek match {
			case Some(tok) =>
				tok.tok match {
					case TokenType.NUMBER => try { num = tok.text.toInt } catch { case e: NumberFormatException => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected INTEGER, but found \"${tok.text}\""))}
					case _ => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected NUMBER, but found ${tok.tok}"))
				}
			case None => Failure(new IllegalArgumentException(s"Expected NUMBER, but stream is empty"))
		}
		stream.extract

		// check if next token is PUNCTUATION
		stream.peek match {
			case Some(tok) =>
				tok.tok match {
					case TokenType.PUNCTUATION =>
					case _ => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected \"]\" but found ${tok.tok}"))
				}
			case None => Failure(new IllegalArgumentException(s"Expected \"]\", but stream is empty"))
		}

		val next = stream.extract
		next.text match {
			case "]" => Success(VarDeclNode(typename, identifier, num))
			case _ => Failure(new IllegalArgumentException(s"Line ${next.line}: Expected \"]\", but found \"${next.text}\""))
		}
	}

	def readDeclaration(stream: TokStream): Try[Declaration] = {
		var typename: String = ""
		var identifier: String = ""

		// all declarations start with TYPE ID PUNCTUATION, so we attempt to match those first

		// match TYPE
		stream.peek match {
			case Some(tok) =>
				if (tok.tok != TokenType.TYPE) {
					Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected TYPE, but found ${tok.tok}"))
				}
				typename = tok.text
			case None => Failure(new IllegalArgumentException(s"Expected TYPE, but stream is empty"))
		}
		stream.extract
		// match ID
		stream.peek match {
			case Some(tok) =>
				if (tok.tok != TokenType.IDENTIFIER) {
					Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected IDENTIFIER, but found ${tok.tok}"))
				}
				identifier = tok.text
			case None => Failure(new IllegalArgumentException("Expected IDENTIFIER, but stream is empty"))
		}
		stream.extract
		// match PUNCTUATION
		stream.peek match {
			case Some(tok) =>
				if (tok.tok != TokenType.PUNCTUATION) {
					Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected \"(, [, or ;\" but found ${tok.tok}"))
				}
			case None => Failure(new IllegalArgumentException(s"Expected (, [, or ; but stream is empty"))
		}
		// get the punctuation
		val next = stream.extract
		next.text match {
			case ";" => Success(VarDeclNode(typename, identifier))
			case "[" => readVarDeclArray(typename, identifier, stream)
			case "(" => readFunDecl(typename, identifier, stream)
			case _ => Failure(new IllegalArgumentException(s"Line ${next.line}: Expected \"(, [, or ;\" but found \"${next.text}\""))
		}
	}

	def apply(tok: Seq[Token]): ProgramNode = {
		val stream = new TokStream(tok)
		val decls = new ArrayBuffer[Declaration]

		while (stream.nonEmpty) {
			readDeclaration(stream) match {
				case Success(decl) => decls += decl
				case Failure(e) =>
			}
		}
	}
}

import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success, Try}

class ParseTree //TODO

/**
  * CFG for this language:
  *
  */

sealed trait ASTNode
case class   ProgramNode(declarations: Seq[Declaration])                                                             extends ASTNode
sealed trait Declaration
case class   VarDeclNode(typename: String, identifier: String, arrayLen: Option[Int] = None)                         extends ASTNode with Declaration
case class   FunDeclNode(returnType: String, identifier: String, params: Seq[ParamNode], body: Seq[Statement])       extends ASTNode with Declaration
case class   ParamNode(typename: String, identifier: String)                                                         extends ASTNode
sealed trait Statement
case class   SelectionStatementNode(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends ASTNode with Statement
case class   IterationStatementNode(condition: Expression, statement: Statement)                                     extends ASTNode with Statement
case class   ReturnStatementNode(expression: Option[Expression])                                                     extends ASTNode with Statement
case class   ExpressionStatementNode(expression: Option[Expression])                                                 extends ASTNode with Statement
sealed trait Expression
case class   AssignmentExpressionNode(identifier: String, right: Expression)                                         extends ASTNode with Expression
case class   SimpleExpressionNode(expression: Either[RelopExpressionNode, AdditiveExpressionNode])                   extends ASTNode with Expression
case class   RelopExpressionNode(left: AdditiveExpressionNode, right: Option[(String, AdditiveExpressionNode)])      extends ASTNode with Expression
case class   AdditiveExpressionNode(left: Option[(AdditiveExpressionNode, String)], right: TermNode)                 extends ASTNode with Expression
case class   TermNode(left: Option[(TermNode, String)], right: FactorNode)                                           extends ASTNode with Expression
case class   CallNode(identifier: String, args: Expression)                                                          extends ASTNode with Expression with Factor
sealed trait Factor
case class   FactorNode(factor: Expression)                                                                          extends ASTNode with Expression with Factor
case class   NumberNode(num: String)                                                                                 extends ASTNode with Expression with Factor

/**
  * A stream of tokens that can be advanced.
  */
trait TokStream {
	/**
	  * Gets the next element in the stream without advancing it.
	  * Prefer peekOption() to this method.
	  * @return The next Token in the stream.
	  * @throws IllegalStateException The stream is empty.
	  */
	def peek: Token

	/**
	  * Gets the next element in the stream if it exists without advancing it.
	  * @return Some(Token) if there's an element in the stream, None if not.
	  */
	def peekOption: Option[Token]

	/**
	  * Extracts the next element in the stream.
	  * Prefer extractIf() to this method.
	  *
	  * @return The next token in the stream.
	  * @throws IllegalStateException The stream is empty.
	  */
	def extract: Token

	/**
	  * Extracts the next element if the given condition is true.
	  *
	  * @param cond
	  *             True  - Extract the token and advance the stream.
	  *             False - Leave the stream as-is.
	  * @return Success(Token)                    - The condition was matched.
	  *         Failure(IllegalArgumentException) - The condition was not matched.
	  *         Failure(IllegalStateException)    - The stream is empty.
	  */
	def extractIf(typ: TokenType.Value): Try[Token]

	/**
	  * Checks if the stream has an element in it.
	  * @return True if there's at least one element in the stream, false if not.
	  */
	def nonEmpty: Boolean

	/**
	  * Checks if the stream is empty.
	  * @return True if the stream is empty, false if not.
	  */
	def empty: Boolean

	/**
	  * Advances the stream until the line number in the Tokens increases.
	  */
	def skipLine(): Unit
}

class SeqTokStream(private val tok: Seq[Token]) extends TokStream {
	private val str: ListBuffer[Token] = tok.to[ListBuffer]
	override def peek: Token = str.head

	override def peekOption: Option[Token] = str.headOption

	override def extract: Token = {
		if (this.empty) {
			throw new IllegalStateException("This TokStream is empty")
		}
		val ret = str.head
		str.remove(0)
		ret
	}

	override def extractIf(): Try[Token] = {
		this.peekOption match {
			case Some(token) if cond(token) => Success(this.extract)
			case Some(token) => Failure()
			case _ => None
		}
	}

	override def nonEmpty: Boolean = str.nonEmpty

	override def empty: Boolean = str.isEmpty

	override def skipLine(): Unit = {
		if (this.empty) return
		val line = str.head.line
		while (this.nonEmpty && str.head.line == line) {
			this.extract
		}
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

	def getToken(stream: TokStream, token: Option[Token], expected: TokenType.Value): Try[Token] = {
		token match {
			case Some(tok) if tok.tok == expected => Success(tok)
			case Some(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected $expected but found ${tok.text}"))
			case None =>
				if (stream.empty) Failure(new IllegalArgumentException(s"Expected $expected but end of stream was reached"))
				stream.extractIf(_.tok == expected) match {
					case Some(tok) => Success(tok)
					case None =>
				}
		}
	}

	def readCompoundStmt(stream: TokStream): Try[Seq[Statement]] = {
		Failure(new IllegalArgumentException("not implemented yet"))
	}

	def readParam(stream: TokStream): Try[ParamNode] = {
		// need to read TYPE ID ; | TYPE ID [ ] ;
		var typename = ""
		var identifier = ""
		// TYPE
		stream.peek match {
			case Some(tok) =>
				tok.tok match {
					case TokenType.TYPE => typename = tok.text
					case _ 	=> Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected TYPE but found ${tok.tok}"))
				}
			case None => Failure(new IllegalArgumentException(s"Expected TYPE but stream is empty"))
		}
		stream.extract

		stream.peek match {
			case Some(tok) =>
				tok.tok match {
					case TokenType.
				}
		}
		ParamNode("", "")
	}

	def readFunDecl(stream: TokStream, typename: String = "", identifier: String = ""): Try[FunDeclNode] = {
		// remaining tokens to match are params ) compound-stmt

		Failure(new IllegalArgumentException("not implemented yet"))
	}

	def readVarDeclArray(stream: TokStream, typename: String = "", identifier: String = ""): Try[VarDeclNode] = {
		// remaining tokens to match are NUM ]

		var num = 0
		// match NUM
		stream.peek match {
			case Some(tok) =>
				tok.tok match {
					case TokenType.INT => try { num = tok.text.toInt } catch { case e: NumberFormatException => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected INTEGER, but found \'${tok.text}\'"))}
					case _ => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected INT, but found ${tok.tok}"))
				}
			case None => Failure(new IllegalArgumentException(s"Expected NUMBER, but stream is empty"))
		}
		stream.extract

		// check if next token is PUNCTUATION
		stream.peek match {
			case Some(tok) =>
				tok.tok match {
					case TokenType.PUNCTUATION => ;
					case _ => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected \']\' but found ${tok.tok}"))
				}
			case None => Failure(new IllegalArgumentException(s"Expected \']\', but stream is empty"))
		}

		val next = stream.extract
		next.text match {
			case "]" => Success(VarDeclNode(typename, identifier, Some(num)))
			case _ => Failure(new IllegalArgumentException(s"Line ${next.line}: Expected \']\', but found \'${next.text}\'"))
		}
	}

	def readDeclaration(stream: TokStream, typeToken: Option[Token] = None, idToken: Option[Token] = None): Try[Declaration] = {
		// all declarations start with TYPE ID PUNCTUATION, so we attempt to match those first

		// match TYPE
		if (stream.empty) Failure(new IllegalArgumentException("Expected TYPE but stream is empty"))
		var typename = ""
		typeToken match {
			case Some(tok) =>
				tok.tok match {
					case TokenType.TYPE => typename = tok.text
					case _ => Failure(new IllegalArgumentException("typeToken parameter must have TYPE type"))
				}
			case None =>
				stream.extractIf(_.tok == TokenType.TYPE) match {
					case Some(tok) => typename = tok.text
					case _ => Failure(new IllegalArgumentException("Expected TYPE"))
				}
		}
		typename match {
			case Some(_) =>
			case None => Failure(new IllegalArgumentException("Expected TYPE"))
		}
		if (stream.empty) Failure(new IllegalArgumentException("Expected IDENTIFIER but stream is empty"))
		val identifier = stream.extractIf(_.tok == TokenType.IDENTIFIER)
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
					Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected \'(, [, or ;\' but found ${tok.tok}"))
				}
				tok.text match {
					case ";" => stream.extract; Success(VarDeclNode(typename, identifier))
					case "[" => stream.extract; readVarDeclArray(typename, identifier, stream)
					case "(" => stream.extract; readFunDecl(typename, identifier, stream)
					case _ => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected \'(, [, or ;\' but found \'${tok.text}\'"))
				}
			case None => Failure(new IllegalArgumentException(s"Expected (, [, or ; but stream is empty"))
		}
	}

	def apply(tok: Seq[Token]): ProgramNode = {
		val stream: TokStream = new SeqTokStream(tok)
		val decls = new ArrayBuffer[Declaration]

		while (stream.nonEmpty) {
			readDeclaration(stream) match {
				case Success(decl) => decls += decl
				case Failure(e) =>
			}
		}

		ProgramNode(Seq()) //TODO
	}
}

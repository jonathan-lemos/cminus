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
	  * Extracts the next element if it is of the given TokenType
	  *
	  * @param typ The type of token to extract.
	  * @return Left(Token) if the token was matched.
	  *         Right(String) if not. This string will contain a string representation of the error produced.
	  */
	def extractType(typ: TokenType.Value): Either[Token, String]

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

	override def extractType(typ: TokenType.Value): Either[Token, String] = {
		this.peekOption match {
			case Some(token) if typ == token.tok => Left(this.extract)
			case Some(token) => Right(s"Line ${token.line}: Expected $typ but found ${token.text}")
			case _ => Right(s"Expected $typ but reached end of stream")
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

	def getTokenType(stream: TokStream, token: Option[Token], expected: TokenType.Value): Try[Token] = {
		token match {
			case Some(tok) if tok.tok == expected => Success(tok)
			case Some(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected $expected but found ${tok.text}"))
			case None =>
				stream.extractType(expected) match {
					case Left(tok) => Success(tok)
					case Right(str) => Failure(new IllegalArgumentException(str))
				}
		}
	}

	def getTokens(stream: TokStream, tokens: Seq[(Option[Token], TokenType.Value)]): Try[Array[Token]] = {
		val ret: ArrayBuffer[Token] = new ArrayBuffer
		for ((tok, expected) <- tokens) {
			getTokenType(stream, tok, expected) match {
				case Success(t) => ret += t
				case Failure(e) => return Failure(e)
			}
		}
		Success(ret)
	}

	def readDeclaration(stream: TokStream, typeToken: Option[Token] = None, idToken: Option[Token] = None): Try[Declaration] = {
		// all declarations start with TYPE ID PUNCTUATION, so we attempt to match those first

		var tokens: Array[Token] = null
		getTokens(stream, Seq((typeToken, TokenType.TYPE), (idToken, TokenType.IDENTIFIER), (None, TokenType.PUNCTUATION))) match {
			case Success(seq) => tokens = seq
			case Failure(e) => return Failure(e)
		}

		tokens(2).text match {
			case ";" => Success(VarDeclNode(tokens(0).text, tokens(1).text))
			case "[" => readVarDeclArray(stream)
			case "(" => readFunDecl(stream)
			case _ => Failure(new IllegalArgumentException(s"Line ${tokens(2).line}: Expected ';,[,(' but found ${tokens(2).text}"))
		}
	}

	def readVarDeclArray(stream: TokStream, typeToken: Option[Token] = None, idToken: Option[Token] = None, puncToken: Option[Token] = None): Try[VarDeclNode] = {
		// TYPE ID [ NUM ]

		var tokens: Array[Token] = null
		getTokens(stream, Seq((typeToken, TokenType.TYPE), (idToken, TokenType.IDENTIFIER), (puncToken, TokenType.PUNCTUATION), (None, TokenType.INT), (None, TokenType.PUNCTUATION), (None, TokenType.PUNCTUATION))) match {
			case Success(seq) => tokens = seq
			case Failure(e) => return Failure(e)
		}

		if (tokens(2).text != "[") return Failure(new IllegalArgumentException(s"Line ${tokens(2).line}: Expected '[' but found ${tokens(2).text}"))
		if (tokens(4).text != "]") return Failure(new IllegalArgumentException(s"Line ${tokens(4).line}: Expected ']' but found ${tokens(4).text}"))
		if (tokens(5).text != ";") return Failure(new IllegalArgumentException(s"Line ${tokens(5).line}: Expected ';' but found ${tokens(5).text}"))
		Success(VarDeclNode(tokens(0).text, tokens(1).text, Some(tokens(3).text.toInt)))
	}

	def readFunDecl(stream: TokStream, typename: String = "", identifier: String = ""): Try[FunDeclNode] = {
		// TYPE ID ( param... )

		Failure(new IllegalArgumentException("not implemented yet"))
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

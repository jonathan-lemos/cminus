import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

class ParseTree //TODO

/**
  * CFG for this language:
  *
  */

sealed trait ASTNode
case class   ProgramNode(declarations: Seq[Declaration])                                                                    extends ASTNode
sealed trait Declaration                                                                                                    extends ASTNode
case class   VarDeclNode(typename: String, identifier: String, arrayLen: Option[Int] = None)                                extends Declaration
case class   FunDeclNode(returnType: String, identifier: String, params: Seq[ParamNode], body: CompoundStatementNode)       extends Declaration
case class   ParamNode(typename: String, identifier: String, array: Boolean = false)                                        extends ASTNode
sealed trait Statement                                                                                                      extends ASTNode
case class   CompoundStatementNode(statements: Seq[Statement])                                                              extends Statement
case class   SelectionStatementNode(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement] = None) extends Statement
case class   IterationStatementNode(condition: Expression, statement: Statement)                                            extends Statement
case class   ReturnStatementNode(expression: Option[Expression] = None)                                                     extends Statement
case class   ExpressionStatementNode(expression: Option[Expression] = None)                                                 extends Statement
sealed trait Factor                                                                                                         extends ASTNode
sealed trait Expression                                                                                                     extends Factor
case class   AssignmentExpressionNode(identifier: String, right: Expression)                                                extends Expression
case class   SimpleExpressionNode(expression: Either[RelopExpressionNode, AdditiveExpressionNode])                          extends Expression
case class   RelopExpressionNode(left: AdditiveExpressionNode, right: Option[(String, AdditiveExpressionNode)])             extends Expression
case class   AdditiveExpressionNode(left: Option[(AdditiveExpressionNode, String)], right: TermNode)                        extends Expression
case class   TermNode(left: Option[(TermNode, String)], right: FactorNode)                                                  extends Expression
case class   CallNode(identifier: String, args: Expression)                                                                 extends Factor
case class   FactorNode(factor: Expression)                                                                                 extends Factor
case class   VarNode(identifier: String, arrayLen: Option[Int] = None)                                                      extends Factor

class ParseException(s: String) extends IllegalArgumentException(s)

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
	  * Extracts a token if the given condition is true.
	  * The stream is not advanced if it's false.
	  *
	  * @param condition The condition to verify
	  */
	def extractIf(condition: Token => Boolean): Try[Token]

	/**
	  * Extracts tokens if the given lambdas are true.
	  * Unless all lambdas evaluate to true, the stream is not advanced.
	  *
	  *
	  */
	def extractIf(cond: Token => Boolean, vargs: (Token => Boolean)*): Try[Seq[Token]]

	/**
	  * Returns tokens to the stream.
	  */
	def barf(s: Seq[Token]): Unit

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

	override def extractIf(cond: Token => Boolean): Try[Token] = {
		if (this.empty) Failure(new ParseException("Stream is empty"))
		if (cond(str.head)) Success(this.extract) else Failure(new ParseException("Condition was not matched"))
	}

	override def extractIf(cond: Token => Boolean, vargs: (Token => Boolean)*): Try[Seq[Token]] = {
		val arr = ArrayBuffer[Token => Boolean](cond)
		arr ++= vargs
		if (this.str.length < arr.length) Failure(new ParseException(s"Expected ${arr.length} elements but stream only has ${str.length} elements"))
		for ((c, i) <- arr.zipWithIndex) {
			if (!c(this.str(i))) Failure(new ParseException(s"Condition $i failed"))
		}
		val buf = new ArrayBuffer[Token]
		arr.indices.foreach(buf += this.extract)
		Success(buf)
	}

	override def barf(c: Seq[Token]): Unit = {
		str.insertAll(0, c)
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

	def readDeclaration(stream: TokStream): Try[(Declaration, Seq[Token])] = {
		// var-declaration | fun-declaration
		readVarDecl(stream).orElse(readFunDecl(stream))
	}

	def readVarDecl(stream: TokStream): Try[(VarDeclNode, Seq[Token])] = {
		// TYPE ID ;|TYPE ID [ INT ] ;
		stream.extractIf(
			_.tok == TokenType.TYPE,
			_.tok == TokenType.IDENTIFIER,
			t => t.tok == TokenType.PUNCTUATION && t.text == ";",
		).orElse(stream.extractIf(
			_.tok == TokenType.TYPE,
			_.tok == TokenType.IDENTIFIER,
			t => t.tok == TokenType.PUNCTUATION && t.text == "[",
			_.tok == TokenType.INT,
			t => t.tok == TokenType.PUNCTUATION && t.text == "]",
			t => t.tok == TokenType.PUNCTUATION && t.text == ";",
		)) match {
			case Success(seq) if seq.length == 3 => Success(VarDeclNode(seq.head.text, seq(1).text), seq)
			case Success(seq) => Success(VarDeclNode(seq.head.text, seq(1).text, Some(seq(4).text.toInt)), seq)
		}
	}

	def readFunDecl(stream: TokStream, typeToken: Option[Token] = None, idToken: Option[Token] = None, pncToken: Option[Token] = None): Try[(FunDeclNode, Seq[Token])] = {
		// TYPE ID ( param... ) COMPOUND-STATEMENT
		val eaten = new ArrayBuffer[Token]
		val params = new ArrayBuffer[ParamNode]
		stream.extractIf(
			_.tok == TokenType.TYPE,
			_.tok == TokenType.IDENTIFIER,
			t => t.tok == TokenType.PUNCTUATION && t.text == "(",
		) match {
			case Success(seq) => eaten ++= seq
			case Failure(e) => return Failure(e)
		}

		while (true) {
			readParam(stream) match {
				case Success((par, tokseq)) => params += par; eaten ++= tokseq
				case Failure(_) => break;
			}
			stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == ",") match {
				case Success(tok) => eaten += tok
				case Failure(_) => break;
			}
		}

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == ")") match {
			case Success(tok) => eaten += tok
			case Failure(e) => stream.barf(eaten); return Failure(e)
		}

		readCompoundStatement(stream) match {
			case Success(seq) => Success(FunDeclNode())
		}
	}

	def readParam(stream: TokStream): Try[(ParamNode, Seq[Token])] = {
		// TYPE ID | TYPE ID []
		stream.extractIf(
			_.tok == TokenType.TYPE,
			_.tok == TokenType.IDENTIFIER,
			t => t.tok == TokenType.PUNCTUATION && t.text == "[",
			t => t.tok == TokenType.PUNCTUATION && t.text == "]",
		).orElse(stream.extractIf(
			_.tok == TokenType.TYPE,
			_.tok == TokenType.IDENTIFIER
		)) match {
			case Success(seq) => Success((ParamNode(seq.head.text, seq(1).text, seq.length == 4), seq))
			case Failure(e) => Failure(e)
		}
	}

	def readCompoundStatement(stream: TokStream): Try[(CompoundStatementNode, Seq[Token])] = {
		// { var-declaration... statement... }
		val tokens = new ArrayBuffer[Token]
		val vardecls = new ArrayBuffer[VarDeclNode]
		val statements = new ArrayBuffer[Statement]

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == "{") match {
			case Success(tok) => tokens += tok
			case Failure(e) => return Failure(e)
		}

		while (true) {
			readVarDecl(stream) match {
				case Success((vdc, seq)) => vardecls += vdc; tokens ++= seq
				case Failure(_) => break;
			}
		}

		while (true) {
			readStatement(stream) match {
				case Success((stmt, seq)) => statements += stmt; tokens ++= seq
				case Failure(_) => break;
			}
		}

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == "}") match {
			case Success(tok) => tokens += tok; Success((CompoundStatementNode(statements), tokens))
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readStatement(stream: TokStream): Try[(Statement, Seq[Token])] = {
		// expression-stmt|compound-stmt|selection-stmt|iteration-stmt|return-stmt
	}

	def readExpressionStatement(stream: TokStream): Try[(ExpressionStatementNode, Seq[Token])] = {
		var expr: Expression = null
		val tokens = new ArrayBuffer[Token]

		readExpression(stream) match {
			case Success((e, seq)) => expr = e; tokens ++= seq
			case Failure(e) => return Failure(e)
		}

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == ";") match {
			case Success()
		}
	}

	def readSelectionStatement(stream: TokStream, keywordToken: Option[Token]): Try[(SelectionStatementNode, Seq[Token])] = {
		getTokenType(stream, keywordToken, TokenType.KEYWORD) match {
			case Success(tok) if tok.text == "if" =>
			case Success(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected 'if' but found ${tok.text}"))
			case Failure(e) => return Failure(e)
		}

		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == "(" => stream.extract
			case Some(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected '(' but found ${tok.text}"))
			case None => Failure(new IllegalArgumentException("Expected '(' but found end of stream"))
		}

		var expression: Expression = null
		readExpression(stream) match {
			case Success(expr) => expression = expr
			case Failure(e) => return Failure(e)
		}

		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == ")" => stream.extract
			case Some(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected ')' but found ${tok.text}"))
			case None => Failure(new IllegalArgumentException("Expected ')' but reached and of stream"))
		}

		var statement: Statement = null
		readStatement(stream) match {
			case Success(stmt) => statement = stmt
			case Failure(e) => return Failure(e)
		}

		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.KEYWORD && tok.text == "else" => stream.extract
			case None => Success(SelectionStatementNode(expression, statement))
		}

		readStatement(stream) match {
			case Success(stmt) => Success(SelectionStatementNode(expression, statement, Some(stmt)))
			case Failure(e) => Failure(e)
		}
	}

	def readIterationStatement(stream: TokStream, keywordToken: Option[Token]): Try[(IterationStatementNode, Seq[Token])] = {
		getTokenType(stream, keywordToken, TokenType.KEYWORD) match {
			case Success(tok) if tok.text == "while" =>
			case Success(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected 'while' but found ${tok.text}"))
			case Failure(e) => return Failure(e)
		}

		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == "(" => stream.extract
			case Some(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected '(' but found ${tok.text}"))
			case None => Failure(new IllegalArgumentException("Expected '(' but found end of stream"))
		}

		var expression: Expression = null
		readExpression(stream) match {
			case Success(expr) => expression = expr
			case Failure(e) => return Failure(e)
		}

		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == ")" => stream.extract
			case Some(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected ')' but found ${tok.text}"))
			case None => Failure(new IllegalArgumentException("Expected ')' but reached and of stream"))
		}

		readStatement(stream) match {
			case Success(stmt) => Success(IterationStatementNode(expression, stmt))
			case Failure(e) => Failure(e)
		}
	}

	def readReturnStatement(stream: TokStream, keywordToken: Option[Token]): Try[(ReturnStatementNode, Seq[Token])] = {
		getTokenType(stream, keywordToken, TokenType.KEYWORD) match {
			case Success(tok) if tok.text == "return" =>
			case Success(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected 'return' but found ${tok.text}"))
			case Failure(e) => return Failure(e)
		}

		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == ";" => stream.extract; Success(ReturnStatementNode())
			case _ =>
		}

		var expression: Expression = null
		readExpression(stream) match {
			case Success(expr) => expression = expr
			case Failure(e) => return Failure(e)
		}

		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == ";" => stream.extract; Success(ReturnStatementNode(Some(expression)))
			case Some(tok) => Failure(new IllegalArgumentException(s"Line ${tok.line}: Expected ';' but found ${tok.text}"))
			case None => Failure(new IllegalArgumentException("Expected ';' but reached end of stream"))
		}
	}

	def readExpression(stream: TokStream): Try[(Expression, Seq[Token])]

	def readFactor(stream: TokStream): Try[FactorNode] = {
		stream.peekOption match {
			case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == "(" =>
				var expression: Expression = null
				readExpression(stream) match {
					case Success(expr) =>
						expression = expr
						stream.peekOption match {
							case Some(tok) if tok.tok == TokenType.PUNCTUATION && tok.text == "(" =>

						}
					case Failure(e) => return Failure(e)
				}
			case Some(tok) if tok.tok == TokenType.IDENTIFIER =>
				stream.extract
				stream.peekOption match {
					case Some(tok) if tok.tok == TokenType.PUNCTUATION =>

				}
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

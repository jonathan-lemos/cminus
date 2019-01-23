import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

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
case class   SimpleExpressionNode(left: AdditiveExpressionNode, right: Option[(String, AdditiveExpressionNode)] = None)     extends Expression
case class   AdditiveExpressionNode(left: TermNode, right: Option[(String, AdditiveExpressionNode)] = None)                 extends Expression
case class   TermNode(left: Factor, right: Option[(String, TermNode)] = None)                                               extends Expression
case class   CallNode(identifier: String, args: Seq[Expression])                                                            extends Factor
case class   VarNode(identifier: String, arrayLen: Option[Int] = None)                                                      extends Factor
case class   IntNode(value: Int)                                                                                            extends Factor
case class   FloatNode(value: Int)                                                                                          extends Factor

class ParseException(s: String) extends IllegalArgumentException(s)

/**
  * A stream of tokens that can be advanced.
  */
trait TokStream {
	/**
	  * Gets the next element in the stream without advancing it.
	  * Prefer peekOption() to this method.
	  * @return The next Token in the stream.
	  * @throws ParseException The stream is empty.
	  */
	def peek: Token

	/**
	  * Gets the next element in the stream if it exists without advancing it.
	  * @return Some(Token) if there's an element in the stream, None if not.
	  */
	def peekOption: Option[Token]

	/**
	  * Extracts the next token in the stream.
	  * Prefer extractIf() to this method.
	  *
	  * @return The next token in the stream.
	  * @throws ParseException The stream is empty.
	  */
	def extract: Token

	/**
	  * Extracts a sequence of ASTNodes if the given conditions are true.
	  * The stream is not advanced if it is false.
	  *
	  * @param conds The condition lambdas to apply.
	  *              These can either take a token and return a boolean, or take this stream and return an optional ASTNode
	  */
	def extractIf(conds: (Either[Token => Boolean, TokStream => Try[ASTNode]], Boolean)*): Try[(Seq[Token], Seq[ASTNode])]

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
	private var preserve: Boolean = false
	private var preserveIndex: Int = 0

	override def peek: Token = preserve match {
		case true if preserveIndex < str.length => str(preserveIndex)
		case false if str.nonEmpty => str.head
		case _ => throw new ParseException("Stream is empty")
	}

	override def peekOption: Option[Token] = preserve match {
		case true if preserveIndex < str.length => Some(str(preserveIndex))
		case false if str.nonEmpty => Some(str.head)
		case _ => None
	}

	override def extract: Token = preserve match {
		case true if preserveIndex < str.length => val ret = this.peek; preserveIndex += 1; ret
		case false if str.nonEmpty => val ret = this.peek; str.remove(0); ret
		case _ => throw new ParseException("Stream is empty")
	}

	private def startPreserve(): Unit = {
		this.preserve = true
	}

	private def finishPreserve(): Seq[Token] = {
		val ret = this.str.slice(0, preserveIndex)
		this.preserve = false
		this.preserveIndex = 0
		this.str.remove(0, preserveIndex)
		ret
	}

	override def extractIf(conds: (Either[Token => Boolean, TokStream => Try[ASTNode]], Boolean)*): Try[(Seq[Token], Seq[ASTNode])] = {
		val tokens = new ArrayBuffer[Token]
		val rettok = new ArrayBuffer[Token]
		val ret = new ArrayBuffer[ASTNode]
		for (_ <- conds) {
			case _ if this.empty =>
				this.barf(tokens)
				Failure(new ParseException("End of stream"))
			case (f: (Token => Boolean), vararg: Boolean) =>
				def m: Try[Unit] = {
					if (f(this.peek)) {
						val t = this.extract
						tokens += t
						rettok += t
						Success()
					}
					else {
						Failure(new ParseException(this.peek.text))
					}
				}
				if (vararg) while (m.isSuccess) {} else m match {
					case Success(_) =>
					case Failure(e) => this.barf(tokens); Failure(e)
				}
			case (f: (TokStream => Try[ASTNode]), vararg: Boolean) =>
				def m: Try[Unit] = {
					this.startPreserve()
					val res = f(this)
					tokens ++= this.finishPreserve()
					res match {
						case Success(node) => ret += node; Success()
						case Failure(e) => Failure(e)
					}
				}
				if (vararg) while (m.isSuccess) {} else m match {
					case Success(_) =>
					case Failure(e) => this.barf(tokens); Failure(e)
				}
		}
		Success((rettok, ret))
	}

	private def barf(c: Seq[Token]): Unit = {
		str.insertAll(0, c)
	}

	override def nonEmpty: Boolean = !this.empty

	override def empty: Boolean = if (preserve) this.preserveIndex < this.str.length else this.str.isEmpty

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

	def readDeclaration(stream: TokStream): Try[Declaration] = {
		// var-declaration | fun-declaration
		readVarDecl(stream) orElse readFunDecl(stream)
	}

	def readVarDecl(stream: TokStream): Try[VarDeclNode] = {
		// TYPE ID ;|TYPE ID [ INT ] ;

		stream.extractIf(
			// TYPE ID ;
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) orElse stream.extractIf(
			// TYPE ID [ INT ]
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "["), false),
			(Left(_.tok == TokenType.INT), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "]"), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) match {
			case Success((seq, _)) if seq.length == 3 => Success(VarDeclNode(seq.head.text, seq(1).text))
			case Success((seq, _)) => Success(VarDeclNode(seq.head.text, seq(1).text, Some(seq(4).text.toInt)))
			case Failure(e) => Failure(new ParseException(s"Expected var-decl but found illegal token ${e.getMessage}"))
		}
	}

	def readFunDecl(stream: TokStream, typeToken: Option[Token] = None, idToken: Option[Token] = None, pncToken: Option[Token] = None): Try[FunDeclNode] = {
		// TYPE ID ( param... ) COMPOUND-STATEMENT
		stream.extractIf(
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Right(readParamComma), true),
			(Right(readParam), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
			(Right(readCompoundStatement), false),
		) orElse stream.extractIf(
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
			(Right(readCompoundStatement), false),
		) match {
			case Success((tseq, aseq)) => Success(FunDeclNode(tseq.head.text, tseq(1).text, aseq.slice(0, aseq.length - 1).asInstanceOf[Seq[ParamNode]], aseq.last.asInstanceOf[CompoundStatementNode]))
			case Failure(e) => Failure(e)
		}
	}

	def readParamComma(stream: TokStream): Try[ParamNode] = {
		// param ,
		stream.extractIf(
			(Right(readParam), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ","), false),
		) match {
			case Success((_, aseq)) => Success(aseq.head.asInstanceOf[ParamNode])
			case Failure(e) => Failure(e)
		}
	}

	def readParam(stream: TokStream): Try[ParamNode] = {
		// TYPE ID | TYPE ID []
		stream.extractIf(
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "["), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "]"), false),
		) orElse stream.extractIf(
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
		) match {
			case Success((seq, _)) => Success(ParamNode(seq.head.text, seq(1).text, seq.length == 4))
			case Failure(e) => Failure(e)
		}
	}

	def readCompoundStatement(stream: TokStream): Try[CompoundStatementNode] = {
		// { var-declaration... statement... }
		stream.extractIf(
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "{"), false),
			(Right(readVarDecl), true),
			(Right(readStatement), true),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "}"), false),
		) match {
			case Success((_, seq)) => Success(CompoundStatementNode())
		}
	}

	def readStatement(stream: TokStream): Try[Statement] = {
		// expression-stmt|compound-stmt|selection-stmt|iteration-stmt|return-stmt
	}

	def readExpressionStatement(stream: TokStream): Try[(ExpressionStatementNode, Seq[Token])] = {
		// expression ;|;

	}

	def readSelectionStatement(stream: TokStream, keywordToken: Option[Token]): Try[(SelectionStatementNode, Seq[Token])] = {
		// if ( expression ) statement else statement
		val tokens = new ArrayBuffer[Token]

		stream.extractIf(
			t => t.tok == TokenType.KEYWORD && t.text == "if",
			t => t.tok == TokenType.PUNCTUATION && t.text == "(",
		) match {
			case Success(seq) => tokens ++= seq
			case Failure(e) => return Failure(e)
		}

		var expr: Expression = null
		readExpression(stream) match {
			case Success((exp, seq)) => tokens ++= seq; expr = exp
			case Failure(e) => stream.barf(tokens); return Failure(e)
		}

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == ")") match {
			case Success(tok) => tokens += tok
			case Failure(e) => stream.barf(tokens); return Failure(e)
		}

		var cps1: Statement = null
		readStatement(stream) match {
			case Success((cps, seq)) => tokens ++= seq; cps1 = cps
			case Failure(e) => stream.barf(tokens); return Failure(e)
		}

		stream.extractIf((t: Token) => t.tok == TokenType.KEYWORD && t.text == "else") match {
			case Success(tok) => tokens += tok
			case Failure(_) => Success((SelectionStatementNode(expr, cps1), tokens))
		}

		readStatement(stream) match {
			case Success((cps, seq)) => tokens ++= seq; Success((SelectionStatementNode(expr, cps1, Some(cps)), tokens))
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readIterationStatement(stream: TokStream, keywordToken: Option[Token]): Try[(IterationStatementNode, Seq[Token])] = {
		// while ( expression ) statement
		val tokens = new ArrayBuffer[Token]

		stream.extractIf(
			t => t.tok == TokenType.KEYWORD && t.text == "while",
			t => t.tok == TokenType.PUNCTUATION && t.text == "(",
		) match {
			case Success(seq) => tokens ++= seq
			case Failure(e) => return Failure(e)
		}

		var expr: Expression = null
		readExpression(stream) match {
			case Success((exp, seq)) => tokens ++= seq; expr = exp
			case Failure(e) => stream.barf(tokens); return Failure(e)
		}

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == ")") match {
			case Success(tok) => tokens += tok
			case Failure(e) => stream.barf(tokens); return Failure(e)
		}

		readCompoundStatement(stream) match {
			case Success((cps, seq)) => tokens ++= seq; Success(IterationStatementNode(expr, cps), tokens)
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readReturnStatement(stream: TokStream, keywordToken: Option[Token]): Try[(ReturnStatementNode, Seq[Token])] = {
		// return ;|return expression ;
		val tokens = new ArrayBuffer[Token]

		stream.extractIf((t: Token) => t.tok == TokenType.KEYWORD && t.text == "return") match {
			case Success(tok) => tokens += tok
			case Failure(e) => return Failure(e)
		}

		var expr: Option[Expression] = None
		readExpression(stream) match {
			case Success((ex, seq)) => tokens ++= seq; expr = Some(ex)
			case Failure(_) =>
		}

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == ";") match {
			case Success(tok) => tokens += tok; Success((ReturnStatementNode(expr), tokens))
			case Failure(e) => Failure(e)
		}
	}

	def readExpression(stream: TokStream): Try[(Expression, Seq[Token])] = {
		readAssignmentExpression(stream).orElse(readSimpleExpression(stream)) match {
			case Success((a: AssignmentExpressionNode, seq)) => Success((a, seq))
			case Success((s: SimpleExpressionNode, seq)) => Success((s, seq))
			case Failure(e) => Failure(e)
		}
	}

	def readAssignmentExpression(stream: TokStream): Try[(AssignmentExpressionNode, Seq[Token])] = {
		// var = expression
		val tokens = new ArrayBuffer[Token]

		stream.extractIf(
			_.tok == TokenType.IDENTIFIER,
			t => t.tok == TokenType.ASSGNOP && t.text == "=",
		) match {
			case Success(seq) => tokens ++= seq
			case Failure(e) => return Failure(e)
		}

		readExpression(stream) match {
			case Success((expr, seq)) => tokens ++= seq; Success((AssignmentExpressionNode(tokens.head.text, expr), tokens))
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readSimpleExpression(stream: TokStream): Try[(SimpleExpressionNode, Seq[Token])] = {
		// additive-expression|additive-expression RELOP additive-expression
		var tokens = new ArrayBuffer[Token]

		var expr: AdditiveExpressionNode = null
		readAdditiveExpression(stream) match {
			case Success((exp, seq)) => tokens ++= seq; expr = exp
			case Failure(e) => return Failure(e)
		}

		var relop: String = ""
		stream.extractIf((t: Token) => t.tok == TokenType.RELOP) match {
			case Success(tok) => tokens += tok; relop = tok.text
			case Failure(_) => Success(SimpleExpressionNode(expr), tokens)
		}

		readAdditiveExpression(stream) match {
			case Success((exp, seq)) => tokens ++= seq; Success((SimpleExpressionNode(expr, Some(relop, exp)), tokens))
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readAdditiveExpression(stream: TokStream): Try[(AdditiveExpressionNode, Seq[Token])] = {
		// term|term ADDOP additive-expression
		var tokens = new ArrayBuffer[Token]

		var expr: TermNode = null
		readTerm(stream) match {
			case Success((exp, seq)) => expr = exp; tokens ++= seq
			case Failure(e) => return Failure(e)
		}

		var addop: String = ""
		stream.extractIf((t: Token) => t.tok == TokenType.ADDOP) match {
			case Success(tok) => tokens += tok; addop = tok.text
			case Failure(_) => Success(AdditiveExpressionNode(expr), tokens)
		}

		readAdditiveExpression(stream) match {
			case Success((aex, seq)) => tokens ++= seq; Success(AdditiveExpressionNode(expr, Some((addop, aex))), tokens)
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readTerm(stream: TokStream): Try[(TermNode, Seq[Token])] = {
		// factor|factor MULOP term
		val tokens = new ArrayBuffer[Token]

		var fac: Factor = null
		readFactor(stream) match {
			case Success((f, seq)) => fac = f; tokens ++= seq
			case Failure(e) => return Failure(e)
		}

		var mulop: String = ""
		stream.extractIf((t: Token) => t.tok == TokenType.MULOP) match {
			case Success(tok) => tokens += tok; mulop = tok.text
			case Failure(_) => Success(TermNode(fac), tokens)
		}

		readTerm(stream) match {
			case Success((t, seq)) => tokens ++= seq; Success(TermNode(fac, Some((mulop, t))), tokens)
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readFactor(stream: TokStream): Try[(Factor, Seq[Token])] = {
		// ( expression )|var|call|int|float
		readVar(stream) orElse readCall(stream) orElse readInt(stream) orElse readFloat(stream) orElse readParenExpression(stream)
	}

	def readCall(stream: TokStream): Try[(CallNode, Seq[Token])] = {
		// ID ( expression... )
		val tokens = new ArrayBuffer[Token]
		var identifier: String = ""

		stream.extractIf((t: Token) => t.tok == TokenType.IDENTIFIER) match {
			case Success(tok) => tokens += tok; identifier = tok.text
			case Failure(e) => return Failure(e)
		}
		readParenExpression(stream) match {
			case Success((pe, seq)) => tokens ++= seq; Success(CallNode(identifier, pe), tokens)
			case Failure(e) => stream.barf(tokens); Failure(e)
		}
	}

	def readVar(stream: TokStream): Try[(VarNode, Seq[Token])] = {
		// ID|ID [ expression ]
		val tokens = new ArrayBuffer[Token]
		var identifier: String = ""

		stream.extractIf((t: Token) => t.tok == TokenType.PUNCTUATION && t.text == "")
	}

	def readInt(stream: TokStream): Try[(IntNode, Seq[Token])] = {
		// INT
	}

	def readFloat(stream: TokStream): Try[(FloatNode, Seq[Token])] = {
		// FLOAT
	}

	def readParenExpression(stream: TokStream): Try[(Expression, Seq[Token])] = {
		// ( expression )
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

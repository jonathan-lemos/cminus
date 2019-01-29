import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

trait ASTNode
case class   ProgramNode(declarations: Seq[Declaration])                                                                       extends ASTNode
sealed trait Declaration                                                                                                       extends ASTNode
case class   VarDeclNode(typename: String, identifier: String, arrayLen: Option[Int] = None, right: Option[Expression] = None) extends Declaration
case class   FunDeclNode(returnType: String, identifier: String, params: Seq[ParamNode], body: CompoundStatementNode)          extends Declaration
case class   ParamNode(typename: String, identifier: String, array: Boolean = false)                                           extends ASTNode
sealed trait Statement                                                                                                         extends ASTNode
case class   CompoundStatementNode(vardecls: Seq[VarDeclNode], statements: Seq[Statement])                                     extends Statement
case class   SelectionStatementNode(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement] = None)    extends Statement
case class   IterationStatementNode(condition: Expression, statement: Statement)                                               extends Statement
case class   ReturnStatementNode(expression: Option[Expression] = None)                                                        extends Statement
case class   ExpressionStatementNode(expression: Option[Expression] = None)                                                    extends Statement
sealed trait Expression                                                                                                        extends ASTNode
sealed trait Factor                                                                                                            extends Expression
case class   AssignmentExpressionNode(identifier: String, right: Expression)                                                   extends Expression
case class   SimpleExpressionNode(left: AdditiveExpressionNode, right: Option[(String, AdditiveExpressionNode)] = None)        extends Expression
case class   AdditiveExpressionNode(left: TermNode, right: Option[(String, AdditiveExpressionNode)] = None)                    extends Expression
case class   TermNode(left: Factor, right: Option[(String, TermNode)] = None)                                                  extends Expression
case class   CallNode(identifier: String, args: Seq[Expression])                                                               extends Factor
case class   VarNode(identifier: String, arrayLen: Option[Expression] = None)                                                  extends Factor
case class   NumNode(value: Either[Int, Double])                                                                               extends Factor
case class   ParenExpressionNode(expr: Expression)                                                                             extends Factor

class ParseException(s: String, e: Either[Option[Token], ParseException]) extends IllegalArgumentException(s)

sealed trait TokStreamMatch
final case class Match(v: Seq[(Either[Token => Boolean, TokStream => Try[ASTNode]], String)]) extends TokStreamMatch {
	def apply: Seq[(Either[Token => Boolean, TokStream => Try[ASTNode]], String)] = v
}
final case class Optional(v: Seq[(Either[Token => Boolean, TokStream => Try[ASTNode]], String)]) extends TokStreamMatch {
	def apply: Seq[(Either[Token => Boolean, TokStream => Try[ASTNode]], String)] = v
}
final case class Vararg(v: Seq[(Either[Token => Boolean, TokStream => Try[ASTNode]], String)]) extends TokStreamMatch {
	def apply: Seq[(Either[Token => Boolean, TokStream => Try[ASTNode]], String)] = v
}

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
	  *              The second member of a tuple denotes this as a "vararg", meaning it takes 0 or more.
	  * @return A sequence of the tokens/ASTNodes matched, or Failure()
	  */
	def extractIf(conds: TokStreamMatch*): Try[(Seq[Token], Seq[ASTNode])]

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
}

class SeqTokStream(private val tok: Seq[Token]) extends TokStream {
	private val str = tok.to[Array]
	private var index = 0

	override def peek: Token = str(index)

	override def peekOption: Option[Token] = if (index < str.length) Some(str(index)) else None

	override def extract: Token = {
		if (this.empty) throw new ParseException("Stream is empty", Left(None))
		this.index += 1
		str(index - 1)
	}

	private def __extractIf(conds: Seq[TokStreamMatch]): Try[(Seq[Token], Seq[ASTNode])] = {
		val ret = new ArrayBuffer[Either[Token, ASTNode]]

		def readExpr(expr: (Either[Token => Boolean, TokStream => Try[ASTNode]], String)): Try[Either[Token, ASTNode]] = {
			if (this.empty) return Failure(new ParseException("Stream is empty", Left(None)))
			expr._1 match {
				case Left(c) =>
					if (c(this.peek)) Success(Left(this.extract))
					else Failure(new ParseException(expr._2, Left(Some(this.peek))))
				case Right(c) => c(this) match {
					case Success(v) => Success(Right(v));
					case Failure(e: ParseException) => Failure(new ParseException(expr._2, Right(e)))
				}
			}
		}

		for (cond <- conds) {
			cond match {
				case Match(v) => for (q <- v) readExpr(q) match {
					case Success(w) => ret += w
					case Failure(e) => return Failure(e)
				}
				case Optional(v) =>
					var ctr = 0
					var ind = this.index
					breakable { for (q <- v) readExpr(q) match {
						case Success(w) => ret += w; ctr += 1; ind = this.index
						case Failure(_) => ret.remove(ret.length - ctr, ctr); this.index = ind; break
					}}
				case Vararg(v) =>
					def m(x: Seq[(Either[Token => Boolean, TokStream => Try[ASTNode]], String)]): Boolean = {
						val tmp = new ArrayBuffer[Either[Token, ASTNode]]
						val indexOld = this.index
						for (q <- x) readExpr(q) match {
							case Success(y) => tmp += y;
							case Failure(_) => this.index = indexOld; return false
						}
						ret ++= tmp
						true
					}
					while (m(v)) {}
			}
		}
		Success((ret.collect{case Left(t) => t}, ret.collect{case Right(a) => a}))
	}

	override def extractIf(conds: TokStreamMatch*): Try[(Seq[Token], Seq[ASTNode])] = {
		val indexSave = this.index
		val ret = __extractIf(conds)
		ret match {
			case Success(v) => Success(v)
			case Failure(e) => this.index = indexSave; Failure(e)
		}
	}

	override def nonEmpty: Boolean = !this.empty

	override def empty: Boolean = this.index >= this.str.length
}

object Parser {
	// var-declaration | fun-declaration
	def readDeclaration(stream: TokStream): Try[Declaration] = readVarDecl(stream) orElse readFunDecl(stream)

	// TYPE ID ;|TYPE ID [ INT ] ;
	def readVarDecl(stream: TokStream): Try[VarDeclNode] = {
		stream.extractIf(
			// TYPE ID < [INT] >? < = expression >?;
			Match(Seq((Left(_.tok == TokType.TYPE), "Expected type"), (Left(_.tok == TokType.IDENTIFIER), "Expected identifier"))),
			Optional(Seq((Left(_.tok == TokType.OBRACKET), "Expected \"[\""), (Left(_.tok == TokType.INT), "Expected int"), (Left(_.tok == TokType.CBRACKET), "Expected \"]\""))),
			Optional(Seq((Left(_.tok == TokType.ASSGNOP), "Expected \"=\""), (Right(readExpression), "Expected expression"))),
			Match(Seq((Left(_.tok == TokType.SEMICOLON), "Expected \";\""))),
		) match {
			case Success((tok, ast)) => tok.length match {
				case 4 => Success(VarDeclNode(tok.head.text, tok(1).text, None, Some(ast.head.asInstanceOf[Expression])))
				case 3 => Success(VarDeclNode(tok.head.text, tok(1).text))
				case 7 => Success(VarDeclNode(tok.head.text, tok(1).text, Some(tok(3).text.toInt), Some(ast.head.asInstanceOf[Expression])))
				case 6 => Success(VarDeclNode(tok.head.text, tok(1).text, Some(tok(3).text.toInt)))
			}
			case Failure(e) => Failure(e)
		}
	}

	def readFunDecl(stream: TokStream, typeToken: Option[Token] = None, idToken: Option[Token] = None, pncToken: Option[Token] = None): Try[FunDeclNode] = {
		// TYPE ID ( param... ) COMPOUND-STATEMENT
		stream.extractIf(
			Match(Seq((Left(_.tok == TokType.TYPE), "Expected type"), (Left(_.tok == TokType.IDENTIFIER), "Expected identifier"), (Left(_.tok == TokType.OPAREN), "Expected ("))),
			Vararg(Seq((Right(readParam), "Expected param"), (Left(_.tok == TokType.COMMA), "Expected \",\""))),
			Match(Seq((Right(readParam), "Expected param"), (Left(_.tok == TokType.CPAREN), "Expected \")\""), (Right(readCompoundStatement), "Expected compound-stmt"))),
		) orElse stream.extractIf(
			Match(Seq((Left(_.tok == TokType.TYPE), "Expected type"), (Left(_.tok == TokType.IDENTIFIER), "Expected identifier"), (Left(_.tok == TokType.OPAREN), "Expected \"(\""), (Left(t => t.tok == TokType.TYPE && t.text == "void"), "Expected \"void\""), (Left(_.tok == TokType.CPAREN), "Expected \")\""), (Right(readCompoundStatement), "Expected compound-stmt")))
		) match {
			case Success((tseq, aseq)) => Success(FunDeclNode(tseq.head.text, tseq(1).text, aseq.slice(0, aseq.length - 1).asInstanceOf[Seq[ParamNode]], aseq.last.asInstanceOf[CompoundStatementNode]))
			case Failure(e) => Failure(e)
		}
	}

	def readParam(stream: TokStream): Try[ParamNode] = {
		// TYPE ID | TYPE ID []
		stream.extractIf(
			Match(Seq((Left(_.tok == TokType.TYPE), "Expected type"), (Left(_.tok == TokType.IDENTIFIER), "Expected param"))),
			Optional(Seq((Left(_.tok == TokType.OBRACKET), "Expected \"{\""), (Left(_.tok == TokType.CBRACKET), "Expected \"}\""))),
		) match {
			case Success((seq, _)) => Success(ParamNode(seq.head.text, seq(1).text, seq.length == 4))
			case Failure(e) => Failure(e)
		}
	}

	def readCompoundStatement(stream: TokStream): Try[CompoundStatementNode] = {
		// { var-declaration... statement... }
		stream.extractIf(
			Match(Seq((Left(_.tok == TokType.OBRACE), "Expected \"{\""))),
			Vararg(Seq((Right(readVarDecl), "Expected var-decl"))),
			Vararg(Seq((Right(readStatement), "Expected statement"))),
			Match(Seq((Left(_.tok == TokType.CBRACE), "Expected \"}\""))),
		) match {
			case Success((_, seq)) => Success(CompoundStatementNode(seq.filter(_.isInstanceOf[VarDeclNode]).asInstanceOf[Seq[VarDeclNode]], seq.filter(_.isInstanceOf[Statement]).asInstanceOf[Seq[Statement]]))
			case Failure(e) => Failure(e)
		}
	}

	// expression-stmt|compound-stmt|selection-stmt|iteration-stmt|return-stmt
	def readStatement(stream: TokStream): Try[Statement] = readExpressionStatement(stream) orElse readCompoundStatement(stream) orElse readSelectionStatement(stream) orElse readIterationStatement(stream) orElse readReturnStatement(stream)

	def readExpressionStatement(stream: TokStream): Try[ExpressionStatementNode] = {
		// expression ;|;
		stream.extractIf(
			Optional(Seq((Right(readExpression), "Expected expression"))),
			Match(Seq((Left(_.tok == TokType.SEMICOLON), "Expected \";\""))),
		) match {
			case Success((_, seq)) if seq.length == 1 => Success(ExpressionStatementNode(Some(seq.head.asInstanceOf[Expression])))
			case Success((_, _)) => Success(ExpressionStatementNode())
			case Failure(e) => Failure(e)
		}
	}

	def readSelectionStatement(stream: TokStream): Try[SelectionStatementNode] = {
		// if ( expression ) statement else statement
		stream.extractIf(
			Match(Seq((Left(t => t.tok == TokType.KEYWORD && t.text == "if"), "Expected \"if\""), (Left(_.tok == TokType.OPAREN), "Expected \"(\""), (Right(readExpression), "Expected expression"), (Left(_.tok == TokType.CPAREN), "Expected \")\""), (Right(readStatement), "Expected statement"))),
			Optional(Seq((Left(t => t.tok == TokType.KEYWORD && t.text == "else"), "Expected \"else\""), (Right(readStatement), "Expected statement"))),
		) match {
			case Success((_, seq)) if seq.length == 3 => Success(SelectionStatementNode(seq.head.asInstanceOf[Expression], seq(1).asInstanceOf[Statement], Some(seq(2).asInstanceOf[Statement])))
			case Success((_, seq)) => Success(SelectionStatementNode(seq.head.asInstanceOf[Expression], seq(1).asInstanceOf[Statement]))
			case Failure(e) => Failure(e)
		}
	}

	def readIterationStatement(stream: TokStream): Try[IterationStatementNode] = {
		// while ( expression ) statement
		stream.extractIf(
			Match(Seq((Left(t => t.tok == TokType.KEYWORD && t.text == "while"), "Expected \"while\""), (Left(_.tok == TokType.OPAREN), "Expected \"(\""), (Right(readExpression), "Expected expression"), (Left(_.tok == TokType.CPAREN), "Expected \")\""), (Right(readStatement), "Expected statement")))
		) match {
			case Success((_, seq)) => Success(IterationStatementNode(seq.head.asInstanceOf[Expression], seq(1).asInstanceOf[Statement]))
			case Failure(e) => Failure(e)
		}
	}

	def readReturnStatement(stream: TokStream): Try[ReturnStatementNode] = {
		// return ;|return expression ;
		stream.extractIf(
			Match(Seq((Left(t => t.tok == TokType.KEYWORD && t.text == "return"), "Expected \"return\""))),
			Optional(Seq((Right(readExpression), "Expected expression"))),
			Match(Seq((Left(_.tok == TokType.SEMICOLON), "Expected \";\""))),
		) match {
			case Success((_, seq)) if seq.length == 1 => Success(ReturnStatementNode(Some(seq.head.asInstanceOf[Expression])))
			case Success((_, _)) => Success(ReturnStatementNode())
			case Failure(e) => Failure(e)
		}
	}

	// assignment-expression|simple-expression
	def readExpression(stream: TokStream): Try[Expression] = readAssignmentExpression(stream) orElse readSimpleExpression(stream)

	def readAssignmentExpression(stream: TokStream): Try[AssignmentExpressionNode] = {
		// var = expression
		stream.extractIf(
			Match(Seq((Left(_.tok == TokType.IDENTIFIER), "Expected identifier"), (Left(_.tok == TokType.ASSGNOP), "Expected \"=\""), (Right(readExpression), "Expected expression")))
		) match {
			case Success((tok, seq)) => Success(AssignmentExpressionNode(tok.head.text, seq.head.asInstanceOf[Expression]))
			case Failure(e) => Failure(e)
		}
	}

	def readSimpleExpression(stream: TokStream): Try[SimpleExpressionNode] = {
		// additive-expression RELOP additive-expression|additive-expression
		stream.extractIf(
			Match(Seq((Right(readAdditiveExpression), "Expected additive-expression"))),
			Optional(Seq((Left(_.tok == TokType.RELOP), "Expected relop"), (Right(readAdditiveExpression), "Expected additive-expression"))),
		) match {
			case Success((tok, seq)) if seq.length == 2 => Success(SimpleExpressionNode(seq.head.asInstanceOf[AdditiveExpressionNode], Some((tok.head.text, seq(1).asInstanceOf[AdditiveExpressionNode]))))
			case Success((_, seq)) => Success(SimpleExpressionNode(seq.head.asInstanceOf[AdditiveExpressionNode]))
			case Failure(e) => Failure(e)
		}
	}

	def readAdditiveExpression(stream: TokStream): Try[AdditiveExpressionNode] = {
		// term ADDOP additive-expression|term
		stream.extractIf(
			Match(Seq((Right(readTerm), "Expected term"))),
			Optional(Seq((Left(_.tok == TokType.ADDOP), "Expected addop"), (Right(readAdditiveExpression), "Expected additive-expression")))
		) match {
			case Success((tok, seq)) if seq.length == 2 => Success(AdditiveExpressionNode(seq.head.asInstanceOf[TermNode], Some((tok.head.text, seq(1).asInstanceOf[AdditiveExpressionNode]))))
			case Success((_, seq)) => Success(AdditiveExpressionNode(seq.head.asInstanceOf[TermNode]))
			case Failure(e) => Failure(e)
		}
	}

	def readTerm(stream: TokStream): Try[TermNode] = {
		// factor MULOP term|factor
		stream.extractIf(
			Match(Seq((Right(readFactor), "Expected factor"))),
			Optional(Seq((Left(_.tok == TokType.MULOP), "Expected mulop"), (Right(readTerm), "Expected term"))),
		) match {
			case Success((tok, seq)) if seq.length == 2 => Success(TermNode(seq.head.asInstanceOf[Factor], Some(tok.head.text, seq(1).asInstanceOf[TermNode])))
			case Success((_, seq)) => Success(TermNode(seq.head.asInstanceOf[Factor]))
			case Failure(e) => Failure(e)
		}
	}

	// ( expression )|var|call|int|float
	def readFactor(stream: TokStream): Try[Factor] = readCall(stream) orElse readVar(stream) orElse readNum(stream) orElse readParenExpression(stream)

	def readNum(stream: TokStream): Try[NumNode] = {
		stream.extractIf(
			Match(Seq((Left(t => t.tok == TokType.INT || t.tok == TokType.FLOAT), "Expected num"))),
		) match {
			case Success((tok, _)) if tok.head.tok == TokType.INT => Success(NumNode(Left(tok.head.text.toInt)))
			case Success((tok, _)) => Success(NumNode(Right(tok.head.text.toDouble)))
			case Failure(e) => Failure(e)
		}
	}

	def readParenExpression(stream: TokStream): Try[ParenExpressionNode] = {
		stream.extractIf(
			Match(Seq((Left(_.tok == TokType.OPAREN), "Expected \"(\""), (Right(readExpression), "Expected expression"), (Left(_.tok == TokType.CPAREN), "Expected \")\""))),
		) match {
			case Success((_, seq)) => Success(ParenExpressionNode(seq.head.asInstanceOf[Expression]))
			case Failure(e) => Failure(e)
		}
	}

	def readCall(stream: TokStream): Try[CallNode] = {
		// ID ( expression... )
		stream.extractIf(
			Match(Seq(Left(_.tok == TokType.IDENTIFIER), Left(_.tok == TokType.OPAREN))),
			Vararg(Seq(Right(readExpression), Left(_.tok == TokType.COMMA))),
			Match(Seq(Right(readExpression), Left(_.tok == TokType.CPAREN))),
		) orElse stream.extractIf(
			Match(Seq(Left(_.tok == TokType.IDENTIFIER), Left(_.tok == TokType.OPAREN), Left(_.tok == TokType.CPAREN)))
		) match {
			case Success((tok, seq)) => Success(CallNode(tok.head.text, seq.asInstanceOf[Seq[Expression]]))
			case Failure(e) => Failure(new ParseException(s"Expected call\n${e.getMessage}"))
		}
	}

	def readVar(stream: TokStream): Try[VarNode] = {
		// ID|ID [ expression ]
		stream.extractIf(
			Match(Seq(Left(_.tok == TokType.IDENTIFIER))),
			Optional(Seq(Left(_.tok == TokType.OBRACKET), Right(readExpression), Left(_.tok == TokType.CBRACKET)))
		) match {
			case Success((tok, seq)) if seq.length == 1 => Success(VarNode(tok.head.text, Some(seq.head.asInstanceOf[Expression])))
			case Success((tok, _)) => Success(VarNode(tok.head.text))
			case Failure(e) => Failure(new ParseException(s"Exprected var\n${e.getMessage}"))
		}
	}

	def readProgramNode(stream: TokStream): Try[ProgramNode] = {
		stream.extractIf(
			Vararg(Seq(Right(readDeclaration)))
		) match {
			case Success((_, seq)) => Success(ProgramNode(seq.asInstanceOf[Seq[Declaration]]))
			case Failure(e) => Failure(new ParseException(s"Expected program\n${e.getMessage}"))
		}
	}

	def apply[T >: ASTNode](tok: Seq[Token], lambda: TokStream => Try[T] = this.readProgramNode _): Try[T] = {
		val stream = new SeqTokStream(tok)
		val res = lambda(stream)
		res match {
			case Success(v) if stream.empty => Success(v)
			case Success(_) => Failure(new ParseException(s"Trailing tokens"))
			case Failure(e) => Failure(e)
		}
	}
}

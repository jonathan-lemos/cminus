import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success, Try}

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
	  *              The second member of a tuple denotes this as a "vararg", meaning it takes 0 or more.
	  * @return A sequence of the tokens/ASTNodes matched, or Failure()
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
}

class SeqTokStream(private val tok: Seq[Token]) extends TokStream {
	private var str: ListBuffer[Token] = tok.to[ListBuffer]

	override def peek: Token = str.head

	override def peekOption: Option[Token] = str.headOption

	override def extract: Token = {
		if (this.empty) throw new ParseException("Stream is empty")
		val ret = this.peek
		this.str.remove(0)
		ret
	}

	private def __extractIf(conds: (Either[Token => Boolean, TokStream => Try[ASTNode]], Boolean)*): Try[(Seq[Token], Seq[ASTNode])] = {
		val rettok = new ArrayBuffer[Token]
		val ret = new ArrayBuffer[ASTNode]
		for (cond <- conds) {
			val vararg = cond._2
			cond._1 match {
				case _ if this.empty =>
					return Failure(new ParseException("End of stream"))
				case Left(f) =>
					def m: Try[Unit] = {
						if (f(this.peek)) {
							rettok += this.extract
							Success()
						}
						else {
							Failure(new ParseException(this.peek.text))
						}
					}

					if (vararg) while (m.isSuccess) {} else m match {
						case Success(_) =>
						case Failure(e) => return Failure(e)
					}
				case Right(f) =>
					def m: Try[Unit] = f(this) match {
						case Success(node) => ret += node; Success()
						case Failure(e) => Failure(e)
					}

					if (vararg) while (m.isSuccess) {} else m match {
						case Success(_) =>
						case Failure(e) => return Failure(e)
					}
			}
		}
		Success((rettok, ret))
	}

	override def extractIf(conds: (Either[Token => Boolean, TokStream => Try[ASTNode]], Boolean)*): Try[(Seq[Token], Seq[ASTNode])] = {
		val tokSave = this.str.clone
		val ret = __extractIf(conds: _*)
		ret match {
			case Success(v) => Success(v)
			case Failure(e) => this.str = tokSave; Failure(e)
		}
	}

	override def nonEmpty: Boolean = this.str.nonEmpty

	override def empty: Boolean = this.str.isEmpty
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
			// TYPE ID = expression;
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.ASSGNOP && t.text == "="), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) orElse stream.extractIf(
			// TYPE ID ;
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) orElse stream.extractIf(
			// TYPE ID [ INT ] = expression ;
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "["), false),
			(Left(_.tok == TokenType.INT), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "]"), false),
			(Left(t => t.tok == TokenType.ASSGNOP && t.text == "="), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) orElse stream.extractIf(
			// TYPE ID [ INT ] ;
			(Left(_.tok == TokenType.TYPE), false),
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "["), false),
			(Left(_.tok == TokenType.INT), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "]"), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) match {
			case Success((tok, ast)) => tok.length match {
				case 4 => Success(VarDeclNode(tok.head.text, tok(1).text, None, Some(ast.head.asInstanceOf[Expression])))
				case 3 => Success(VarDeclNode(tok.head.text, tok(1).text))
				case 7 => Success(VarDeclNode(tok.head.text, tok(1).text, Some(tok(3).text.toInt), Some(ast.head.asInstanceOf[Expression])))
				case 6 => Success(VarDeclNode(tok.head.text, tok(1).text, Some(tok(3).text.toInt)))
			}
			case Failure(e) => Failure(new ParseException(s"Expected var-decl\n${e.getMessage}"))
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
			(Left(t => t.tok == TokenType.TYPE && t.text == "void"), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
			(Right(readCompoundStatement), false),
		) match {
			case Success((tseq, aseq)) => Success(FunDeclNode(tseq.head.text, tseq(1).text, aseq.slice(0, aseq.length - 1).asInstanceOf[Seq[ParamNode]], aseq.last.asInstanceOf[CompoundStatementNode]))
			case Failure(e) => Failure(new ParseException(s"Expected fun-decl\n${e.getMessage}"))
		}
	}

	def readParamComma(stream: TokStream): Try[ParamNode] = {
		// param ,
		stream.extractIf(
			(Right(readParam), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ","), false),
		) match {
			case Success((_, aseq)) => Success(aseq.head.asInstanceOf[ParamNode])
			case Failure(e) => Failure(new ParseException(s"Expected param ,\n${e.getMessage}"))
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
			case Failure(e) => Failure(new ParseException(s"Expected param\n${e.getMessage}"))
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
			case Success((_, seq)) => Success(CompoundStatementNode(seq.filter(_.isInstanceOf[VarDeclNode]).asInstanceOf[Seq[VarDeclNode]], seq.filter(_.isInstanceOf[Statement]).asInstanceOf[Seq[Statement]]))
			case Failure(e) => Failure(new ParseException(s"Expected compound-statement\n${e.getMessage}"))
		}
	}

	def readStatement(stream: TokStream): Try[Statement] = {
		// expression-stmt|compound-stmt|selection-stmt|iteration-stmt|return-stmt
		readExpressionStatement(stream) orElse readCompoundStatement(stream) orElse readSelectionStatement(stream) orElse readIterationStatement(stream) orElse readReturnStatement(stream) match {
			case Success(stmt) => Success(stmt)
			case Failure(e) => Failure(new ParseException(s"Expected statement\n${e.getMessage}"))
		}
	}

	def readExpressionStatement(stream: TokStream): Try[ExpressionStatementNode] = {
		// expression ;|;
		stream.extractIf(
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) match {
			case Success((_, seq)) if seq.length == 1 => Success(ExpressionStatementNode(Some(seq.head.asInstanceOf[Expression])))
			case Success((_, _)) => Success(ExpressionStatementNode())
			case Failure(e) => Failure(new ParseException(s"Expected expression-statement\n${e.getMessage}"))
		}
	}

	def readSelectionStatement(stream: TokStream): Try[SelectionStatementNode] = {
		// if ( expression ) statement else statement
		stream.extractIf(
			(Left(t => t.tok == TokenType.KEYWORD && t.text == "if"), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
			(Right(readStatement), false),
			(Left(t => t.tok == TokenType.KEYWORD && t.text == "else"), false),
			(Right(readStatement), false),
		) orElse stream.extractIf(
			(Left(t => t.tok == TokenType.KEYWORD && t.text == "if"), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
			(Right(readStatement), false),
		) match {
			case Success((_, seq)) if seq.length == 3 => Success(SelectionStatementNode(seq.head.asInstanceOf[Expression], seq(1).asInstanceOf[Statement], Some(seq(2).asInstanceOf[Statement])))
			case Success((_, seq)) => Success(SelectionStatementNode(seq.head.asInstanceOf[Expression], seq(1).asInstanceOf[Statement]))
			case Failure(e) => Failure(new ParseException(s"Expected selection-statement\n${e.getMessage}"))
		}
	}

	def readIterationStatement(stream: TokStream): Try[IterationStatementNode] = {
		// while ( expression ) statement
		stream.extractIf(
			(Left(t => t.tok == TokenType.KEYWORD && t.text == "while"), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
			(Right(readStatement), false),
		) match {
			case Success((_, seq)) => Success(IterationStatementNode(seq.head.asInstanceOf[Expression], seq(1).asInstanceOf[Statement]))
			case Failure(e) => Failure(new ParseException(s"Expected iteration-statement\n${e.getMessage}"))
		}
	}

	def readReturnStatement(stream: TokStream): Try[ReturnStatementNode] = {
		// return ;|return expression ;
		stream.extractIf(
			(Left(t => t.tok == TokenType.KEYWORD && t.text == "return"), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) orElse stream.extractIf(
			(Left(t => t.tok == TokenType.KEYWORD && t.text == "return"), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ";"), false),
		) match {
			case Success((_, seq)) if seq.length == 1 => Success(ReturnStatementNode(Some(seq.head.asInstanceOf[Expression])))
			case Success((_, _)) => Success(ReturnStatementNode())
			case Failure(e) => Failure(new ParseException(s"Expected return-statement\n${e.getMessage}"))
		}
	}

	// assignment-expression|simple-expression
	def readExpression(stream: TokStream): Try[Expression] = readAssignmentExpression(stream) orElse readSimpleExpression(stream) match {
		case Success(exp) => Success(exp)
		case Failure(e) => Failure(new ParseException(s"Expected expression\n${e.getMessage}"))
	}

	def readAssignmentExpression(stream: TokStream): Try[AssignmentExpressionNode] = {
		// var = expression
		stream.extractIf(
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.ASSGNOP && t.text == "="), false),
			(Right(readExpression), false)
		) match {
			case Success((tok, seq)) => Success(AssignmentExpressionNode(tok.head.text, seq.head.asInstanceOf[Expression]))
			case Failure(e) => Failure(new ParseException(s"Expected assignment-expression\n${e.getMessage}"))
		}
	}

	def readSimpleExpression(stream: TokStream): Try[SimpleExpressionNode] = {
		// additive-expression RELOP additive-expression|additive-expression
		stream.extractIf(
			(Right(readAdditiveExpression), false),
			(Left(_.tok == TokenType.RELOP), false),
			(Right(readAdditiveExpression), false),
		) orElse stream.extractIf(
			(Right(readAdditiveExpression), false),
		) match {
			case Success((tok, seq)) if seq.length == 2 => Success(SimpleExpressionNode(seq.head.asInstanceOf[AdditiveExpressionNode], Some((tok.head.text, seq(1).asInstanceOf[AdditiveExpressionNode]))))
			case Success((_, seq)) => Success(SimpleExpressionNode(seq.head.asInstanceOf[AdditiveExpressionNode]))
			case Failure(e) => Failure(new ParseException(s"Expected simple-expression\n${e.getMessage}"))
		}
	}

	def readAdditiveExpression(stream: TokStream): Try[AdditiveExpressionNode] = {
		// term ADDOP additive-expression|term
		stream.extractIf(
			(Right(readTerm), false),
			(Left(_.tok == TokenType.ADDOP), false),
			(Right(readAdditiveExpression), false),
		) orElse stream.extractIf(
			(Right(readTerm), false),
		) match {
			case Success((tok, seq)) if seq.length == 2 => Success(AdditiveExpressionNode(seq.head.asInstanceOf[TermNode], Some((tok.head.text, seq(1).asInstanceOf[AdditiveExpressionNode]))))
			case Success((_, seq)) => Success(AdditiveExpressionNode(seq.head.asInstanceOf[TermNode]))
			case Failure(e) => Failure(new ParseException(s"Expected additive-expression\n${e.getMessage}"))
		}
	}

	def readTerm(stream: TokStream): Try[TermNode] = {
		// factor MULOP term|factor
		stream.extractIf(
			(Right(readFactor), false),
			(Left(_.tok == TokenType.MULOP), false),
			(Right(readTerm), false),
		) orElse stream.extractIf(
			(Right(readFactor), false),
		) match {
			case Success((tok, seq)) if seq.length == 2 => Success(TermNode(seq.head.asInstanceOf[Factor], Some(tok.head.text, seq(1).asInstanceOf[TermNode])))
			case Success((_, seq)) => Success(TermNode(seq.head.asInstanceOf[Factor]))
			case Failure(e) => Failure(new ParseException(s"Expected term\n${e.getMessage}"))
		}
	}

	// ( expression )|var|call|int|float
	def readFactor(stream: TokStream): Try[Factor] = readVar(stream) orElse readCall(stream) orElse readParenExpression(stream) orElse readNum(stream)

	def readNum(stream: TokStream): Try[NumNode] = {
		stream.extractIf(
			(Left(t => t.tok == TokenType.INT || t.tok == TokenType.PUNCTUATION), false)
		) match {
			case Success((tok, _)) if tok.head.tok == TokenType.INT => Success(NumNode(Left(tok.head.text.toInt)))
			case Success((tok, _)) => Success(NumNode(Right(tok.head.text.toDouble)))
			case Failure(e) => Failure(new ParseException(s"Expected num\n${e.getMessage}"))
		}
	}

	def readParenExpression(stream: TokStream): Try[ParenExpressionNode] = {
		stream.extractIf(
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
		) match {
			case Success((_, seq)) => Success(seq.head.asInstanceOf[ParenExpressionNode])
			case Failure(e) => Failure(new ParseException(s"Expected paren-expression\n${e.getMessage}"))
		}
	}

	def readCall(stream: TokStream): Try[CallNode] = {
		// ID ( expression... )
		stream.extractIf(
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Right(readExpressionComma), true),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
		) orElse stream.extractIf(
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ")"), false),
		) match {
			case Success((tok, seq)) => Success(CallNode(tok.head.text, seq.asInstanceOf[Seq[Expression]]))
			case Failure(e) => Failure(new ParseException(s"Expected call\n${e.getMessage}"))
		}
	}

	def readExpressionComma(stream: TokStream): Try[Expression] = {
		// expression ,
		stream.extractIf(
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == ","), false),
		) match {
			case Success((_, seq)) => Success(seq.head.asInstanceOf[Expression])
			case Failure(e) => Failure(new ParseException(s"Expected expression-comma\n${e.getMessage}"))
		}
	}

	def readVar(stream: TokStream): Try[VarNode] = {
		// ID|ID [ expression ]
		stream.extractIf(
			(Left(_.tok == TokenType.IDENTIFIER), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "["), false),
			(Right(readExpression), false),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "]"), false),
		) orElse stream.extractIf(
			(Left(_.tok == TokenType.IDENTIFIER), false),
		) match {
			case Success((tok, seq)) if seq.length == 1 => Success(VarNode(tok.head.text, Some(seq.head.asInstanceOf[Expression])))
			case Success((tok, _)) => Success(VarNode(tok.head.text))
			case Failure(e) => Failure(new ParseException(s"Exprected var\n${e.getMessage}"))
		}
	}

	def readProgramNode(stream: TokStream): Try[ProgramNode] = {
		stream.extractIf((Right(readDeclaration), true)) match {
			case Success((_, seq)) => Success(ProgramNode(seq.asInstanceOf[Seq[Declaration]]))
			case Failure(e) => Failure(new ParseException(s"Expected program\n${e.getMessage}"))
		}
	}

	def apply[T >: ASTNode](tok: Seq[Token], lambda: TokStream => Try[T] = readProgramNode): Try[T] = lambda(new SeqTokStream(tok))
}

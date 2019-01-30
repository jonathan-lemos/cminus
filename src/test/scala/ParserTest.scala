import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class ParserTest extends FunSuite {
		test("Parser.Basic") {
		val tokens = Seq(
			Token(TokType.TYPE, "int", 1),
			Token(TokType.IDENTIFIER, "main", 1),
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.TYPE, "void", 1),
			Token(TokType.CPAREN, ")", 1),
			Token(TokType.OBRACE, "{", 1),

			Token(TokType.KEYWORD, "return", 2),
			Token(TokType.INT, "0", 2),
			Token(TokType.SEMICOLON, ";", 2),

			Token(TokType.CBRACE, "}", 3),
		)
		val tree = Parser(tokens)
		assert(tree.isSuccess)
		val expect = ProgramNode(Seq(
			FunDeclNode("int", "main", Seq(), CompoundStatementNode(
				Seq(),
				Seq(
					ReturnStatementNode(Some(SimpleExpressionNode(AdditiveExpressionNode(TermNode(NumNode(Left(0)))))))
				)
			))
		))
		assert(tree.get == expect)
	}

	test("Parser.SyntaxError") {
		val tokens = Seq(
			Token(TokType.TYPE, "int", 1),
			Token(TokType.IDENTIFIER, "main", 1),
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.TYPE, "void", 1),
			Token(TokType.CPAREN, ")", 1),
			Token(TokType.OBRACE, "{", 1),

			Token(TokType.KEYWORD, "return", 2),
			Token(TokType.INT, "0", 2),
			Token(TokType.INT, "0", 2),
			Token(TokType.SEMICOLON, ";", 2),

			Token(TokType.CBRACE, "}", 3),
		)
		val tree = Parser(tokens)
		assert(tree.isFailure)
		tree match {case Failure(e: ParseException) => e.printErr()}
	}

	test("Parser.ArithmeticComplicated") {
		val tokens = Seq(
			Token(TokType.INT, "46", 1),
			Token(TokType.MULOP, "*", 1),
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.IDENTIFIER, "x", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.IDENTIFIER, "y", 1),
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.CPAREN, ")", 1),
			Token(TokType.MULOP, "/", 1),
			Token(TokType.IDENTIFIER, "z", 1),
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.IDENTIFIER, "x", 1),
			Token(TokType.COMMA, ",", 1),
			Token(TokType.IDENTIFIER, "y", 1),
			Token(TokType.COMMA, ",", 1),
			Token(TokType.INT, "47", 1),
			Token(TokType.ADDOP, "-", 1),
			Token(TokType.INT, "2", 1),
			Token(TokType.CPAREN, ")", 1),
			Token(TokType.CPAREN, ")", 1),
		)
		val tree = Parser(tokens, Parser.readSimpleExpression)
		assert(tree.isSuccess)
		val expect =
		SimpleExpressionNode(
			AdditiveExpressionNode(
				TermNode(NumNode(Left(46)),
					Some(("*",
							TermNode(
								ParenExpressionNode(
									SimpleExpressionNode(
										AdditiveExpressionNode(
											TermNode(VarNode("x")),
											Some(("+",
													AdditiveExpressionNode(
														TermNode(CallNode("y",Seq()),
															Some(("/",
																	TermNode(CallNode("z",Seq(
																		SimpleExpressionNode(
																			AdditiveExpressionNode(
																				TermNode(VarNode("x"))
																			)
																		),
																		SimpleExpressionNode(
																			AdditiveExpressionNode(
																				TermNode(VarNode("y"))
																			)
																		),
																		SimpleExpressionNode(
																			AdditiveExpressionNode(
																				TermNode(NumNode(Left(47))),
																				Some(("-",
																						AdditiveExpressionNode(
																							TermNode(NumNode(Left(2)))
																						)
																				))
																			)
																		)
																	)))
															))
														)
													)
												))
										)
									)
								)
							)
					))
				)
			)
		)
		assert(tree.get == expect)
	}

	test("Parser.ArithmeticBasic") {
		val tokens = Seq(
			Token(TokType.INT, "46", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.INT, "2", 1),
		)
		val tree = Parser(tokens, Parser.readSimpleExpression)
		assert(tree.isSuccess)
		val expect = SimpleExpressionNode(
			AdditiveExpressionNode(
				TermNode(NumNode(Left(46))),
				Some(("+", AdditiveExpressionNode(
					TermNode(NumNode(Left(2))),
				)))
			)
		)
		assert(tree.get == expect)
	}

	test("Parser.ParamsVarDecl") {
		val tokens = Seq(
			Token(TokType.TYPE, "int", 1),
			Token(TokType.IDENTIFIER, "main", 1),
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.TYPE, "int", 1),
			Token(TokType.IDENTIFIER, "argc", 1),
			Token(TokType.COMMA, ",", 1),
			Token(TokType.TYPE, "float", 1),
			Token(TokType.IDENTIFIER, "argv", 1),
			Token(TokType.OBRACKET, "[", 1),
			Token(TokType.CBRACKET, "]", 1),
			Token(TokType.CPAREN, ")", 1),
			Token(TokType.OBRACE, "{", 1),

			Token(TokType.TYPE, "int", 2),
			Token(TokType.IDENTIFIER, "x", 2),
			Token(TokType.ASSGNOP, "=", 2),
			Token(TokType.INT, "4", 2),
			Token(TokType.SEMICOLON, ";", 2),

			Token(TokType.KEYWORD, "return", 3),
			Token(TokType.IDENTIFIER, "x", 3),
			Token(TokType.SEMICOLON, ";", 3),

			Token(TokType.CBRACE, "}", 3),
		)
		val tree = Parser(tokens)
		assert(tree.isSuccess)
		val expect = ProgramNode(Seq(
			FunDeclNode("int", "main", Seq(ParamNode("int", "argc"), ParamNode("float", "argv", array = true)), CompoundStatementNode(
				Seq(VarDeclNode("int", "x", None, Some(SimpleExpressionNode(AdditiveExpressionNode(TermNode(NumNode(Left(4)))))))),
				Seq(ReturnStatementNode(Some(SimpleExpressionNode(AdditiveExpressionNode(TermNode(VarNode("x"))))))),
			))
		))
		assert(tree.get == expect)
	}
}

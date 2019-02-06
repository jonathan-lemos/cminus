import org.scalatest.FunSuite

import scala.util.Failure

class ParserTest extends FunSuite {
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
			Token(TokType.SEMICOLON, ";", 2),

			Token(TokType.CBRACE, "}", 3),
			Token(TokType.CBRACE, "}", 3),
		)
		val tree = Parser(tokens)
		assert(tree.isFailure)
		tree match {case Failure(e: ParseException) => e.printErr(); case _ => assert(false)}
	}

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
		val expect = ProgramNode(1, Seq(
			FunDeclNode(1, "int", "main", Seq(), CompoundStatementNode(
				1,
				Seq(),
				Seq(
					ReturnStatementNode(2, Some(SimpleExpressionNode(2, AdditiveExpressionNode(2, TermNode(2, NumNode(2, Left(0)))))))
				)
			))
		))
		assert(tree.get == expect)
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
				1,
				AdditiveExpressionNode(
					1,
					TermNode(1, NumNode(1, Left(46)),
						Some(("*",
								TermNode(
									1,
									ParenExpressionNode(
										1,
										SimpleExpressionNode(
											1,
											AdditiveExpressionNode(
												1,
												TermNode(1, VarNode(1, "x")),
												Some(("+",
														AdditiveExpressionNode(
															1,
															TermNode(1, CallNode(1, "y",Seq()),
																Some(("/",
																		TermNode(1, CallNode(1, "z",Seq(
																			SimpleExpressionNode(
																				1,
																				AdditiveExpressionNode(
																					1,
																					TermNode(1, VarNode(1, "x"))
																				)
																			),
																			SimpleExpressionNode(
																				1,
																				AdditiveExpressionNode(
																					1,
																					TermNode(1, VarNode(1, "y"))
																				)
																			),
																			SimpleExpressionNode(
																				1,
																				AdditiveExpressionNode(
																					1,
																					TermNode(1, NumNode(1, Left(47))),
																					Some(("-",
																							AdditiveExpressionNode(
																								1,
																								TermNode(1, NumNode(1, Left(2)))
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
			1,
			AdditiveExpressionNode(
				1,
				TermNode(1, NumNode(1, Left(46))),
				Some(("+", AdditiveExpressionNode(
					1,
					TermNode(1, NumNode(1, Left(2))),
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
		val expect = ProgramNode(1, Seq(
			FunDeclNode(1, "int", "main", Seq(ParamNode(1, "int", "argc"), ParamNode(1, "float", "argv", array = true)), CompoundStatementNode(
				1,
				Seq(VarDeclNode(2, "int", "x", None, Some(SimpleExpressionNode(2, AdditiveExpressionNode(2, TermNode(2, NumNode(2, Left(4)))))))),
				Seq(ReturnStatementNode(3, Some(SimpleExpressionNode(3, AdditiveExpressionNode(3, TermNode(3, VarNode(3, "x"))))))),
			))
		))
		assert(tree.get == expect)
	}

	test("Parser.Empty") {
		val tokens = Seq()
		val tree = Parser(tokens)
		assert(tree.isFailure)
	}
}

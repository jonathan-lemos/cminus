import org.scalatest.FunSuite

class ParserTest extends FunSuite {
	test("Parser.ArithmeticComplicated") {
		val tokens = Seq(
			Token(TokenType.INT, "46", 1),
			Token(TokenType.MULOP, "*", 1),
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.IDENTIFIER, "x", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.IDENTIFIER, "y", 1),
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
			Token(TokenType.MULOP, "/", 1),
			Token(TokenType.IDENTIFIER, "z", 1),
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.IDENTIFIER, "x", 1),
			Token(TokenType.PUNCTUATION, ",", 1),
			Token(TokenType.IDENTIFIER, "y", 1),
			Token(TokenType.PUNCTUATION, ",", 1),
			Token(TokenType.INT, "47", 1),
			Token(TokenType.ADDOP, "-", 1),
			Token(TokenType.INT, "2", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
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
			Token(TokenType.INT, "46", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.INT, "2", 1),
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

	test("Parser.Basic") {
		val tokens = Seq(
			Token(TokenType.TYPE, "int", 1),
			Token(TokenType.IDENTIFIER, "main", 1),
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.TYPE, "void", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
			Token(TokenType.PUNCTUATION, "{", 1),

			Token(TokenType.KEYWORD, "return", 2),
			Token(TokenType.INT, "0", 2),
			Token(TokenType.PUNCTUATION, ";", 2),

			Token(TokenType.PUNCTUATION, "}", 3),
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

	test("Parser.ParamsVarDecl") {
		val tokens = Seq(
			Token(TokenType.TYPE, "int", 1),
			Token(TokenType.IDENTIFIER, "main", 1),
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.TYPE, "int", 1),
			Token(TokenType.IDENTIFIER, "argc", 1),
			Token(TokenType.PUNCTUATION, ",", 1),
			Token(TokenType.TYPE, "float", 1),
			Token(TokenType.IDENTIFIER, "argv", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
			Token(TokenType.PUNCTUATION, "{", 1),

			Token(TokenType.TYPE, "int", 2),
			Token(TokenType.IDENTIFIER, "x", 2),
			Token(TokenType.ASSGNOP, "=", 2),
			Token(TokenType.INT, "4", 2),
			Token(TokenType.PUNCTUATION, ";", 2),

			Token(TokenType.KEYWORD, "return", 3),
			Token(TokenType.IDENTIFIER, "x", 3),
			Token(TokenType.PUNCTUATION, ";", 3),

			Token(TokenType.PUNCTUATION, "}", 3),
		)
		val tree = Parser(tokens)
		assert(tree.isSuccess)
		val expect = ProgramNode(Seq(
			FunDeclNode("int", "main", Seq(ParamNode("int", "argc"), ParamNode("float", "argv")), CompoundStatementNode(
				Seq(VarDeclNode("int", "x", None, Some(SimpleExpressionNode(AdditiveExpressionNode(TermNode(NumNode(Left(4)))))))),
				Seq(ReturnStatementNode(Some(SimpleExpressionNode(AdditiveExpressionNode(TermNode(VarNode("x"))))))),
			))
		))
		assert(tree.get == expect)
	}
}

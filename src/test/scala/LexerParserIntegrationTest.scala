import org.scalatest.FunSuite

class LexerParserIntegrationTest extends FunSuite {
	test("LexerParser.Basic") {
		val lines = Seq(
			"2 +(3*4)"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readSimpleExpression)
		assert(tree.isSuccess)
		val expect =
		SimpleExpressionNode(
			AdditiveExpressionNode(
				TermNode(NumNode(Left(2))),
				Some(("+",AdditiveExpressionNode(
					TermNode(
						ParenExpressionNode(
							SimpleExpressionNode(
								AdditiveExpressionNode(
									TermNode(NumNode(Left(3)),
										Some(("*",TermNode(NumNode(Left(4)))))))))))))))
		assert(tree.get == expect)
	}

	test("LexerParser.Big") {
		val lines = Seq(
			"int square(int x) {",
			"   int i = 1;",
			"   int y = x;",
			"   while (i < x) {",
			"       y = y + x;",
			"   }",
			"   return y;",
			"}",
			"",
			"float pi = 6.28 / 2;",
			"",
			"int main(void) {",
			"   float twopi = 2 * pi;",
			"   print(2 + (2 + 2) * square(twopi));",
			"}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens)
		assert(tree.isSuccess)
	}
}

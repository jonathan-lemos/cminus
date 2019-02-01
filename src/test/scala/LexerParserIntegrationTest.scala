import org.scalatest.FunSuite

class LexerParserIntegrationTest extends FunSuite {
	test("LexerParser.FuktTest") {
		val lines = Seq(
			"print(2 + (3 + (4 * square())) * square(twopi));"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readExpressionStatement)
		assert(tree.isSuccess)
	}

	test("LexerParser.Basic") {
		val lines = Seq(
			"2 +(3*4)"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readSimpleExpression)
		assert(tree.isSuccess)
		val expect =
		SimpleExpressionNode(
			1,
			AdditiveExpressionNode(
				1,
				TermNode(1, NumNode(1, Left(2))),
				Some(("+",AdditiveExpressionNode(
					1,
					TermNode(
						1,
						ParenExpressionNode(
							1,
							SimpleExpressionNode(
								1,
								AdditiveExpressionNode(
									1,
									TermNode(1, NumNode(1, Left(3)),
										Some(("*",TermNode(1, NumNode(1, Left(4)))))))))))))))
		assert(tree.get == expect)
	}

	test("LexerParser.BigAccept") {
		val lines = Seq(
			"/* /* does some bullshit */",
			"int q = abcacadabra; // <- this should be ignored",
			"*/",
			"float pow(float x, int yfactor) {",
			"   int i = 0;",
			"   float ret = -1.0e+19 * -1;",
			"   while (i < (yfactor + 1) / 2) {",
			"       ret = ret * x;",
			"       if (ret < 0)",
			"           while (ret < 0) ret = ret + x;",
			"       else { ret = ret * -1; }",
			"       x = x + 1;",
			"   }",
			"   return ret;",
			"}",
			"",
			"// doesn't actually print anything",
			"void printall(float x[]) {",
			"   int y[15];",
			"   ;;;;;;",
			"}",
			"",
			"float pi = (5.28 + 1) / pow(2e1, 1);",
			"int multiplier = 2 + 3 + 4 * (5 * (pow(2.4e7, 6) + sqrt(4)));",
			"",
			"int main(void) {",
			"   printall(pow(pi, multiplier[2], 3 == 3) * (4.7 + 2));",
			"   return 0;",
			"}"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens)
		assert(tree.isSuccess)
	}

	test("LexerParser.FailFloatArrayLen") {
		val lines = Seq("int x[2.4];")
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readVarDecl)
		assert(tree.isFailure)
	}
}

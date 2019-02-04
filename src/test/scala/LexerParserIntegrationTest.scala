import org.scalatest.FunSuite

import scala.util.{Failure, Try}

class LexerParserIntegrationTest extends FunSuite {
	def prettyPrint(t: Try[ASTNode]): Unit = t match {case Failure(e: ParseException) => e.printErr(); case _ =>}

	test("LexerParser.FailFunDeclVarDeclBelowStmt") {
		val lines = Seq(
			"int q(int x, int y) {",
			"   return 0;",
			"   float x;",
			"}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

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
			"   while (i + 4 < (yfactor + 1) / 2) {",
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

	test("LexerParser.BigAccept2") {
		val lines = Seq(
		"/*/* does some bullshit */",
		"MEGA_SYNTAX_ERROR */",
		"",
		"int MEGACONSTANT = 2;",
		"float HYPERCONSTANT = 6.28;",
		"float PI = HYPERCONSTANT * (MEGACONSTANT / 2e0 / MEGACONSTANT);",
		"",
		"int MEGAFUNCTION ( void )",
		"{",
		"",
		"	return 4; }",
		"",
		"int MEGAFUNCTIONTWO(int x,int y){if (x < 0) return x + y; else return x - y;}",
		"float a(float b) { // MEGA_SYNTAX_ERROR 2",
		"	int c = 4.0;",
		"	if (b < 0) return 0.0;",
		"	while (c * (4 + b + MEGACONSTANT * 5) <= 3e19 / (7 + a(-3e-4 + (3 / 7 + (MEGAFUNCTION() + MEGAFUNCTIONTWO(3, 4e4))))+2)) {",
		"		c = c - MEGAFUNCTION() + MEGACONSTANT;",
		"		while(0)return 4;",
		"		if (c < 0) {",
		"			if (b < 0)",
		"				return 1;",
		"		}",
		"		else {",
		"			c = c + 2;",
		"		}",
		"	};",
		"	;;;;",
		"	return MEGACONSTANT + 3e+9;",
		"}",
		"",
		"int main(void){int x;",
		"	int y[4];",
		"	int xx = 4;",
		"	x = 4;",
		"	y[0] = 4.0e+0;",
		"	y[1] = MEGAFUNCTIONTWO(x+xx,xx+(xx*MEGAFUNCTION()));",
		"	y[2] = MEGAFUNCTION() + a(y[1]);",
		"	y[1 + (1 * MEGAFUNCTION()) - MEGAFUNCTIONTWO(0, 0.0)] = 17;",
		"	return y[0] + (y[1] + y[2]);",
		"}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens)
		prettyPrint(tree)
		assert(tree.isSuccess)
	}

	test("LexerParser.FailFloatArrayLen") {
		val lines = Seq("int x[2.4];")
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readVarDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.FailFunDeclEmpty") {
		val lines = Seq(
			"int q() {",
			"   return 0;",
			"}"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.FailFunDeclTrailingComma") {
		val lines = Seq(
			"int q(int x, int y,) {",
			"   return 0;",
			"}"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.FailFunDeclMissingID") {
		val lines = Seq(
			"int q(int, int y) {",
			"   return 0;",
			"}"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.FailFunDeclMissingType") {
		val lines = Seq(
			"int q(int x, y) {",
			"   return 0;",
			"}"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.FailFunDeclNoStmt") {
		val lines = Seq(
			"int q(int x, int y) return 0;"
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.FailFunDeclNoID") {
		val lines = Seq(
			"int (int x, int y) {",
			"   return 0;",
			"}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.FailFunDeclNoTYPE") {
		val lines = Seq(
			"q(int x, int y) {",
			"   return 0;",
			"}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isFailure)
		prettyPrint(tree)
	}

	test("LexerParser.SuccessFunDecl1") {
		val lines = Seq(
			"int q(int x, void y) {",
			"   int x;",
			"   int y = 4;",
			"   int y[16];",
			"   int y[15] = 4;",
			"   return x;",
			"}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isSuccess)
		prettyPrint(tree)
	}

	test("LexerParser.SuccessFunDecl2") {
		val lines = Seq(
			"int q(void) {}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isSuccess)
		prettyPrint(tree)
	}

	test("LexerParser.SuccessFunDecl3") {
		val lines = Seq(
			"int q(float r) {}",
		)
		val tokens = Lexer(lines)
		val tree = Parser(tokens, Parser.readFunDecl)
		assert(tree.isSuccess)
		prettyPrint(tree)
	}

}

import org.scalatest.FunSuite

class LexerTest extends FunSuite {
	test("Lexer.basic") {
		val res = Lexer(Seq(
			"( -123e+19 else== int abc 123 123.0}"
		))
		val expect = Seq(
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.FLOAT, "-123e+19", 1),
			Token(TokType.KEYWORD, "else", 1),
			Token(TokType.RELOP, "==", 1),
			Token(TokType.TYPE, "int", 1),
			Token(TokType.IDENTIFIER, "abc", 1),
			Token(TokType.INT, "123", 1),
			Token(TokType.FLOAT, "123.0", 1),
			Token(TokType.CBRACE, "}", 1),
		)
		assert(res == expect)
	}

	test("Lexer.float") {
		val res = Lexer(Seq(
			"2.0 0.2 -2.0 +2.0 -2.0e+14 -02.040E-14 2e14 .2 2. 2.4.3 23 0..2"
		))
		val expect = Seq(
			Token(TokType.FLOAT, "2.0", 1),
			Token(TokType.FLOAT, "0.2", 1),
			Token(TokType.FLOAT, "-2.0", 1),
			Token(TokType.FLOAT, "+2.0", 1),
			Token(TokType.FLOAT, "-2.0e+14", 1),
			Token(TokType.FLOAT, "-02.040E-14", 1),
			Token(TokType.FLOAT, "2e14", 1),
			Token(TokType.ERROR, ".", 1),
			Token(TokType.INT, "2", 1),
			Token(TokType.INT, "2", 1),
			Token(TokType.ERROR, ".", 1),
			Token(TokType.FLOAT, "2.4", 1),
			Token(TokType.ERROR, ".", 1),
			Token(TokType.INT, "3", 1),
			Token(TokType.INT, "23", 1),
			Token(TokType.INT, "0", 1),
			Token(TokType.ERROR, ".", 1),
			Token(TokType.ERROR, ".", 1),
			Token(TokType.INT, "2", 1),
		)
		assert(res == expect)
	}

	test("Lexer.multiline") {
		val res = Lexer(Seq(
			"int xX = 4;",
			"if (xX == 4){",
			"   float pi = 4/3.14;",
			"   print(3.14);",
			"}"
		))
		val expect = Seq(
			Token(TokType.TYPE, "int", 1),
			Token(TokType.IDENTIFIER, "xX", 1),
			Token(TokType.ASSGNOP, "=", 1),
			Token(TokType.INT, "4", 1),
			Token(TokType.SEMICOLON, ";", 1),

			Token(TokType.KEYWORD, "if", 2),
			Token(TokType.OPAREN, "(", 2),
			Token(TokType.IDENTIFIER, "xX", 2),
			Token(TokType.RELOP, "==", 2),
			Token(TokType.INT, "4", 2),
			Token(TokType.CPAREN, ")", 2),
			Token(TokType.OBRACE, "{", 2),

			Token(TokType.TYPE, "float", 3),
			Token(TokType.IDENTIFIER, "pi", 3),
			Token(TokType.ASSGNOP, "=", 3),
			Token(TokType.INT, "4", 3),
			Token(TokType.MULOP, "/", 3),
			Token(TokType.FLOAT, "3.14", 3),
			Token(TokType.SEMICOLON, ";", 3),

			Token(TokType.IDENTIFIER, "print", 4),
			Token(TokType.OPAREN, "(", 4),
			Token(TokType.FLOAT, "3.14", 4),
			Token(TokType.CPAREN, ")", 4),
			Token(TokType.SEMICOLON, ";", 4),

			Token(TokType.CBRACE, "}", 5),
		)
		assert(res == expect)
	}

	test("Lexer.equals") {
		val res = Lexer(Seq(
			"=!= >=="
		))
		val expect = Seq(
			Token(TokType.ASSGNOP, "=", 1),
			Token(TokType.RELOP, "!=", 1),
			Token(TokType.RELOP, ">=", 1),
			Token(TokType.ASSGNOP, "=", 1),
		)
		assert(res == expect)
	}

	test("Lexer.keyword") {
		val res = Lexer(Seq(
			"elseif else if"
		))
		val expect = Seq(
			Token(TokType.IDENTIFIER, "elseif", 1),
			Token(TokType.KEYWORD, "else", 1),
			Token(TokType.KEYWORD, "if", 1),
		)
		assert(res == expect)
	}

	test("Lexer.comment") {
		val res = Lexer(Seq(
			"/** /abc*/*def/* * / *//*/*ghi*/*/jkl/*/*mno/*",
			"pqr */ stu */ */ vwx */",
			"yza /* bcd /* efg */"
		))
		val expect = Seq(
			Token(TokType.MULOP, "*", 1),
			Token(TokType.IDENTIFIER, "def", 1),
			Token(TokType.IDENTIFIER, "jkl", 1),
			Token(TokType.IDENTIFIER, "vwx", 2),
			Token(TokType.MULOP, "*", 2),
			Token(TokType.MULOP, "/", 2),
			Token(TokType.IDENTIFIER, "yza", 3),
			Token(TokType.ERROR, "Expected */", 3),
		)
		assert(res == expect)
	}

	test("Lexer.ifa") {
		val res = Lexer(Seq("ifa"))
		val expect = Seq(Token(TokType.IDENTIFIER, "ifa", 1))
		assert(res == expect)
	}
}

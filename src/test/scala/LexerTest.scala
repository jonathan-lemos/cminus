import org.scalatest.FunSuite

class LexerTest extends FunSuite {
	test("Lexer.basic") {
		val res = Lexer(Seq(
			"(else== int abc 123 }"
		))
		val expect = Seq(
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.KEYWORD, "else", 1),
			Token(TokenType.RELOP, "==", 1),
			Token(TokenType.TYPE, "int", 1),
			Token(TokenType.IDENTIFIER, "abc", 1),
			Token(TokenType.NUMBER, "123", 1),
			Token(TokenType.PUNCTUATION, "}", 1),
		)
		assert(res == expect)
	}

	test("Lexer.multiline") {
		val res = Lexer(Seq(
			"int x_X = 4;",
			"if (x_X == 4){",
			"   float pi = 4/3.14;",
			"   print(3.14);",
			"}"
		))
		val expect = Seq(
			Token(TokenType.TYPE, "int", 1),
			Token(TokenType.IDENTIFIER, "x_X", 1),
			Token(TokenType.ASSGNOP, "=", 1),
			Token(TokenType.NUMBER, "4", 1),
			Token(TokenType.PUNCTUATION, ";", 1),

			Token(TokenType.KEYWORD, "if", 2),
			Token(TokenType.PUNCTUATION, "(", 2),
			Token(TokenType.IDENTIFIER, "x_X", 2),
			Token(TokenType.RELOP, "==", 2),
			Token(TokenType.NUMBER, "4", 2),
			Token(TokenType.PUNCTUATION, ")", 2),
			Token(TokenType.PUNCTUATION, "{", 2),

			Token(TokenType.TYPE, "float", 3),
			Token(TokenType.IDENTIFIER, "pi", 3),
			Token(TokenType.ASSGNOP, "=", 3),
			Token(TokenType.NUMBER, "4", 3),
			Token(TokenType.MULOP, "/", 3),
			Token(TokenType.NUMBER, "3.14", 3),
			Token(TokenType.PUNCTUATION, ";", 3),

			Token(TokenType.IDENTIFIER, "print", 4),
			Token(TokenType.PUNCTUATION, "(", 4),
			Token(TokenType.NUMBER, "3.14", 4),
			Token(TokenType.PUNCTUATION, ")", 4),
			Token(TokenType.PUNCTUATION, ";", 4),

			Token(TokenType.PUNCTUATION, "}", 5),
		)
		assert(res == expect)
	}

	test("Lexer.equals") {
		val res = Lexer(Seq(
			"=!= >=="
		))
		val expect = Seq(
			Token(TokenType.ASSGNOP, "=", 1),
			Token(TokenType.RELOP, "!=", 1),
			Token(TokenType.RELOP, ">=", 1),
			Token(TokenType.ASSGNOP, "=", 1),
		)
		assert(res == expect)
	}

	test("Lexer.keyword") {
		val res = Lexer(Seq(
			"elseif else if"
		))
		val expect = Seq(
			Token(TokenType.IDENTIFIER, "elseif", 1),
			Token(TokenType.KEYWORD, "else", 1),
			Token(TokenType.KEYWORD, "if", 1),
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
			Token(TokenType.MULOP, "*", 1),
			Token(TokenType.IDENTIFIER, "def", 1),
			Token(TokenType.IDENTIFIER, "jkl", 1),
			Token(TokenType.IDENTIFIER, "vwx", 2),
			Token(TokenType.MULOP, "*", 2),
			Token(TokenType.MULOP, "/", 2),
			Token(TokenType.IDENTIFIER, "yza", 3),
			Token(TokenType.ERROR, "Expected */", 3),
		)
		assert(res == expect)
	}
}

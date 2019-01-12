import org.scalatest.FunSuite

class LexerTest extends FunSuite {
	test("Lexer.basic") {
		val res = Lexer(Seq(
			"(else== int abc 123 }"
		))
		val expect = Seq(
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.KEYWORD, "else", 1),
			Token(TokenType.OPERATOR, "==", 1),
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
			"   float pi = 4+3.14;",
			"   print(3.14);",
			"}"
		))
		val expect = Seq(
			Token(TokenType.TYPE, "int", 1),
			Token(TokenType.IDENTIFIER, "x_X", 1),
			Token(TokenType.OPERATOR, "=", 1),
			Token(TokenType.NUMBER, "4", 1),
			Token(TokenType.PUNCTUATION, ";", 1),

			Token(TokenType.KEYWORD, "if", 2),
			Token(TokenType.PUNCTUATION, "(", 2),
			Token(TokenType.IDENTIFIER, "x_X", 2),
			Token(TokenType.OPERATOR, "==", 2),
			Token(TokenType.NUMBER, "4", 2),
			Token(TokenType.PUNCTUATION, ")", 2),
			Token(TokenType.PUNCTUATION, "{", 2),

			Token(TokenType.TYPE, "float", 3),
			Token(TokenType.IDENTIFIER, "pi", 3),
			Token(TokenType.OPERATOR, "=", 3),
			Token(TokenType.NUMBER, "4", 3),
			Token(TokenType.OPERATOR, "+", 3),
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
			"=!="
		))
		val expect = Seq(
			Token(TokenType.OPERATOR, "=", 1),
			Token(TokenType.OPERATOR, "!=", 1),
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
			"/*abc*/**/ */def*/ghi/*jkl/*",
			"abc/**/*/def*/*/",
			"ghi/*"
		))
		val expect = Seq(
			Token(TokenType.IDENTIFIER, "ghi", 1),
			Token(TokenType.ERROR, "*", 2),
			Token(TokenType.IDENTIFIER, "ghi", 3),
			Token(TokenType.ERROR, "Expected */", 3),
		)
		assert(res == expect)
	}
}

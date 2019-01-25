import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class SeqTokStreamTest extends FunSuite {
	case class ExprNode(i: Int, addop: String, f: Float) extends ASTNode

	def readExpr(stream: TokStream): Try[ASTNode] = {
		stream.extractIf(
			(Left(_.tok == TokenType.INT), false),
			(Left(_.tok == TokenType.ADDOP), false),
			(Left(_.tok == TokenType.FLOAT), false),
		) match {
			case Success((tok, seq)) =>
				Success(ExprNode(tok.head.text.toInt, tok(1).text, tok(2).text.toFloat))
			case Failure(e) => Failure(e)
		}
	}

	test("SeqTokStream.Basic") {
		val tokens = Seq(
			Token(TokenType.INT, "2", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.FLOAT, "2.3", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.peek.tok == TokenType.INT)
		assert(stream.extract.tok == TokenType.INT)
		assert(stream.peekOption.isDefined && stream.peekOption.get.text == "+")
		assert(stream.extract.tok == TokenType.ADDOP)
		assert(stream.nonEmpty)
		assert(stream.extract.tok == TokenType.FLOAT)
		assert(stream.empty)
		assert(stream.peekOption.isEmpty)
		assertThrows[NoSuchElementException](stream.peek)
		assertThrows[ParseException](stream.extract)
	}

	test("SeqTokStream.ExtractIfBasic") {
		val tokens = Seq(
			Token(TokenType.INT, "2", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.FLOAT, "2.3", 1)
		)
		var stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.INT), false),
			(Left(_.text == "+"), false),
			(Left(_.line == 1), false),
		).isSuccess)
		stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			(Right(readExpr), false),
		).isSuccess)
	}

	test("SeqTokStream.ExtractIfVararg") {
		val tokens = Seq(
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.INT, "2", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.FLOAT, "2.3", 1),
			Token(TokenType.INT, "2", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.FLOAT, "2.3", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.PUNCTUATION), false),
			(Right(readExpr), true),
			(Left(_.tok == TokenType.PUNCTUATION), false),
		).get._2.length == 2)
	}

	test("SeqTokStream.ExtractIfVarargNone") {
		val tokens = Seq(
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.PUNCTUATION), false),
			(Right(readExpr), true),
			(Left(_.tok == TokenType.PUNCTUATION), false),
		).get._2.isEmpty)
	}

	test("SeqTokStream.BasicFailure") {
		val tokens = Seq(
			Token(TokenType.INT, "2", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.FLOAT, "2.3", 1)
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.INT), false),
			(Left(_.tok == TokenType.ADDOP), false),
			(Left(_.tok == TokenType.ADDOP), false),
		).isFailure)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.INT), false),
			(Left(_.tok == TokenType.ADDOP), false),
			(Left(_.tok == TokenType.FLOAT), false),
			(Left(_ => true), false),
		).isFailure)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.INT), false),
			(Left(_.text == "+"), false),
			(Left(_.line == 1), false),
		).isSuccess)
	}

	test("SeqTokStream.RightFailure") {
		val tokens = Seq(
			Token(TokenType.PUNCTUATION, "(", 1),
			Token(TokenType.INT, "2", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.FLOAT, "2.3", 1),
			Token(TokenType.INT, "2", 1),
			Token(TokenType.ADDOP, "+", 1),
			Token(TokenType.FLOAT, "2.3", 1),
			Token(TokenType.PUNCTUATION, ")", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.PUNCTUATION), false),
			(Right(readExpr), true),
			(Left(t => t.tok == TokenType.PUNCTUATION && t.text == "("), false)
		).isFailure)
		assert(stream.extractIf(
			(Left(_.tok == TokenType.PUNCTUATION), false),
			(Right(readExpr), true),
			(Left(_.tok == TokenType.PUNCTUATION), false),
		).get._2.length == 2)
	}
}

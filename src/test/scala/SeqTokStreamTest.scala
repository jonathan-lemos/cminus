import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class SeqTokStreamTest extends FunSuite {
	case class ExprNode(i: Int, addop: String, f: Float) extends ASTNode

	def readExpr(stream: TokStream): Try[ASTNode] = {
		stream.extractIf(
			Match(Seq(Left(_.tok == TokType.INT), Left(_.tok == TokType.ADDOP), Left(_.tok == TokType.FLOAT)))
		) match {
			case Success((tok, _)) =>
				Success(ExprNode(tok.head.text.toInt, tok(1).text, tok(2).text.toFloat))
			case Failure(e) => Failure(e)
		}
	}

	test("SeqTokStream.Basic") {
		val tokens = Seq(
			Token(TokType.INT, "2", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.FLOAT, "2.3", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.peek.tok == TokType.INT)
		assert(stream.extract.tok == TokType.INT)
		assert(stream.peekOption.isDefined && stream.peekOption.get.text == "+")
		assert(stream.extract.tok == TokType.ADDOP)
		assert(stream.nonEmpty)
		assert(stream.extract.tok == TokType.FLOAT)
		assert(stream.empty)
		assert(stream.peekOption.isEmpty)
		assertThrows[ArrayIndexOutOfBoundsException](stream.peek)
		assertThrows[ParseException](stream.extract)
	}

	test("SeqTokStream.ExtractIfBasic") {
		val tokens = Seq(
			Token(TokType.INT, "2", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.FLOAT, "2.3", 1)
		)
		var stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.INT), Left(_.text == "+"), Left(_.line == 1)))
		).isSuccess)
		stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			Match(Seq(Right(readExpr))),
		).isSuccess)
	}

	test("SeqTokStream.ExtractIfVararg") {
		val tokens = Seq(
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.INT, "2", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.FLOAT, "2.3", 1),
			Token(TokType.INT, "2", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.FLOAT, "2.3", 1),
			Token(TokType.CPAREN, ")", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.OPAREN))),
			Vararg(Seq(Right(readExpr))),
			Match(Seq(Left(_.tok == TokType.CPAREN))),
		).get._2.length == 2)
	}

	test("SeqTokStream.ExtractIfVarargNone") {
		val tokens = Seq(
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.CPAREN, ")", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.OPAREN))),
			Vararg(Seq(Right(readExpr))),
			Match(Seq(Left(_.tok == TokType.CPAREN))),
		).get._2.isEmpty)
	}

	test("SeqTokStream.BasicFailure") {
		val tokens = Seq(
			Token(TokType.INT, "2", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.FLOAT, "2.3", 1)
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.INT), Left(_.tok == TokType.ADDOP), Left(_.tok == TokType.ADDOP)))
		).isFailure)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.INT), Left(_.tok == TokType.ADDOP), Left(_.tok == TokType.FLOAT), Left(_ => true))),
		).isFailure)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.INT), Left(_.text == "+"), Left(_.line == 1))),
		).isSuccess)
	}

	test("SeqTokStream.RightFailure") {
		val tokens = Seq(
			Token(TokType.OPAREN, "(", 1),
			Token(TokType.INT, "2", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.FLOAT, "2.3", 1),
			Token(TokType.INT, "2", 1),
			Token(TokType.ADDOP, "+", 1),
			Token(TokType.FLOAT, "2.3", 1),
			Token(TokType.CPAREN, ")", 1),
		)
		val stream = new SeqTokStream(tokens)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.OPAREN))),
			Vararg(Seq(Right(readExpr))),
			Match(Seq(Left(_.tok == TokType.OPAREN))),
		).isFailure)
		assert(stream.extractIf(
			Match(Seq(Left(_.tok == TokType.OPAREN))),
			Vararg(Seq(Right(readExpr))),
			Match(Seq(Left(_.tok == TokType.CPAREN))),
		).get._2.length == 2)
	}
}

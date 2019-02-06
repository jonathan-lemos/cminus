import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main extends App {
	override def main(args: Array[String]): Unit = {
		if (args.length == 0) {
			println("Specify a filename as a command-line parameter.")
			return
		}
		// Read the file as a string, split it into lines
		val lines = Try(Source.fromFile(args(0)).mkString.split("\n")) getOrElse {
			println(s"Failed to open file ${args(0)}")
			return
		}

		// Lexically analyze the lines into tokens
		val tokens = Lexer(lines)
		if (tokens.foldLeft(false)((a, t) => if (t.tok == TokType.ERROR) {
			Color.printRed(s"Error(Line ${t.line}): ")
			println("Invalid token \"" + s"${t.text}" + "\"")
			true
		} else a)) return

		// build an AST out of our token stream
		val tree = Parser(tokens)
		tree match {
			case Success(pn) => println(pn); Color.printGreen("Accept\n")
			case Failure(e: ParseException) => e.printErr(); Color.printRed("Reject\n")
			case Failure(e) => throw e
		}
	}
}

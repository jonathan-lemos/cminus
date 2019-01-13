import scala.io.Source

object Main extends App {
	private def tokenPrettyPrint(c: Iterable[Token]): Unit = {
		def pad(s: Any, n: Int): String = ("%-" + n + "s").format(s.toString)
		val tokLen: Int = c.reduceLeft((t1: Token, t2: Token) => if (t1.tok.toString.length > t2.tok.toString.length) t1 else t2).tok.toString.length
		val lineLen: Int = c.reduceLeft((t1: Token, t2: Token) => if (t1.line > t2.line) t1 else t2).line.toString.length
		val textLen: Int = c.reduceLeft((t1: Token, t2: Token) => if (t1.text.length > t2.text.length) t1 else t2).text.length
		c.foreach(s => println(pad(s.line, lineLen) + ":" + pad(s.tok, tokLen) + " " + pad(s.text, textLen)))
	}

	override def main(args: Array[String]): Unit = {
		if (args.length == 0) {
			println("Specify a filename as a command-line parameter.")
			return
		}
		// Read the file as a string, split it into lines
		val lines = Source.fromFile(args(0)).mkString.split("\n")
		// Lexically analyze the lines into tokens
		val tokens = Lexer(lines)
		// Print each token
		tokenPrettyPrint(tokens)
	}
}

import scala.io.Source

object Main extends App {
	override def main(args: Array[String]): Unit = {
		if (args.length == 0) {
			println("Specify a filename as a command-line parameter.")
			return
		}
		// Read the file as a string, split it into lines
		val lines = Source.fromFile(args(0)).mkString.split("\n")
		// Lexically analyze the lines into tokens
		val tokens = Lexer(lines, true)
	}
}

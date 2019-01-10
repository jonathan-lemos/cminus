import scala.io.Source

object Main extends App {
	override def main(args: Array[String]): Unit = {
		if (args.length == 0) {
			println("Specify a filename as a command-line parameter.")
			return
		}
		val lines = Source.fromFile(args(0)).mkString.split("\n")
		val tokens = Lexer(lines)
		tokens.foreach(println)
	}
}

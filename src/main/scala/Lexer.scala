import scala.collection.mutable.ArrayBuffer

object TokenType extends Enumeration {
	val ERROR, IDENTIFIER, KEYWORD, NUMBER, PUNCTUATION, TYPE = Value
}

case class Token(tok: TokenType.Value, text: String, line: Int) {
	override def toString: String = tok + "\t" + line + "\t" + text
}

object Lexer {
	private val keywords = Seq("else", "if", "return", "while")
	private val operators = Seq("==", "!=", "!", "=", ">", "<")
	private val punctuation = Seq("{", "}", "(", ")", ";")
	private val types = Seq("int", "void")

	private val parseRegex = ("^(" + keywords.reduce((a, v) => a + "|" + v) + "|" + types.reduce((a, v) => a + "|" + v) + "|" + punctuation.reduce((a, v) => a + "\\" + v) + "|\\w+|//|/\\*)(.*)$").r
	private val commentRegex = ".*(\\*/|/\\*)(.*)$".r

	def apply(lines: Seq[String]): Seq[Token] = {
		println(parseRegex)
		val arr = new ArrayBuffer[Token]
		var commentCtr: Int = 0
		for ((s, i) <- lines.zipWithIndex) {
			var sb = s.trim()
			while (sb != "") {
				sb match {
					case commentRegex(token, rest) if commentCtr > 0 =>
						token match {
							case "/*" => commentCtr += 1
							case "*/" => commentCtr -= 1
						}
						sb = rest
					case parseRegex(token, rest) if commentCtr == 0 =>
						sb = rest
						if (keywords.contains(token)) {
							arr += Token(TokenType.KEYWORD, token, i + 1)
						}
						else if (punctuation.contains(token)) {
							arr += Token(TokenType.PUNCTUATION, token, i + 1)
						}
						else if (types.contains(token)) {
							arr += Token(TokenType.TYPE, token, i + 1)
						}
						else if (token.matches("[A-Za-z]+")) {
							arr += Token(TokenType.IDENTIFIER, token, i + 1)
						}
						else if (token.matches("[0-9]+")) {
							arr += Token(TokenType.NUMBER, token, i + 1)
						}
						else if (token.matches("//")) {
							sb = ""
						}
						else if (token.matches("/\\*")) {
							commentCtr += 1
						}
						else {
							arr += Token(TokenType.ERROR, token, i + 1)
						}
					case _ if commentCtr > 0 =>
						sb = ""
					case _ if commentCtr == 0 =>
						arr += Token(TokenType.ERROR, sb, i + 1)
						sb = ""
				}
				sb = sb.trim()
			}
		}
		if (commentCtr > 0) {
			arr += Token(TokenType.ERROR, "Expected */", lines.length)
		}
		arr
	}
}

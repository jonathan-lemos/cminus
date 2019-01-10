import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/**
  * The types of tokens this Lexer can output.
  */
object TokenType extends Enumeration {
	val ERROR, IDENTIFIER, KEYWORD, NUMBER, OPERATOR, PUNCTUATION, TYPE = Value
}

/**
  * A data class containing a token.
  * @param tok The TokenType of this token.
  * @param text The actual text in this token.
  * @param line The line this token occurs on.
  */
case class Token(tok: TokenType.Value, text: String, line: Int) {
	override def toString: String = tok + "\t" + line + "\t" + text
}

/**
  * Lexes the raw lines.
  * The first step in compilation outside of reading the file.
  */
object Lexer {
	private val keywords = HashSet("else", "if", "return", "while")
	private val operators = Seq("==", "!=", ">=", "<=", "||", "&&", "|", "&", "<<", ">>", "!", "=", ">", "<")
	private val punctuation = HashSet("{", "}", "(", ")", ";", ",")
	private val types = HashSet("int", "void")

	private val identifierRegex = "^([A-Za-z_]+)(.*)$".r
	private val numberRegex = "^([0-9\\.]+)(.*)$".r
	private val oneLineCommentRegex = "//.*$".r
	private val blockCommentOpenRegex = "/\\*(.*)".r
	private val blockCommentCloseRegex = ".*\\*/(.*)".r

	private def standardizeRegex(s: String): String = s.replaceAll("([\\[\\]\\{\\}\\(\\)\\\\\\|\\&])", "\\\\$1")

	private def buildRegex(c: Iterable[String]): Regex = ("^(" + c.reduce((a, k) => a + "|" + standardizeRegex(k)) + ")(.*)$").r

	private val keywordRegex = buildRegex(keywords)
	private val operatorRegex = buildRegex(operators)
	private val punctuationRegex = buildRegex(punctuation)
	private val typesRegex = buildRegex(types)

	def apply(lines: Seq[String]): Seq[Token] = {
		val arr = new ArrayBuffer[Token]
		var commentCtr: Int = 0
		for ((s, i) <- lines.zipWithIndex) {
			var sb = s.trim()
			while (sb != "") {
				sb match {
					case blockCommentCloseRegex(rest) if commentCtr > 0 =>
						commentCtr -= 1
						sb = rest
					case blockCommentOpenRegex(rest) =>
						commentCtr += 1
						sb = rest
					case oneLineCommentRegex() if commentCtr == 0 =>
						sb = ""
					case keywordRegex(token, rest) if commentCtr == 0 =>
						arr += Token(TokenType.KEYWORD, token, i + 1)
						sb = rest
					case operatorRegex(token, rest) if commentCtr == 0 =>
						arr += Token(TokenType.OPERATOR, token, i + 1)
						sb = rest
					case punctuationRegex(token, rest) if commentCtr == 0 =>
						arr += Token(TokenType.PUNCTUATION, token, i + 1)
						sb = rest
					case typesRegex(token, rest) if commentCtr == 0 =>
						arr += Token(TokenType.TYPE, token, i + 1)
						sb = rest
					case identifierRegex(token, rest) if commentCtr == 0 =>
						arr += Token(TokenType.IDENTIFIER, token, i + 1)
						sb = rest
					case numberRegex(token, rest) if commentCtr == 0 =>
						arr += Token(TokenType.NUMBER, token, i + 1)
						sb = rest
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

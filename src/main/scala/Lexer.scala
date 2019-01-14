import scala.collection.immutable.{HashSet, SortedSet}
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.MatchError

/**
  * The types of tokens this Lexer can output.
  */
object TokenType extends Enumeration {
	val ADDOP, ASSGNOP, COMPOP, EQOP, ERROR, IDENTIFIER, KEYWORD, MULOP, NUMBER, PUNCTUATION, TYPE = Value
}

/**
  * A data class containing a token.
  * @param tok  The TokenType of this token.
  * @param text The actual text in this token.
  * @param line The line this token occurs on.
  */
case class Token(tok: TokenType.Value, text: String, line: Int) {
	override def toString: String = s"($line,$tok," + "\"" + s"$text" + "\")"
}

object Token {
	/**
	  * The list of addition operators supported by this Lexer.
	  * Precedence: 4
	  */
	val addOps      = HashSet("+", "-")
	/**
	  * The list of assignment operators supported by this Lexer.
	  * Precedence: 14
	  */
	val assgnOps    = HashSet("=")
	/**
	  * The list of comparison operators supported by this Lexer.
	  * Precedence: 6
	  *
	  * Seq is needed to match ">=" before ">"
	  */
	val compOps     = Seq(">=", "<=", ">", "<")
	/**
	  * The list of equality operators supported by this Lexer.
	  * Precedence: 7
	  */
	val eqOps       = HashSet("==", "!=")
	/**
	  * The list of keywords supported by this Lexer.
	  */
	val keywords    = HashSet("else", "if", "return", "while")
	/**
	  * The list of multiplication operators supported by this Lexer.
	  * Precedence: 3
	  */
	val mulOps      = HashSet("*", "/")
	/**
	  * The list of punctuation supported by this Lexer.
	  */
	val punctuation = HashSet("{", "}", "(", ")", ";", ",")
	/**
	  * The list of types supported by this Lexer.
	  */
	val types       = HashSet("float", "int", "void")
}


/**
  * Lexically analyzes the raw lines.
  * The first step in compilation outside of reading the file.
  */
object Lexer {
	/**
	  * Escapes special "regex" characters in a string.
	  * This will place "\" in front of []{}()\|&+*
	  * @param s The string to standardize.
	  * @return  The standardized string.
	  */
	private def standardizeRegex(s: String): String = s.replaceAll("""([\[\]\{\}\(\)\\\&\|\*\+])""", """\\$1""")

	/**
	  * Builds a regex out of an iterable.
	  * All entries in the iterable are matched.
	  * The regex will match longer entries before shorter ones in a sorted list.
	  * Group 1 - Captures anything in the iterable. This is case sensitive.
	  * Group 2 - The rest of the string.
	  * For example {"abc", "def", "()["} turns into {{{ "^(abc|def|\(\)\[)(.*)$" }}}
	  *
	  * @param c The collection of elements. This must be iterable (foreach)
	  * @return  The corresponding regex.
	  */
	private def buildRegex(c: Iterable[String]): Regex = ("^(" + c.fold("")(_ + "|" + standardizeRegex(_)).substring(1) + ")(.*)$").r

	/**
	  * Builds a bounded regex out of an iterable.
	  * The tokens in the list can only be followed by a non word character or end of line.
	  * Group 1 - Captures anything in the iterable. This is case sensitive.
	  * Group 2 - The rest of the string.
	  * For example {"abc", "def", "()["} turns into {{{ "^(abc\\b|def\\v|\(\)\[\\b)(.*)$" }}}
	  *
	  * @param c The collection of elements. This must be iterable (foreach)
	  * @return  The corresponding regex.
	  */
	private def buildRegexBounded(c: Iterable[String]): Regex = ("^(" + c.fold("")(_ + "|" + standardizeRegex(_) + "\\b").substring(1) + ")(.*)$").r

	/**
	  * The types of tokens that can be Lexed.
	  * _1 - The type of token.
	  * _2 - The regex that matches it.
	  *     Group 1 - The matched token.
	  *     Group 2 - The rest of the string.
	  */
	private val tokenClasses: Seq[(TokenType.Value, Regex)] = Seq(
		(TokenType.ADDOP, buildRegex(Token.addOps)),
		(TokenType.ASSGNOP, buildRegex(Token.assgnOps)),
		(TokenType.COMPOP, buildRegex(Token.compOps)),
		(TokenType.EQOP, buildRegex(Token.eqOps)),
		(TokenType.KEYWORD, buildRegexBounded(Token.keywords)),
		(TokenType.PUNCTUATION, buildRegex(Token.punctuation)),
		(TokenType.MULOP, buildRegex(Token.mulOps)),
		(TokenType.TYPE, buildRegex(Token.types)),

		/**
		  * Matches identifiers, which are strings of upper/lower case letters with optional underscores.
		  *
		  * {{{
		  * [A-Za-z_]          - Matches an upper/lower case letter or underscore (identifier character)
		  * [A-Za-z_]+         - Matches one or more identifier characters
		  * ([A-Za-z_]+)       - Captures a group of one or more identifier characters
		  * ^([A-Za-z_]+)      - Matches 0 or more of any character at the beginning of the string.
		  * .                  - Matches any character
		  * .*                 - Matches 0 or more of any character
		  * (.*)               - Captures a group of 0 or more of any character
		  * (.*)$              - Captures a group of 0 or more of any character until the end of the string.
		  * ^([A-Za-z_]+)(.*)$ - Captures a group of identifier characters at the beginning of the string, and then captures the rest of the characters in another group.
		  * }}}
		  *
		  * Triple quotes denote a "raw" string, where backslashes show up literally instead of escaping the next character.
		  */
		(TokenType.IDENTIFIER, """^([A-Za-z_]+)(.*)$""".r),

		/**
		  * Matches numbers, which are strings of numbers with an optional period in the middle.
		  * Group 1 - An integer or a float literal.
		  * Group 2 - The rest of the string
		  *
		  * {{{
		  * [0-9]                         - Matches a numeric character.
		  * [0-9]+                        - Matches one or more numeric characters.
		  * \.                            - Matches a literal period (. matches any character, "\\" is a literal backslash that escapes it).
		  * \.?                           - Matches 0 or 1 literal periods.
		  * [0-9]+\.?                     - Matches one or more numeric characters followed by an optional literal period.
		  * [0-9]+\.?[0-9]+               - Matches one or more numeric characters followed by an optional literal period followed by one or more numeric characters.
		  * |                             - Regex OR. Has the lowest precedence of all regex operators.
		  * [0-9]+\.?[0-9]+|[0-9]         - Matches either a number followed by an optional period followed by a number, or a single number.
		  *                                 The first regex will not match a single number, so a second one is needed to cover it.
		  * (.*)$                         - Captures a group of any character until the end of the string.
		  * ^([0-9]+\.?[0-9]+|[0-9])(.*)$ - Captures a number with an optional period in the middle, and in a seperate group captures the rest of the string.
		  * }}}
		  */
		(TokenType.NUMBER, """^([0-9]+\.?[0-9]+|[0-9])(.*)$""".r),
	)

	/**
	  * Matches "//".
	  * This regex does not capture anything.
	  *
	  * {{{
	  * "//"     - Matches "//" literally
	  * ".*"     - Matches any character 0 or more times.
	  * "^//.*$" - Matches "//" at any point in the string.
	  * }}}
	  */
	private val oneLineCommentRegex = """^//.*$""".r

	/**
	  * Matches "/*"
	  * Group 1 - The rest of the string.
	  *
	  * {{{
	  * "/\\*"       - Matches a "/" followed by a literal "*" ("*" without backslash means "0 or more of the aforementioned")
	  * ".*"         - Matches any character 0 or more times.
	  * "^/\\*(.*)$" - Matches a "/*" and captures the rest of the string.
	  * }}}
	  */*/*/
	private val blockCommentOpenRegex = """^/\*(.*)$""".r

	/**
	/*/*
	  * Matches "/*" or "*/" anywhere in the string
	  * Group 1 - The token matched.
	  * Group 2 - The rest of the string.
	  *
	  * {{{
	  * "\\*/"                 - Matches a "*" followed by a literal "/"
	  * "/\\*"                 - Matches a literal "/" followed by a literal "*"
	  * ".*"                   - Matches any character.
	  * ".*?"                  - Matches any character lazily (taking as few characters as needed).
	  *                          This is so it doesn't gobble the entire string if there's more than one "*/" or "/*" in the line.
	  * "(.*)"                 - Captures a group of any character.
	  *
	  * "^.*?(\\*/|/\\*)(.*)$" - Matches the first "/*" or "*/" anywhere in the string, while capturing the rest of the string after.
	  * }}}
	  */
	private val blockCommentInsideRegex = """^.*?(\*/|/\*)(.*)$""".r

	/**
	  * Lexically analyzes lines of source code.
	  * @param lines The lines of code.
	  * @return      A sequence of tokens.
	  */
	def apply(lines: Seq[String]): Seq[Token] = {
		// The sequence needs to be mutable, so we use ArrayBuffer
		val arr = new ArrayBuffer[Token]
		// How deeply nested we are in /* */ comments.
		// 0 means we are not currently in a comment
		var commentCtr: Int = 0
		// Foreach line with line number
		for ((s, i) <- lines.zipWithIndex) {
			var sb = s.trim()
			// While there is still text in sb, repeatedly extract a token from the beginning
			while (sb != "") {
				// if we're in a comment
				if (commentCtr > 0) {
					// we only need to look for open/close comments
					sb match {
						case blockCommentInsideRegex(token, rest) =>
							token match {
								case "/*" => commentCtr += 1
								case "*/" => commentCtr -= 1
							}
							sb = rest
						// if we didn't find any, skip this line
						case _ =>
							sb = ""
					}
				}
				else {
					sb match {
						// match comments first
						case blockCommentOpenRegex(rest) =>
							commentCtr += 1
							sb = rest
						case oneLineCommentRegex() =>
							// don't process the rest of the string after '//'
							sb = ""
						case _	=>
							// get the longest match, this causes "==" to outprioritize "="
							// on error, return an error token containing the first char, increase the char counter
							val mat = tokenClasses.foldLeft((TokenType.ERROR, sb.substring(0, 1), sb.substring(1)))((ta: (TokenType.Value, String, String), tk: (TokenType.Value, Regex))=> {
								sb match {
									// if this regex matches, set our return to the longer of the tokens
									case tk._2(tok, rest) => if (ta._1 != TokenType.ERROR && ta._2.length >= tok.length) ta else (tk._1, tok, rest)
									// if not, just return ta
									case _ => ta;
								}
							})
							// add this token
							arr += Token(mat._1, mat._2, i + 1)
							// set sb to the rest of the string
							sb = mat._3
					}
				}
				// remove whitespace from the resulting string
				sb = sb.trim()
			}
		}
		if (commentCtr > 0) {
			arr += Token(TokenType.ERROR, "Expected */", lines.length)
		}
		// finally, return the array
		arr
	}
}

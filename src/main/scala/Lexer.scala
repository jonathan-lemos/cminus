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
  * @param tok  The TokenType of this token.
  * @param text The actual text in this token.
  * @param line The line this token occurs on.
  */
case class Token(tok: TokenType.Value, text: String, line: Int) {
	override def toString: String = line + ":" + tok + "\t" + text
}

/**
  * Lexically analyzes the raw lines.
  * The first step in compilation outside of reading the file.
  */
object Lexer {
	/**
	  * The keywords supported. These will be returned as TokenType.KEYWORD
	  */
	private val keywords = HashSet("else", "if", "return", "while")

	/**
	  * The operators supported.
	  * These are matched from left to right (so "==" matches before "=").
	  * These will be returned as TokenType.OPERATOR
	  *
	  * Seq is needed over HashSet because order is important.
	  */
	private val operators = Seq("==", "!=", ">=", "<=", "||", "&&", "<<", ">>", "+=", "-=", "*=", "/=", "++", "--", "|", "&", "!", "=", ">", "<", "*", "/", "+", "-")

	/**
	  * The punctuation supported. These will be returned as TokenType.OPERATOR
	  */
	private val punctuation = HashSet("{", "}", "(", ")", ";", ",")

	/**
	  * The types supported. These will be returned as TokenType.TYPE
	  */
	private val types = HashSet("int", "float", "void")

	/**
	  * Matches identifiers, which are strings of upper/lower case letters with optional underscores.
	  * Group 1 - A string of [A-Za-z_] (an identifier)
	  * Group 2 - The rest of the string
	  *
	  * {{{
	  * "[A-Za-z_]"          - Matches an upper/lower case letter or underscore (identifier character)
	  * "[A-Za-z_]+"         - Matches one or more identifier characters
	  * "([A-Za-z_]+)"       - Captures a group of one or more identifier characters
	  * "^([A-Za-z_]+)"      - Matches 0 or more of any character at the beginning of the string.
	  * "."                  - Matches any character
	  * ".*"                 - Matches 0 or more of any character
	  * "(.*)"               - Captures a group of 0 or more of any character
	  * "(.*)$"              - Captures a group of 0 or more of any character until the end of the string.
	  * "^([A-Za-z_]+)(.*)$" - Captures a group of identifier characters at the beginning of the string, and then captures the rest of the characters in another group.
	  * }}}
	  */
	private val identifierRegex = "^([A-Za-z_]+)(.*)$".r

	/**
	  * Matches numbers, which are strings of numbers with an optional period in the middle.
	  * Group 1 - An integer or a float literal.
	  * Group 2 - The rest of the string
	  *
	  * {{{
	  * "[0-9]"                          - Matches a numeric character.
	  * "[0-9]+"                         - Matches one or more numeric characters.
	  * "\\."                            - Matches a literal period ("." matches any character, "\\" is a literal backslash that escapes it).
	  * "\\.?"                           - Matches 0 or 1 literal periods.
	  * "[0-9]+\\.?"                     - Matches one or more numeric characters followed by an optional literal period.
	  * "[0-9]+\\.?[0-9]+"               - Matches one or more numeric characters followed by an optional literal period followed by one or more numeric characters.
	  * "|"                              - Regex OR. Has the lowest precedence of all regex operators.
	  * "[0-9]+\\.?[0-9]+|[0-9]          - Matches either a number followed by an optional period followed by a number, or a single number.
	  *                                    The first regex will not match a single number, so a second one is needed to cover it.
	  * "(.*)$"                          - Captures a group of any character until the end of the string.
	  * "^([0-9]+\\.?[0-9]+|[0-9])(.*)$" - Captures a number with an optional period in the middle, and in a seperate group captures the rest of the string.
	  * }}}
	  */
	private val numberRegex = "^([0-9]+\\.?[0-9]+|[0-9])(.*)$".r

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
	private val oneLineCommentRegex = "^//.*$".r

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
	private val blockCommentOpenRegex = "^/\\*(.*)$".r

	/**
	 /*/*/*
	  * Matches "*/" anywhere in the string
	  * Group 1 - The rest of the string.
	  *
	  * {{{
	  * "\\*/"          - Matches a "*" followed by a literal "/"
	  * ".*"            - Matches any character.
	  * "(.*)"          - Captures a group of any character.
	  *
	  * "^.*\\* /(.*)$" - Matches a "*/" anywhere in the string, while capturing the rest of the string after.
	  * }}}
	  */
	private val blockCommentCloseRegex = "^.*\\*/(.*)$".r

	/**
	  * Escapes special "regex" characters in a string.
	  * This will place "\" in front of []{}()\|&+*
	  * @param s The string to standardize.
	  * @return  The standardized string.
	  */
	private def standardizeRegex(s: String): String = s.replaceAll("([\\[\\]\\{\\}\\(\\)\\\\\\|\\&\\*\\+])", "\\\\$1")

	/**
	  * Builds a regex out of an iterable.
	  * Group 1 - Captures anything in the iterable. This is case sensitive.
	  * Group 2 - The rest of the string.
	  * For example {"abc", "def", "()["} turns into {{{ "^(abc|def|\(\)\[)(.*)$" }}}
	  *
	  * @param c The collection of elements. This must be iterable (foreach)
	  * @return  The corresponding regex.
	  */
	private def buildRegex(c: Iterable[String]): Regex = ("^(" + c.reduce((a, k) => a + "|" + standardizeRegex(k)) + ")(.*)$").r

	/**
	  * Matches keywords ("if", "return", etc.)
	  * Group 1 - The keyword
	  * Group 2 - The rest of the string.
	  */
	private val keywordRegex = buildRegex(keywords)

	/**
	  * Matches operators ("==", ">>", etc.)
	  * Group 1 - The operator.
	  * Group 2 - The rest of the string.
	  */
	private val operatorRegex = buildRegex(operators)

	/**
	  * Matches punctuation ("{", "[", etc.)
	  * Group 1 - The punctuation.
	  * Group 2 - The rest of the string.
	  */
	private val punctuationRegex = buildRegex(punctuation)

	/**
	  * Matches type keywords ("int", "void", etc.)
	  * Group 1 - The keyword.
	  * Group 2 - The rest of the string.
	  */
	private val typesRegex = buildRegex(types)

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
			// Remove whitespace from beginning of line and assign it to sb
			var sb = s.trim()
			// While there is still text in sb
			while (sb != "") {
				// Try to match a token. If we match, remove that token from the string.
				sb match {
					// If we are in a comment, attempt to match an ending comment (*/) on this line
					case blockCommentCloseRegex(rest) if commentCtr > 0 =>
						// If we match, reduce the comment level
						commentCtr -= 1
						// Set sb to the rest of the string
						sb = rest
					// Attempt to match an open comment (/*)
					case blockCommentOpenRegex(rest) =>
						// If we match, increase the comment counter
						commentCtr += 1
						// Set sb to the rest of the string
						sb = rest
					// Attempt to match a one-line comment (//) if we are not currently in a comment
					case oneLineCommentRegex() if commentCtr == 0 =>
						// If we match, do not attempt to process the rest of the line
						sb = ""
					// Attempt to match a keyword if we are not in a comment
					case keywordRegex(token, rest) if commentCtr == 0 =>
						// If we match, add this keyword to the list
						arr += Token(TokenType.KEYWORD, token, i + 1)
						// Set sb to the rest of the string
						sb = rest
					// Attempt to match an operator if we are not in a comment
					case operatorRegex(token, rest) if commentCtr == 0 =>
						// If we match, add this operator to the list
						arr += Token(TokenType.OPERATOR, token, i + 1)
						// Set sb to the rest of the string
						sb = rest
					// Attempt to match punctuation if we are not in a comment
					case punctuationRegex(token, rest) if commentCtr == 0 =>
						// If we match, add this operator to the list
						arr += Token(TokenType.PUNCTUATION, token, i + 1)
						// Set sb to the rest of the string
						sb = rest
					// Attempt to match a type if we are not in a comment
					case typesRegex(token, rest) if commentCtr == 0 =>
						// If we match, add this type to the list
						arr += Token(TokenType.TYPE, token, i + 1)
						// Set sb to the rest of the string
						sb = rest
					// Attempt to match an identifier if we are not in a comment
					// This must be after other matchers because otherwise it will match keywords
					case identifierRegex(token, rest) if commentCtr == 0 =>
						// If we match, add this identifier to the list
						arr += Token(TokenType.IDENTIFIER, token, i + 1)
						// Set sb to the rest of the string
						sb = rest
					// Attempt to match a number if we are not in a comment
					case numberRegex(token, rest) if commentCtr == 0 =>
						// If we match, add this number to the list
						arr += Token(TokenType.NUMBER, token, i + 1)
						// Set sb to the rest of the string
						sb = rest
					// If we are in a comment and we didn't match
					case _ if commentCtr > 0 =>
						// Do not process the rest of the string
						sb = ""
					// If we are NOT in a comment and we didn't match
					case _ if commentCtr == 0 =>
						// TODO: make this skip to the next identifier instead of using this hack

						// This is a syntax error. Add the error to the string if the last token was not an error.
						if (arr(arr.length - 1).line == i + 1 && arr(arr.length - 1).tok != TokenType.ERROR) {
							arr += Token(TokenType.ERROR, sb, i + 1)
						}
						// Remove one character from the front of the string.
						sb = sb.substring(1);
				}
				// Remove whitespace from the front of the new string
				sb = sb.trim()
			}
		}
		// If we are still in a comment level
		if (commentCtr > 0) {
			// Report an error
			arr += Token(TokenType.ERROR, "Expected */", lines.length)
		}
		// Return the array
		arr
	}
}

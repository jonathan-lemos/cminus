import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/**
  * An enum representing the types of tokens this Lexer can handle.
  */
object TokType extends Enumeration {
	val ADDOP, ASSGNOP, CBRACE, CBRACKET, COMMA, CPAREN, CHUCK_E_CHEESE, ERROR, FLOAT, IDENTIFIER, INT, KEYWORD, MULOP, OBRACE, OBRACKET, OPAREN, RELOP, SEMICOLON, TYPE = Value
}

/**
  * An immutable data class containing a token.
  * @param tok  The TokType of this token.
  * @param text The actual text in this token.
  * @param line The line this token occurs on.
  */
case class Token(tok: TokType.Value, text: String, line: Int) {
	override def toString: String = s"($line,$tok," + "\"" + s"$text" + "\")"
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
	  * All entries in the iterable are matched. Precedence is from left to right.
	  * Group 1 - Captures anything in the iterable. This is case sensitive.
	  * Group 2 - The rest of the string.
	  * For example {"abc", "def", "()["} turns into {{{ ^(abc|def|\(\)\[)(.*)$ }}}
	  *
	  * @param c The collection of elements. This must be iterable (foreach)
	  * @return  The corresponding regex.
	  */
	private def buildRegex(c: String*): Regex = ("^(" + c.fold("")(_ + "|" + standardizeRegex(_)).substring(1) + ")(.*)$").r

	/**
	  * Builds a bounded regex out of an iterable.
	  * The tokens in the list can only be followed by a non word character or end of line.
	  * Group 1 - Captures anything in the iterable. This is case sensitive.
	  * Group 2 - The rest of the string.
	  * For example {"abc", "def", "()["} turns into {{{ ^(abc\b|def\b|\(\)\[\b)(.*)$" }}}
	  *
	  * @param c The collection of elements. This must be iterable (foreach)
	  * @return  The corresponding regex.
	  */
	private def buildRegexBounded(c: String*): Regex = ("^(" + c.fold("")(_ + "|" + standardizeRegex(_) + "\\b").substring(1) + ")(.*)$").r

	/**
	  * Matches floats.
	  * Matches: "2.0", "0.2", "-2.0", "+2.0", "-2.0e+14", "-02.040E-14", "2e14"
	  * Does not match: ".2", "2.", "2.4.3", "23", "0..2"
	  * Group 1 - A float literal.
	  * Group 2 - The rest of the string
	  *
	  * {{{
	  * [-+]                                             - Matches a plus or a minus.
	  * [-+]?                                            - Matches an optional plus or minus.
	  * \d                                               - Matches a numeric character (same as [0-9]).
	  * \d+                                              - Matches one or more numeric characters.
	  * (?:...)                                          - Non-capturing group. Groups regex without capturing what's inside.
	  * \.                                               - Matches a literal period (. matches any character, "\" is a literal backslash that escapes it).
	  * (?:\.\d+)                                        - Matches a literal period followed by one or more digits.
	  * (?:\.\d+)?                                       - Optionally matches a literal period followed by one or more digits.
	  * [eE]                                             - Matches "e" or "E"
	  * [eE][-+]?\d+                                     - Matches "e" or "E" followed by an optional plus or minus followed by one or more numeric characters (scientific notation).
	  * |                                                - Regex OR
	  * (?:(?:\.\d+)?[eE][-+]?\d+|\.\d+))                - Matches either an optional period + one or more digits followed by scientific notation or a required period followed by one or more digits.
	  *                                                    In other words the period and following digits are only optional if scientific notation is used.
	  * (.*)                                             - Captures any character 0 or more times
	  * ^([-+]?\d+(?:(?:\.\d+)?[eE][-+]?\d+|\.\d+))(.*)$ - Captures an optional plus or minus followed by digits with a period in the middle, or if scientific notation is used, the period is optional. The next group captures the rest of the string.
	  *
	  * }}}
	  */
	private val floatRegex = """^([-+]?\d+(?:(?:\.\d+)?[eE][-+]?\d+|\.\d+))(.*)$""".r

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
	private val identifierRegex = """^([A-Za-z]+)(.*)$""".r

	/**
	  * Matches integers, which are strings of digits.
	  * Group 1 - An integer literal.
	  * Group 2 - The rest of the string
	  *
	  * {{{
	  * -                - Matches "-" literally
	  * -?               - Optionally matches a literal "-"
	  * [0-9]            - Matches a numeric digit
	  * [0-9]+           - Matches one or more numeric digits
	  * ([0-9]+)         - Captures one or more numeric digits
	  * .                - Matches any character
	  * .*               - Matches 0 or more of any character
	  * (.*)             - Captures 0 or more of every character.
	  * ^(-?[0-9]+)(.*)$ - Captures an optional minus followed by 1 or more digits followed by another capture of the rest of the string.
	  * }}}
	  */
	private val intRegex = """^(-?[0-9]+)(.*)$""".r

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
	  * The types of tokens that can be Lexed.
	  * _1 - The type of token.
	  * _2 - The regex that matches it.
	  *     Group 1 - The matched token.
	  *     Group 2 - The rest of the string.
	  * Earlier groups are matched before later ones.
	  */
	private val tokenClasses: Seq[(TokType.Value, Regex)] = Seq(
		(TokType.ADDOP     , buildRegex("+", "-")),
		(TokType.ASSGNOP   , buildRegex("=")),
		(TokType.CBRACE    , buildRegex("}")),
		(TokType.CBRACKET  , buildRegex("]")),
		(TokType.COMMA     , buildRegex(",")),
		(TokType.CPAREN    , buildRegex(")")),
		(TokType.FLOAT     , floatRegex),
		(TokType.INT       , intRegex),
		(TokType.KEYWORD   , buildRegexBounded("else", "if", "return", "while")),
		(TokType.MULOP     , buildRegex("*", "/")),
		(TokType.OBRACE    , buildRegex("{")),
		(TokType.OBRACKET  , buildRegex("[")),
		(TokType.OPAREN    , buildRegex("(")),
		(TokType.RELOP     , buildRegex(">=", "<=", "==", "!=", ">", "<")),
		(TokType.SEMICOLON , buildRegex(";")),
		(TokType.TYPE      , buildRegexBounded("float", "int", "void")),
		(TokType.IDENTIFIER, identifierRegex),
	)

	/**
	  * Lexically analyzes lines of source code.
	  * @param lines The lines of code.
	  * @return      A sequence of tokens.
	  */
	def apply(lines: Seq[String], debugOutput: Boolean = false): Seq[Token] = {
		// The sequence needs to be mutable, so we use ArrayBuffer
		val arr = new ArrayBuffer[Token]
		// How deeply nested we are in /* */ comments.
		// 0 means we are not currently in a comment
		var commentCtr: Int = 0

		/**
		  * Handles comments in the string if applicable.
		  *
		  * @param sb The string to process.
		  * @return An option containing Some(rest_of_string) or None() (if comments were not handled)
		  */
		def handleComment(sb: String): Option[String] = {
			// If we are in a comment.
			if (commentCtr > 0) sb match {
				// Use the "blockCommentInsideRegex, which matches "/*" or "*/" anywhere in this line."
				case blockCommentInsideRegex(token, rest) =>
					token match {
						case "/*" => commentCtr += 1
						case "*/" => commentCtr -= 1
					}
					Some(rest)
				case _ => Some("")
			}
			// If we are not in a comment.
			else sb match {
				// Attempt to match "/*" at the beginning of this string.
				case blockCommentOpenRegex(rest) =>
					commentCtr += 1
					Some(rest)
				// Attempt to match "//" at the beginning of this string, ignore the rest of the line if so.
				case oneLineCommentRegex() => Some("")
				case _ => None
			}
		}

		/**
		  * Extracts a token from the string, returning the token and the rest of the string.
		  * @param sb The input string.
		  * @param line The line number.
		  * @return A tuple containing the extracted token and the rest of the string.
		  * If none could be matched, it returns a token of type TokType.ERROR that extracts a single character, returning the rest of the string.
		  */
		def getToken(sb: String, line: Int): (Token, String) = tokenClasses.foldLeft((Token(TokType.ERROR, sb.substring(0, 1), line), sb.substring(1)))((ta: (Token, String), tk: (TokType.Value, Regex)) => {
			sb match {
				// if this regex matches, set our return to the longer of the tokens
				case tk._2(tok, rest) => if (ta._1.tok != TokType.ERROR && ta._1.text.length >= tok.length) ta else (Token(tk._1, tok, line), rest)
				// if not, just return ta
				case _ => ta
			}
		})

		if (debugOutput) { Color.printGreen("Starting Lexer"); println }
		// Foreach line with line number. The main loop
		for ((s, i) <- lines.zipWithIndex) {
			if (debugOutput) { Color.printBlue(s"Input (Line ${i + 1}): " + "\"" + s"$s" + "\""); println }

			var sb = s.trim()
			// While there is still text in sb, repeatedly extract a token from the beginning
			while (sb != "") {
				handleComment(sb) match {
					case Some(rest) => sb = rest
					case None =>
						val tok = getToken(sb, i + 1)
						arr += tok._1
						sb = tok._2
						if (debugOutput) println(tok._1.toString)
				}
				// Remove whitespace.
				sb = sb.trim()
			}
		}
		if (commentCtr > 0) {
			arr += Token(TokType.ERROR, "Expected */", lines.length)
		}
		// finally, return the array
		arr
	}
}

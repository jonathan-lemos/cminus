import org.scalatest.FunSuite

class SemAnalyzerIntegrationTest extends FunSuite {
	test("SemAnalyzer.BasicSuccess") {
		val lines = Seq(
			"int main(void) {",
			"   return 0;",
			"}"
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
	}

	test("SemAnalyzer.BasicFail") {
		val lines = Seq(
			"int main(void) {",
			"   return x;",
			"}"
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
	}
}

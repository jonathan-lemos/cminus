import org.scalatest.FunSuite

import scala.util.Failure

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

	test("SemAnalyzer.DeclaredSuccess") {
		val lines = Seq(
			"int main(void) {",
			"   int x = 4;",
			"   return x;",
			"}"
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
	}

	test("SemAnalyzer.DeclaredFail1") {
		val lines = Seq(
			"void main(void) {",
			"   int x = 4;",
			"   return x;",
			"}"
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
	}

	test("SemAnalyzer.DeclaredFail2") {
		val lines = Seq(
			"int main(void) {",
			"   int x = 4;",
			"   return y;",
			"}"
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		analysis match {case Failure(e: SemAnalyzerException) => e.prettyPrint(); case _ =>}
	}
}

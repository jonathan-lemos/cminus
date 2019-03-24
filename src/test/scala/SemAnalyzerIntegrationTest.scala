import org.scalatest.FunSuite

import scala.util.{Failure, Try}

class SemAnalyzerIntegrationTest extends FunSuite {
	def prettyPrint(res: Try[Unit]): Unit	= res match {case Failure(_: SemAnalyzerException) => /*e.prettyPrint()*/; case _ =>}
	def mkLines(s: String): Seq[String] = s.trim.split("\n").map(_.trim)

	test("SemAnalyzer.ReturnTypeSuccess") {
		val lines = mkLines("""
int x(void) { return 4; }
void y(void) { return; }
void z(void) { }
float main(void) {return 4.0E-13; }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ReturnFailure1") {
		val lines = mkLines("""
float main(void) {return 4; }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ReturnFailure2") {
		val lines = mkLines("""
int main(void) {return 4.0; }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ReturnFailure3") {
		val lines = mkLines("""
int main(void) {return; }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ReturnFailure4") {
		val lines = mkLines("""
void main(void) { return 4; }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamSuccess") {
		val lines = mkLines("""
int v(void) { return 3; }
int x(int a, int b, int c) { return a + b + c; }
float y(int x, float y) { return y; }
int main(void) { y(1, 2.0); return x(1, 2, v()); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail1") {
		val lines = mkLines("""
int x(void) { return 0; }
int main(void) { return x(1); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail2") {
		val lines = mkLines("""
void y(void) { }
int x(void) { return 0; }
int main(void) { return x(y()); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail3") {
		val lines = mkLines("""
int x(int a) { return 0; }
int main(void) { return x(1.0); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail4") {
		val lines = mkLines("""
int x(int a) { return 0; }
int main(void) { return x(); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail5") {
		val lines = mkLines("""
int x(int a) { return 0; }
int main(void) { return x(1, 1); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail6") {
		val lines = mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1, 1); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail7") {
		val lines = mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1.0, 1); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail8") {
		val lines = mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1, 1.0, 2); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail9") {
		val lines = mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ArithmeticSuccess") {
		val lines = mkLines("""
int main(void) {
	float x = 2.0 + 3.0e4 * (5.0e-2 / 4.0);
	int y = 2 + 3 / (4 + 5);
	return y;
}
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ArithmeticFailure1") {
		val lines = mkLines("""
void x(void) {}
int main(void) { return 2 + x(); }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ArithmeticFailure2") {
		val lines = mkLines("""
int main(void) { return 2 + 2.0; }
		""")

		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}
}

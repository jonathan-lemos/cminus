import org.scalatest.WordSpec

import scala.util.{Failure, Success, Try}

class SemAnalyzerIntegrationTest extends WordSpec {
	def prettyPrint(res: Try[Unit]): Unit	= res match {case Failure(_: SemAnalyzerException) => /*e.prettyPrint()*/; case _ =>}
	def mkLines(s: String): Seq[String] = s.trim.split("\n").map(_.trim)

	val successCases: Seq[(String, Seq[String])] = Seq(
		("ReturnSuccess", mkLines("""
int x(void) { return 4; }
void y(void) { return; }
void z(void) { }
float main(void) {return 4.0E-13; }
		""")),
		("ParamSuccess", mkLines("""
int v(void) { return 3; }
int x(int a, int b, int c) { return a + b + c; }
float y(int x, float y) { return y; }
int main(void) { y(1, 2.0); return x(1, 2, v()); }
		""")),
		("ArithmeticSuccess", mkLines("""
int main(void) {
	float x;
	int y;
	x = 2.0 + 3.0e4 * (5.0e-2 / 4.0);
	y = 2 + 3 / (4 + 5);
	return y;
}
		""")),
	)

	val failCases: Seq[(String, Seq[String])] = Seq(
		("Cannot return int from float fn", mkLines("""
float main(void) {return 4; }
		""")),
		("Cannot return float from int fn", mkLines("""
int main(void) {return 4.0; }
		""")),
		("Cannot return void from int fn", mkLines("""
int main(void) {return; }
		""")),
		("Cannot return nothing from int fn", mkLines("""
int main(void) { }
		""")),
		("Cannot return int from void fn", mkLines("""
void main(void) { return 4; }
		""")),
		("Cannot pass parameter to void param", mkLines("""
int x(void) { return 0; }
int main(void) { return x(1); }
		""")),
		("Cannot pass void fn return to void param list", mkLines("""
void y(void) { }
int x(void) { return 0; }
int main(void) { return x(y()); }
		""")),
		("Cannot pass float to int param 1", mkLines("""
int x(int a) { return 0; }
int main(void) { return x(1.0); }
		""")),
		("Cannot pass 0 params to 1 param fn", mkLines("""
int x(int a) { return 0; }
int main(void) { return x(); }
		""")),
		("Cannot pass 2 params to 1 param fn", mkLines("""
int x(int a) { return 0; }
int main(void) { return x(1, 1); }
		""")),
		("Cannot pass int to float param", mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1, 1); }
		""")),
		("Cannot pass float to int param 2", mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1.0, 1); }
		""")),
		("Cannot pass 3 params to 2 param fn", mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1, 1.0, 2); }
		""")),
		("Cannot pass 1 param to 2 param fn", mkLines("""
int x(int a, float b) { return 0; }
int main(void) { return x(1); }
		""")),
		("Cannot use type void in expression", mkLines("""
void x(void) {}
int main(void) { return 2 + x(); }
		""")),
		("Cannot mix int and float in expression", mkLines("""
int main(void) { return 2 + 2.0; }
		""")),
		("Cannot assign int to float", mkLines("""
int main(void) { float x; x = 2; return 0; }
		""")),
		("Cannot assign float to int", mkLines("""
int main(void) { int x; x = 4.0; return 0; }
		""")),
		("Cannot instantiate var of type void", mkLines("""
int main(void) { void x; return 0; }
		""")),
	)

	"Success Tests" should {
		successCases foreach testSuccess
	}

	"Failure Tests" should {
		failCases foreach testFailure
	}

	def testSuccess(lines: (String, Seq[String])): Unit = {
		s"'${lines._1}' succeeds" in {
			val tree = Parser(Lexer(lines._2)) match {
				case Success(p) => p
				case Failure(e: ParseException) => e.printErr(); fail("Failed to parse")
				case Failure(_) => fail("Unknown error")
			}
			val analysis = SemAnalyzer(tree)
			assert(analysis.isSuccess)
		}
	}

	def testFailure(lines: (String, Seq[String])): Unit = {
		s"'${lines._1}' fails" in {
			val tree = Parser(Lexer(lines._2)) match {
				case Success(p) => p
				case Failure(e: ParseException) => e.printErr(); fail("Failed to parse")
				case Failure(_) => fail("Unknown error")
			}
			val analysis = SemAnalyzer(tree)
			assert(analysis.isFailure)
		}
	}
}

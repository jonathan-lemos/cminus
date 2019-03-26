import org.scalatest.WordSpec

import scala.util.{Failure, Success, Try}

class SemAnalyzerIntegrationTest extends WordSpec {
	def prettyPrint(res: Try[Unit]): Unit	= res match {case Failure(e: SemAnalyzerException) => e.prettyPrint(); case _ =>}
	def mkLines(s: String): Seq[String] = s.trim.split("\n").map(_.trim)

	val successCases: Seq[(String, Seq[String])] = Seq(
		("ReturnSuccess", mkLines("""
int x(void) { return 4; return 8; }
void y(void) { return; return; }
void z(void) { }
float aa(void) {return 4.0E-13; return 4.0; }
int main(void) { return 0; }
		""")),
		("ParamSuccess", mkLines("""
int v(void) { return 3; }
int x(int a, int b, int c) { return a + b + c; }
float y(int x, float y) { return y; }
int main(void) { v(); y(1, 2.0); return x(1, 2, v()); }
		""")),
		("ArithmeticSuccess", mkLines("""
float x(void) { return 4.0; }
int y(int x) { return x * 2; }
int main(void) {
	float x;
	int y;
	x = 2.0 + 3.0e4 * (5.0e-2 / (4.0 + x()));
	y = 2 + 3 / (4 + 5) / y(4);
	return y;
}
		""")),
		("ArraySuccess", mkLines("""
int x(int q) { return q; }
int y(int q[]) { return q[0]; }
int main(void) {
	int x[4];
    x[0] = 4;
	x[1 * (1 + 4)] = 7;
    x[main() + 9] = 18 + 4;
	return x[main() + 9] + x(x[1]) + y(x);
}
		""")),
		("ScopeSuccess", mkLines("""
int x;
int x(int q) { {int q = q; return x + q + x(x);} }
int main(void) {
	int q;
    int x;
	if (x(x) == x(q)) {
        int x = x(x);
		return x + q;
    }
	else if (1.0 == 1.0) {
		return x + q;
    }
	return x + x + q + q + x(q);
}
		""")),
	)

	val failCases: Seq[(String, Seq[String])] = Seq(
		("Cannot return int from float fn", mkLines("""
float x(void) {return 4; }
int main(void) { return 0; }
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
		("Array index assgn cannot be float", mkLines("""
int main(void) { int x[4]; x[2.0] = 4; return 0; }
		""")),
		("Array index assgn cannot be void", mkLines("""
void x(void) {}
int main(void) { int x[4]; x[x()] = 4; return 0; }
		""")),
		("Cannot index non-array type", mkLines("""
int main(void) { int x; return x[0]; }
		""")),
		("Cannot pass int array to int", mkLines("""
void z(int y) {}
int main(void) { int x[4]; z(x); return 0; }
		""")),
		("Cannot pass int to int array", mkLines("""
void z(int y[]) {}
int main(void) { int x; z(x); return 0; }
		""")),
		("Cannot pass int array index to int array", mkLines("""
void z(int y[]) {}
int main(void) { int x[4]; z(x[0]); return 0; }
		""")),
		("Cannot double declare var at global scope", mkLines("""
int x; int x;
int main(void) { return 0; }
		""")),
		("Cannot double declare function", mkLines("""
void x(void) {}
int x(void) {return 0;}
int main(void) { return 0; }
		""")),
		("Cannot double declare at function scope", mkLines("""
int main(void) { int x; int x; return x; }
		""")),
		("Cannot double declare param in function root level", mkLines("""
int x(int y) { int y; return y; }
int main(void) { return 0; }
		""")),
		("Cannot use undeclared var", mkLines("""
int x(int y) { return y; }
int main(void) { return x; }
		""")),
		("Cannot use undeclared func", mkLines("""
int x;
int main(void) { return x(); }
		""")),
		("Cannot compare int and float 1", mkLines("""
int main(void) { if (1 == 1.0) return 0; }
		""")),
		("Cannot compare int and float 2", mkLines("""
int main(void) { if (1.0 > 1) return 0; }
		""")),
		("Cannot compare void with void", mkLines("""
void x(void) {}
int main(void) { if (x() != x()) return 0; }
		""")),
		("Cannot compare void with float", mkLines("""
void x(void) {}
int main(void) { if (x() < 1.0) return 0; }
		""")),
		("Cannot mix float and int in add expr", mkLines("""
int main(void) { 2.0 + 2; return 0; }
		""")),
		("Cannot mix float and int in mul expr", mkLines("""
int main(void) { 2 * 2.0; return 0; }
		""")),
		("Cannot mix void and int in mul expr", mkLines("""
void x(void) {}
int main(void) { 2 * x(); return 0; }
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
			if (!analysis.isSuccess) {
				prettyPrint(analysis)
				fail("Expected success, but it failed")
			}
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

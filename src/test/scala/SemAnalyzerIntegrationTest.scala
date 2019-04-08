import org.scalatest.WordSpec
import java.io._

import scala.util.{Failure, Success, Try}

class SemAnalyzerIntegrationTest extends WordSpec {
	val EXPORT_CASES = false

	def writeFile(filename: String, contents: Seq[String]): Unit = {
		if (EXPORT_CASES) {
			val pw = new PrintWriter(new File(filename))
			contents.foreach(l => pw.println(l))
			pw.close()
		}
	}
	def prettyPrint(res: Try[Unit]): Unit	= res match {case Failure(e: SemAnalyzerException) => e.prettyPrint(); case _ =>}
	def mkLines(s: String): Seq[String] = s.trim.split("\n")

    val successCases: Seq[(String, Seq[String])] = Seq(
        ("ReturnSuccess", mkLines("""
int x(void) { return 4; return 8; }
void y(void) { return; return; }
void z(void) { }
float aa(void) { if (1 == 0) { return 4.0E-13; } }
void main(void) { main(); }
        """)),
        ("ParamSuccess", mkLines("""
int v(void) { return 3; }
int x(int a, int b, int c) { return a + b + c; }
float y(int x, float y) { return y; }
void main(void) { v(); y(1, 2.0); x(1, 2, v()); }
        """)),
        ("ArithmeticSuccess", mkLines("""
float x(void) { return 4.0; }
int y(int x) { return x * 2; }
void main(void) {
    float x;
    int y;
	int z;
	x; y; ;;;
    x = x;
	y = (x >= 2.0) + 4 + (y != 2);
    if (y > 0) x = 1.0; else y = 0;
	z = y = (y = 4) * (z = 2) * 4;
    x = 2.0 + 3.0e4 * (5.0e-2 / (4.0 + x()));
    y = 2 + 3 / (4 + 5) / y(4);
	return;
}
        """)),
        ("ArraySuccess", mkLines("""
int x(int q) { return q; }
int y(int q[]) { return q[0]; }
int z(void) { return 2; }
void main(void) {
    int x[4];
    x[0] = 4;
    x[1 * (1 + 4)] = 7;
    x[z() + 9] = 18 + 4;
    x[x[x(4)]] = x[z() + 9] + x(x[1]) + y(x);
}
        """)),
        ("ScopeSuccess", mkLines("""
float a;
int x;
int x(int q) { { int q; { q = q; { return x + q + x(x); } } } }
void main(void) {
    int q;
    int x;
	float b;
    if (x(x) == x(q)) {
		float c;
        int x;
		c = a + b;
        x = x(x);
        x + q;
    }
    else if (1.0 == 1.0) {
        x + q;
    }
    x = x + x + q + q + x(q);
}
        """)),
        ("InputOutputSuccess", mkLines("""
void main(void) { int x; x = input(); output(x); }
        """)),
        ("CrazySuccess", mkLines("""
//ACCEPT

int f(int x) {
    return x;
}

void main(void) {
    int x[1];
    x[x[x[0]]]
    =
    f(f(f(0)));
	f(x[f(x[f(x[0])])]);
}
        """)),
        ("SelectionSortSuccess", mkLines("""
/* A program to perform selection sort on a 10 element array */

int x[10];

int minloc ( int a[] , int low, int high)
{
   int i; int x; int k;
   k = low;
   x = a[low];
   i = low + 1;
   while (i < high)
   {   if (a[i] < x)
      {   x = a[i];
         k = i; }
         i = i + 1;
   }
      return k;
}

void sort ( int a[], int low, int high)
{
   int i; int k;
   low = 0;
   high = 10;
   i = low;
   while (i < high-1)
   {
      int t;
      k = minloc(a,i,high);
      t = a[k];
      a[k] = a[i];
      a[i] = t;
      i = i + 1;
   }
}

void main(void)
{
   int i;
   i = 0;
   while (i < 10)
   {
      x[i] = input();
      i = i + 1;
   }
   sort(x,0,10);
   i = 0;
   while (i < 10)
   {
      output(x[i]);
      i = i + 1;
   }
}
        """)),
    )

	val failCases: Seq[(String, Seq[String])] = Seq(
		("Cannot return int from float fn", mkLines("""
float x(void) {return 4; }
void main(void) { }
		""")),
		("Cannot return float from int fn", mkLines("""
int x(void) { return 4.0; }
void main(void) { x(); }
		""")),
		("Cannot return void from int fn", mkLines("""
int x(void) { return; }
void main(void) { x(); }
		""")),
		("Cannot return nothing from int fn", mkLines("""
int x(void) {}
void main(void) { }
		""")),
		("Cannot return int from void fn", mkLines("""
void main(void) { return 4; }
		""")),
		("Cannot return int array from int fn", mkLines("""
int x(void) { int y[1]; return y; }
void main(void) { return 4; }
		""")),
		("Cannot use type int array in condition", mkLines("""
void main(void) { int y[1]; if (y) main(); }
		""")),
		("Cannot use type float in condition", mkLines("""
void main(void) { if (2.0) main(); }
		""")),
		("Cannot pass parameter to void param", mkLines("""
int x(void) { return 0; }
void main(void) { x(1); }
		""")),
		("Cannot pass void fn return to void param list", mkLines("""
void y(void) { }
int x(void) { return 0; }
void main(void) { x(y()); }
		""")),
		("Cannot pass float to int param 1", mkLines("""
int x(int a) { return 0; }
void main(void) { x(1.0); }
		""")),
		("Cannot pass 0 params to 1 param fn", mkLines("""
int x(int a) { return 0; }
void main(void) { x(); }
		""")),
		("Cannot pass 2 params to 1 param fn", mkLines("""
int x(int a) { return 0; }
void main(void) { x(1, 1); }
		""")),
		("Cannot pass int to float param", mkLines("""
int x(int a, float b) { return 0; }
void main(void) { x(1, 1); }
		""")),
		("Cannot pass float to int param 2", mkLines("""
int x(int a, float b) { return 0; }
void main(void) { x(1.0, 1); }
		""")),
		("Cannot pass 3 params to 2 param fn", mkLines("""
int x(int a, float b) { return 0; }
void main(void) { x(1, 1.0, 2); }
		""")),
		("Cannot pass 1 param to 2 param fn", mkLines("""
int x(int a, float b) { return 0; }
void main(void) { x(1); }
		""")),
		("Cannot use type void in expression 1", mkLines("""
void x(void) {}
void main(void) { 2 + x(); }
		""")),
		("Cannot use type void in expression 2", mkLines("""
void x(void) {}
void main(void) { x() + x(); }
		""")),
		("Cannot mix int and float in expression", mkLines("""
void main(void) { 2 + 2.0; }
		""")),
		("Cannot assign int to float", mkLines("""
void main(void) { float x; x = 2; }
		""")),
		("Cannot assign float to int", mkLines("""
void main(void) { int x; x = 4.0; }
		""")),
		("Cannot assign int assgn expr to float", mkLines("""
void main(void) { float x; int y; x = (y = 4); }
		""")),
		("Cannot assign float assgn expr to int", mkLines("""
void main(void) { float x; int y; y = (x = 4.0); }
		""")),
		("Cannot assign to function 1", mkLines("""
int x(void) { return 0; }
void main(void) { x = 4; }
		""")),
		("Cannot assign to function 2", mkLines("""
int x(void) { return 0; }
int y(void) { return 0; }
void main(void) { x = y; }
		""")),
		("Cannot assign to function 3", mkLines("""
int x(void) { return 0; }
void main(void) { x = x; }
		""")),
		("Assgn chain must be same type 1", mkLines("""
void main(void) { int x; int y; float z; x = z = y; }
		""")),
		("Assgn chain must be same type 2", mkLines("""
void main(void) { int x; int y; float z; x = y = 4.0; }
		""")),
		("Cannot instantiate var of type void", mkLines("""
void main(void) { void x; }
		""")),
		("Array index assgn cannot be float", mkLines("""
void main(void) { int x[4]; x[2.0] = 4; }
		""")),
		("Array index assgn cannot be void", mkLines("""
void x(void) {}
void main(void) { int x[4]; x[x()] = 4; }
		""")),
		("Cannot index non-array type", mkLines("""
void main(void) { int x; x[0]; }
		""")),
		("Cannot pass int array to int", mkLines("""
void z(int y) {}
void main(void) { int x[4]; z(x); }
		""")),
		("Cannot pass int to int array", mkLines("""
void z(int y[]) {}
void main(void) { int x; z(x); }
		""")),
		("Cannot pass int array index to int array", mkLines("""
void z(int y[]) {}
void main(void) { int x[4]; z(x[0]); }
		""")),
		("Cannot double declare var at global scope", mkLines("""
int x; int x;
void main(void) { }
		""")),
		("Cannot double declare function 1", mkLines("""
void x(void) {}
int x(void) {return 0;}
void main(void) { }
		""")),
		("Cannot double declare function 2", mkLines("""
void x(void) {}
void x(void) {}
void main(void) { }
		""")),
		("Cannot double declare at function scope", mkLines("""
void main(void) { int x; int x; }
		""")),
		("Cannot double declare param in function root level", mkLines("""
int x(int y) { int y; return y; }
void main(void) { }
		""")),
		("Cannot use undeclared var", mkLines("""
int x(int y) { return y; }
void main(void) { x; }
		""")),
		("Cannot use undeclared func", mkLines("""
int x;
void main(void) { x(); }
		""")),
		("Cannot use var from popped scope 1", mkLines("""
void main(void) {
	if (1) {
        int x;
    }
	else {
        x = 4;
    }
}
		""")),
		("Cannot use var from popped scope 2", mkLines("""
void main(void) {
	{
        int x;
    }
	x = 4;
}
		""")),
		("Cannot use var from popped scope 3", mkLines("""
int r(void) {
	int y;
	y = 4;
    return y;
}
void main(void) {
	int z;
	z = y + 4;
}
		""")),
		("Cannot use var from popped scope 4", mkLines("""
int r(int y) {
    return y;
}
void main(void) {
	int z;
	z = y + 4;
}
		""")),
		("Cannot compare int and float 1", mkLines("""
void main(void) { if (1 == 1.0) main(); }
		""")),
		("Cannot compare int and float 2", mkLines("""
void main(void) { if (1.0 > 1) main(); }
		""")),
		("Cannot compare void with void", mkLines("""
void x(void) {}
void main(void) { if (x() != x()) main(); }
		""")),
		("Cannot compare void with float", mkLines("""
void x(void) {}
void main(void) { if (x() < 1.0) main(); }
		""")),
		("Cannot mix float and int in add expr", mkLines("""
void main(void) { 2.0 + 2; }
		""")),
		("Cannot mix float and int in mul expr", mkLines("""
void main(void) { 2 * 2.0; }
		""")),
		("Cannot mix void and int in mul expr", mkLines("""
void x(void) {}
void main(void) { 2 * x(); }
		""")),
		("Last decl must be main function 1", mkLines("""
void main(void) { }
int x;
		""")),
		("Last decl must be main function 2", mkLines("""
void main(void) { }
void v(void) {}
		""")),
		("Last decl must be main function 3", mkLines("""
void v(void) {}
		""")),
		("Main must be void main void 1", mkLines("""
int main(void) { return 0; }
		""")),
		("Main must be void main void 2", mkLines("""
void main(int argc) { }
		""")),
		("If expr cannot be void", mkLines("""
void main(void) { if (main()) return 0; }
		""")),
		("If expr cannot be float", mkLines("""
void main(void) { if (2.0) return 0; }
		""")),
	)

	"Success Tests" should {
		for ((c, i) <- successCases.zipWithIndex) {
			val s = "%02d".format(i)
			writeFile(s"$s-accept-${c._1.replace(" ", "-")}.cm", c._2)
		}
		successCases foreach testSuccess
	}

	"Failure Tests" should {
		for ((c, i) <- failCases.zipWithIndex) {
			val s = "%02d".format(i)
			writeFile(s"$s-reject-${c._1.replace(" ", "-")}.cm", c._2)
		}
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

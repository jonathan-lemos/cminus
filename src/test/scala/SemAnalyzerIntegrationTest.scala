import org.scalatest.FunSuite

import scala.util.{Failure, Try}

class SemAnalyzerIntegrationTest extends FunSuite {
	def prettyPrint(res: Try[Unit]): Unit	= res match {case Failure(e: SemAnalyzerException) => e.prettyPrint(); case _ =>}

	test("SemAnalyzer.BigSuccess") {
		val lines = Seq(
			"/*/* does some bullshit */",
			"MEGA_SYNTAX_ERROR */",
			"",
			"int MEGACONSTANT = 2;",
			"float HYPERCONSTANT = 6.28;",
			"float PI = HYPERCONSTANT * (MEGACONSTANT / 2e0 / MEGACONSTANT);",
			"",
			"int MEGAFUNCTION ( void )",
			"{",
			"",
			"	return 4; }",
			"",
			"int MEGAFUNCTIONTWO(int x,int y){if (x < 0) return x + y; else return x - y;}",
			"float a(float b) { // MEGA_SYNTAX_ERROR 2",
			"	int c = 4.0;",
			"	if (b < 0) return 0.0;",
			"	while (c * (4 + b + MEGACONSTANT * 5) <= 3e19 / (7 + a(-3e-4 + (3 / 7 + (MEGAFUNCTION() + MEGAFUNCTIONTWO(3, 4e4))))+2)) {",
			"		int d = c - MEGAFUNCTION() + MEGACONSTANT;",
			"		while(0)return 4;",
			"		if (c < 0) {",
			"			if (b < 0)",
			"				return 1;",
			"		}",
			"		else {",
			"			c = c + 2;",
			"		}",
			"	};",
			"	;;;;",
			"	return MEGACONSTANT + 3e+9;",
			"}",
			"",
			"int main(void){int x;",
			"	int y[4];",
			"	int xx = 4;",
			"	x = 4;",
			"	y[0] = 4.0e+0;",
			"	y[1] = MEGAFUNCTIONTWO(x+xx,xx+(xx*MEGAFUNCTION()));",
			"	y[2] = MEGAFUNCTION() + a(y[1]);",
			"	y[1 + (1 * MEGAFUNCTION()) - MEGAFUNCTIONTWO(0, 0.0)] = 17;",
			"	return y[0] + (y[1] + y[2]);",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
		prettyPrint(analysis)
	}

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

	test("SemAnalyzer.VarAssgnSuccess") {
		val lines = Seq(
			"int main(void) {",
			"   int x = 4;",
			"   x = 5;",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.VarAssgnFailure1") {
		val lines = Seq(
			"int main(void) {",
			"   int x = main;",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.VarAssgnFailure2") {
		val lines = Seq(
			"int main(void) {",
			"   int x = 4;",
			"   x = main;",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.VarAssgnFailure3") {
		val lines = Seq(
			"int q(void) {}",
			"int main(void) {",
			"   main = q;",
			"   return 0;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.AlreadyDeclared1") {
		val lines = Seq(
			"int x;",
			"int main(void) {",
			"   int x = 4;",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.BadReturn1") {
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

	test("SemAnalyzer.AlreadyDeclared2") {
		val lines = Seq(
			"int main(void) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   int x = 4;",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ReturnFunction") {
		val lines = Seq(
			"int q(void) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   return q;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ReturnOmit") {
		val lines = Seq(
			"int q(void) {",
			"   int x = 4;",
			"}",
			"int main(void) {",
			"   return 4;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ReturnVoid") {
		val lines = Seq(
			"void main(void) {",
            "   return 0;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail1") {
		val lines = Seq(
			"int q(void) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   int x = q;",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail2") {
		val lines = Seq(
			"int q(int z, int y) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   int x = q(1);",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail3") {
		val lines = Seq(
			"int q(int z, int y) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   int x = q(1, 2, 3);",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamFail4") {
		val lines = Seq(
			"int q(void) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   int x = q(1);",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamSucc") {
		val lines = Seq(
			"int q(void) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   int x = q();",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.ParamSucc2") {
		val lines = Seq(
			"int q(int z, int y) {",
			"   int x = 4;",
			"   return x;",
			"}",
			"int main(void) {",
			"   int x = q(1, 2);",
			"   return x;",
			"}",
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isSuccess)
		prettyPrint(analysis)
	}

	test("SemAnalyzer.VoidInExpr") {
		val lines = Seq(
			"void x(void) {}",
			"int main(void) {",
			"   return 5 + x();",
			"}"
		)
		val tree = Parser(Lexer(lines)).getOrElse(throw new IllegalArgumentException("parser is fukt"))
		val analysis = SemAnalyzer(tree)
		assert(analysis.isFailure)
		prettyPrint(analysis)
	}
}

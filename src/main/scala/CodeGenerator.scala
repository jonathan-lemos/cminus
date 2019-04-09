private case class Quadruple(op: String, par1: String = "", par2: String = "", par3: String = "")
private case class Ctx(var index: Int = 0, var id: Int = 0) {
	def genTmp: String = {
		val ret = s"_t$id"
		id += 1
		ret
	}
}

object CodeGenerator {
	def genExpression(e: Expression): (Seq[Quadruple], String, Option[String]) = {

	}

	def genIterationStatement(is: IterationStatementNode, c: Ctx): Seq[Quadruple] = {
		val (condition, tmpvar, relop) = genExpression(is.condition)
		val indexOld = c.index
		c.index += condition.length

		val op: Option[String] = relop match {
			case Some(s) =>
				c.index += 1
				Some(s match {
					case ">"  => "BRLTE"
					case ">=" => "BRLT"
					case "<"  => "BRGTE"
					case "<=" => "BRGT"
					case "==" => "BRNE"
					case "!=" => "BREQ"
				})
			case None =>
				c.index += 2
				None
		}

		c.index += 1
		val stmt = Seq(Quadruple("block")) ++ genStatement(is.statement, c) ++ Seq(Quadruple("end", "block"))
		c.index += 1
		val condops = op match {
			case Some(s) => Seq(Quadruple(s, tmpvar, "", (c.index + 1).toString))
			case None =>
				val tmp = c.genTmp
				Seq(Quadruple("COMP", tmpvar, "0", tmp), Quadruple("BREQ", tmp, "", (c.index + 1).toString))
		}
		c.index += 1
		condops ++ stmt ++ Seq(Quadruple("BR", "", "", indexOld.toString))
	}

	def genSelectionStatement(ss: SelectionStatementNode, c: Ctx): Seq[Quadruple] = {
		val (condition, tmpvar, relop) = genExpression(ss.condition)
		c.index += condition.length
		val op: Option[String] = relop match {
			case Some(s) =>
				c.index += 1
				Some(s match {
					case ">"  => "BRLTE"
					case ">=" => "BRLT"
					case "<"  => "BRGTE"
					case "<=" => "BRGT"
					case "==" => "BREQ"
					case "!=" => "BRNE"
				})
			case None =>
				c.index += 2
				None
		}
		c.index += 1
		val ifstmt = Seq(Quadruple("block")) ++ genStatement(ss.ifStatement, c) ++ Seq(Quadruple("end", "block"))
		c.index += 1
		val condops = op match {
			case Some(s) => Seq(Quadruple(s, tmpvar, "", (c.index + 1).toString))
			case None =>
				val tmp = c.genTmp
				Seq(Quadruple("COMP", tmpvar, "0", tmp), Quadruple("BREQ", tmp, "", (c.index + 1).toString))
		}
		ss.elseStatement match {
			case Some(s) =>
				c.index += 2
				val elsestmt = Seq(Quadruple("block")) ++ genStatement(s, c) ++ Seq(Quadruple("end", "block"))
				c.index += 1
				condops ++ ifstmt ++ Seq(Quadruple("BR", "", "", (c.index + 1).toString)) ++ elsestmt
			case None => condops ++ ifstmt
		}
	}

	def genStatement(s: Statement, c: Ctx): Seq[Quadruple] = {
		s match {
			case a: CompoundStatementNode => genCompoundStatement(a, c)
			case a:	SelectionStatementNode => genSelectionStatement(a, c)
		}
	}

	def genCompoundStatement(cs: CompoundStatementNode, c: Ctx): Seq[Quadruple] = {
		val localDecs = cs.vardecls.map(s => Quadruple("alloc", "r", "", s.identifier))
		c.index += localDecs.length
		val statements = cs.statements.flatMap(s => {
			val ret = genStatement(s, c)
			c.index += ret.length
			ret
		})
		localDecs ++ statements
	}

	def genFunDecl(fd: FunDeclNode, c: Ctx): Seq[Quadruple] = {
		val head = Seq(Quadruple("func", fd.identifier, fd.returnType, fd.params.length.toString))
		val params = fd.params.flatMap(s => Seq(Quadruple("param", "", "", s.identifier), Quadruple("alloc", "4", "", s.identifier)))
		c.index += head.length + params.length
		val cps = genCompoundStatement(fd.body, c)
		val tail = Seq(Quadruple("end", "func", fd.identifier))
		c.index += tail.length
		head ++ params ++ cps ++ tail
	}

	def genVarDecl(vd: VarDeclNode, c: Ctx): Seq[Quadruple] = {
		val ret: Seq[Quadruple] = vd.arrayLen match {
			case Some(i) => vd.typename match {
				case "int" => Seq(Quadruple("alloc", (i * 4).toString, "", vd.identifier))
				case "float" => Seq(Quadruple("alloc", (i * 4).toString, "", vd.identifier))
			}
			case None => Seq(Quadruple("alloc", "4", "", vd.identifier))
		}
		c.index += ret.length
		ret
	}

	def genDeclaration(d: Declaration, c: Ctx): Seq[Quadruple] = d match {
		case vd: VarDeclNode => genVarDecl(vd, c)
		case fd: FunDeclNode => genFunDecl(fd, c)
	}

	def apply(root: ProgramNode): Seq[Quadruple] = root.declarations.flatMap(s => genDeclaration(s, Ctx()))
}

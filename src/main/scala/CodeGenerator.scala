case class Quadruple(op: String, par1: String = "", par2: String = "", par3: String = "")

object CodeGenerator {
	def genExpression(e: Expression): Seq[Quadruple] = {

	}

	def genSelectionStatement(ss: SelectionStatementNode): Seq[Quadruple] = {
		val condition = genExpression(ss.condition)
	}

	def genStatement(s: Statement): Seq[Quadruple] = {
		s match {

		}
	}

	def genCompoundStatement(cs: CompoundStatementNode): Seq[Quadruple] = {
		val localDecs = cs.vardecls.map(s => Quadruple("alloc", "r", "", s.identifier))
		val statements = cs.statements.flatMap(s => genStatement(s))
		localDecs ++ statements
	}

	def genFunDecl(fd: FunDeclNode, index: Int): Seq[Quadruple] = {
		val head = Seq(Quadruple("func", fd.identifier, fd.returnType, fd.params.length.toString))
		val params = fd.params.flatMap(s => Seq(Quadruple("param", "", "", s.identifier), Quadruple("alloc", "4", "", s.identifier)))
		val tail = Seq(Quadruple("end", "func", fd.identifier))
		head ++ params ++ genCompoundStatement(fd.body) ++ tail
	}

	def genVarDecl(vd: VarDeclNode, index: Int): Seq[Quadruple] = {
		vd.arrayLen match {
			case Some(i) => vd.typename match {
				case "int" => Seq(Quadruple("alloci", i.toString, "", vd.identifier))
				case "float" => Seq(Quadruple("allocf", i.toString, "", vd.identifier))
			}
			case None => Seq(Quadruple("alloc", "4", "", vd.identifier))
		}
	}

	def genDeclaration(d: Declaration, index: Int): Seq[Quadruple] = d match {
		case vd: VarDeclNode => genVarDecl(vd, index)
		case fd: FunDeclNode => genFunDecl(fd, index)
	}

	def apply(root: ProgramNode): Seq[Quadruple] = root.declarations.flatMap(s => genDeclaration(s))
}

package backend

import frontend.ast._

object intermediateRep {

  type SymbolTable = Map[Ident, SymbolEntry]

  // As some registers are r0 and some are lr/pc
  type Register = String

  case class SymbolEntry(ty: Type, start: Int, end: Int)

  sealed trait IRNode
  //TODO: load will need to change to put the return value in r0, not just always return 0
  // push {lr} stores the return address of where the function was called from
  // pop {pc} pulls that back out so control is returned to the caller
  case class DIRECTIVE(str: String) extends IRNode{
    override def toString: String = s".${str}"
  }

  case class LABEL(label: String) extends IRNode {
    override def toString: String = s"${label}:"
  }

  case class START_FUNC() extends IRNode {
    override def toString = PUSH("lr").toString
  }
  case class EXIT_FUNC() extends IRNode {
    override def toString = POP("pc").toString
  }
  case class LOAD_ARG() extends IRNode {
    override def toString = ""
  }

  case class RETURNVALUE(exitCode: Int) extends IRNode {
    override def toString = LOAD("r0", exitCode).toString
  }

  case class RETURNREG(name: String) extends IRNode {
    override def toString = MOVE("r0", name).toString
  }

  case class MOVE(to: Register, from: Register) extends IRNode {
    override def toString = s"MOV $to, $from"
  }

  case class LOAD(num: Register, value: Int) extends IRNode {
    override def toString = s"LDR $num, =$value"
  }

  case class PUSH(num: Register) extends IRNode {
    override def toString = s"PUSH {$num}"
  }

  case class POP(num: Register) extends IRNode {
    override def toString = s"POP {$num}"
  }
}

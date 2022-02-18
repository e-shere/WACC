package backend

import frontend.ast._

object intermediateRep {

  type SymbolTable = Map[Ident, SymbolEntry]

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
    override def toString = "PUSH {lr}"
  }
  case class EXIT_FUNC() extends IRNode {
    override def toString = "POP {pc}"
  }
  case class LOAD_ARG() extends IRNode {
    override def toString = ""
  }

  case class RETURN(exitCode: Int) extends IRNode {
    override def toString = s"LDR r0, =${exitCode}"
  }

  case class MOVE() extends IRNode
  case class LOAD() extends IRNode
  case class PUSH() extends IRNode
  case class POP() extends IRNode
}

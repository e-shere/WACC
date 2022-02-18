package backend

import frontend.ast._

object intermediateRep {

  type SymbolTable = Map[Ident, SymbolEntry]

  case class SymbolEntry(ty: Type, start: Int, end: Int)

  sealed trait IRNode
  //TODO: load will need to change to put the return value in r0, not just always return 0
  // push {lr} stores the return address of where the function was called from
  // pop {pc} pulls that back out so control is returned to the caller
  case class function(name: String, body: List[IRNode]) extends IRNode {
    override def toString = name + ":\n\tPUSH {lr}\n" + body.mkString("\n") + "\tLDR r0, =0" +
      "\n\tPOP {pc}\n\t.ltorg"
  }

}

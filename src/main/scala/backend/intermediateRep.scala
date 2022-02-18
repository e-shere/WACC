package backend

import frontend.ast._

object intermediateRep {

  type SymbolTable = Map[Ident, SymbolEntry]

  case class SymbolEntry(ty: Type, start: Int, end: Int)

  sealed trait IRNode
  case class function(name: String, body: List[IRNode]) extends IRNode {
    override def toString = name + ":\n\tPUSH {lr}\n" + body.mkString("\n") + "\tLDR r0, =0" +
      "\n\tPOP {pc}\n\t.ltorg\n"
  }

}

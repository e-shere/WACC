package backend

import frontend.ast._

object intermediateRep {

  type SymbolTable = Map[Ident, SymbolEntry]

  case class SymbolEntry(ty: Type, start: Int, end: Int)

  sealed trait IRNode
  case class first() extends IRNode

}

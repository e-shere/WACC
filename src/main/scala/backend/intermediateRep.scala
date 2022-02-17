package backend

import frontend.ast._

class intermediateRep {

  type SymbolTable = Map[(ScopeId, Ident), SymbolEntry]

  type ScopeId = (Int, Int)

  case class SymbolEntry(ty: Type, start: Int, end: Int)

}

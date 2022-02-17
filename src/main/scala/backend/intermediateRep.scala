package backend

import frontend.ast._

object intermediateRep {

  val UNKNOWN = -1

  type FlatId = (ScopeId, String)

  type SymbolTable = Map[(ScopeId, String), SymbolEntry]

  type ScopeId = (Int, Int)

  case class SymbolEntry(ty: Type, start: Option[Int], end: Option[Int])

  sealed trait IRNode
  case class StartFunc() extends IRNode
  case class EndFunc() extends IRNode
  case class LoadArg(id: String) extends IRNode

}

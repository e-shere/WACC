package frontend

import frontend.ast._

object symbols {

  type TypeTree = Map[ScopeId, TypeTable]

  type ScopeId = (Int, Int)

  case class TypeTable(symbols: Map[Ident, Type], parent: TypeTable)

}

package frontend

import frontend.ast._

object symbols {

  type TypeTree = Map[ScopeId, TypeTable]

  type ScopeId = (Int, Int)

  case class TypeTable(symbols: Map[Ident, Type], parent: Option[TypeTable]) {

    def locallyContains(ident: Ident): Boolean = {
      symbols contains ident
    }

    def contains(ident: Ident): Boolean = {
      locallyContains(ident) || parentContains(ident)
    }

    def parentContains(ident: Ident): Boolean = {
      parent match {
        case None    => false
        case Some(p) => p contains ident
      }
    }

    def +(kv: (Ident, Type)): TypeTable = {
      this.copy(symbols = this.symbols + kv)
    }

    def get(ident: Ident): Option[Type] = {
      symbols get ident match {
        case None => parent.flatMap(_ get ident)
        case x    => x
      }
    }

  }

}

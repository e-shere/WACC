package frontend

import frontend.ast._

object symbols {

  case class TypeTable(symbols: Map[Ident, (Type, Int)], parent: Option[TypeTable], var counter: Int) {

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
      counter += 1
      this.copy(symbols = this.symbols + (kv._1 -> (kv._2, counter)))
    }

    def get(ident: Ident): Option[Type] = {
      symbols get ident match {
        case None => parent.flatMap(_ get ident)
        case Some(x) => Some(x._1)
      }
    }

  }

}

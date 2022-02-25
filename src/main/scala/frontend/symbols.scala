package frontend

import frontend.ast._

object symbols {

  case class TypeTable(symbols: Map[Ident, (Type, Int)], parent: Option[TypeTable], counter: Int) {

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
      this.copy(symbols = this.symbols + (kv._1 -> (kv._2, counter + 1)), counter = this.counter + 1)
    }

    def getType(ident: Ident): Option[Type] = {
      symbols get ident match {
        case None => parent.flatMap(_ getType ident)
        case Some(x) => Some(x._1)
      }
    }

    def getOffset(ident: Ident): Option[Int] = {
      symbols get ident match {
        case None => parent.flatMap(_ getOffset ident)
        case Some(x) => Some(x._2)
      }
    }

  }

}

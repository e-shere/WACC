package frontend

import frontend.ast._

object symbols {

  case class TypeTable(symbols: Map[ArrayIdent, (Type, Int)], parent: Option[TypeTable], counter: Int) {

    def locallyContains(ident: ArrayIdent): Boolean = {
      symbols contains ident
    }

    def contains(ident: ArrayIdent): Boolean = {
      locallyContains(ident) || parentContains(ident)
    }

    def parentContains(ident: ArrayIdent): Boolean = {
      parent match {
        case None    => false
        case Some(p) => p contains ident
      }
    }

    def +(kv: (ArrayIdent, Type)): TypeTable = {
      this.copy(symbols = this.symbols + (kv._1 -> (kv._2, counter)), counter = this.counter + kv._2.size)
    }

    def getType(ident: ArrayIdent): Option[Type] = {
      symbols get ident match {
        case None => parent.flatMap(_ getType ident)
        case Some(x) => Some(x._1)
      }
    }

    def getOffset(ident: ArrayIdent): Option[Int] = {
      symbols get ident match {
        case None => parent.flatMap(_ getOffset ident).map(counter + _)
        case Some(x) => Some(x._2)
      }
    }

  }
}

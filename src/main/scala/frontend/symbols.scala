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
      val declaredIdent = symbols.keys.find(_ == ident)
//      println(s"$declaredIdent")
      declaredIdent match {
        case Some(x) if leqPos(declaredIdent.get.pos, ident.pos) => Some(symbols(x)._2)
        case _ => parent.flatMap(_ getOffset ident).map(counter + _)
      }
    }

    def leqPos(p1: (Int, Int), p2: (Int, Int)): Boolean =
      p1._1 < p2._1 || (p1._1 == p1._1 && p1._2 <= p2._2)

  }
}

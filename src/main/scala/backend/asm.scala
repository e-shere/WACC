package backend

object asm {
  sealed trait Asm

  case class Directive(value: String) extends Asm {
    override def toString = "." + value
  }

  case class Label(value: String) extends Asm {
    override def toString = value + ":"
  }

  case class Push(reg: String) extends Asm
  case class Pop(reg: String) extends Asm
}

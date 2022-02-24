package backend

object asm {

  def intToAsmLit(i: Int): String = "#" + i

  sealed trait Asm

  case class Directive(value: String) extends Asm {
    override def toString = "." + value
  }

  case class Label(value: String) extends Asm {
    override def toString = value + ":"
  }

  case class Push(reg: String) extends Asm
  case class Pop(reg: String) extends Asm

  case class Or(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class And(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Eq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Neq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Leq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Lt(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Geq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Gt(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Add(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Sub(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Mul(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Div(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Mod(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Not(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
  }

  case class Neg(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
  }

  case class Len(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
  }

  case class Ord(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
  }

  case class Chr(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
  }


  case class Mov(target: String, x: String) extends Asm

  case class Malloc(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
  }

  case class Ldr(target: String, pos: String, offset: String) extends Asm {
    def this(target: String, pos: String) = this(target, pos, 0)
  }
}

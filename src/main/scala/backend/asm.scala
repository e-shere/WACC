package backend

object asm {
  sealed trait Asm

  case class Directive(value: String) extends Asm {
    override def toString = "." + value
  }

  case class Label(value: String) extends Asm {
    override def toString = value + ":"
  }

  case class Push(reg: String) extends Asm {
    override def toString = s"PUSH {$reg}"
  }

  case class Pop(reg: String) extends Asm {
    override def toString = s"POP {$reg}"
  }

  case class Or(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
    override def toString = s"ORR $target, $x, $y"
  }

  case class And(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
    override def toString = s"AND $target, $x, $y"
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
    override def toString = s"ADDS $target, $x, $y"
  }

  case class Sub(target: String, x: String, y: String) extends Asm {
    //TODO: think about overflow errors
    def this(x: String, y: String) = this(x, x, y)
    override def toString = s"SUBS $target, $x, $y"
  }

  case class Mul(target1: String, target2: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, y, x, y)
    override def toString = s"SMULL $x, $y, $x, $y"
    // TODO: include s"CMP $y, $x ASR #31\nBLNE ${label of overflow error function}"
    // result will be in register x
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

}

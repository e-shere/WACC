package backend

object asm {
  sealed trait Asm

  val SEP = "\n\t"

  case class Directive(value: String) extends Asm {
    override def toString = "." + value
  }

  case class Label(value: String) extends Asm {
    override def toString = value + ":"
  }

  // length of argRegs <= 4
  case class CallAssembly(argRegs: List[String], funcName: String) extends Asm {
    // replace with a call to to register
    override def toString = argRegs.zipWithIndex.map(x => Move(s"r${x._2}", x._1, None)).mkString(SEP) +
                              s"BL $funcName"
  }

  case class Move(target: String, dest: String, cond: Option[String]) extends Asm {
    override def toString = s"MOV${cond.getOrElse("")} $target, $dest"
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

  case class Compare(first: String, second: String) extends Asm {
    override def toString = s"CMP $first, $second"
  }

  case class Eq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Neq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
  }

  case class Leq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
    override def toString = Compare(x, y) + SEP + Move(target, "#1", Some("LE")) +
      Move(target, "#0", Some("GT"))
  }

  case class Lt(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
    override def toString = Compare(x, y) + SEP + Move(target, "#1", Some("LT")) +
      Move(target, "#0", Some("GE"))
  }

  case class Geq(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
    override def toString = Compare(x, y) + SEP + Move(target, "#1", Some("GE")) +
      Move(target, "#0", Some("LT"))
  }

  case class Gt(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
    override def toString = Compare(x, y) + SEP + Move(target, "#1", Some("GT")) +
      Move(target, "#0", Some("LE"))
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
    override def toString = CallAssembly(List(x, y), "p_check_divide_by_zero") + SEP +
      CallAssembly(List.empty, "__aeabi_idiv") + Move(target, "r0", None)
  }

  case class Mod(target: String, x: String, y: String) extends Asm {
    def this(x: String, y: String) = this(x, x, y)
    override def toString = CallAssembly(List(x, y), "p_check_divide_by_zero") + SEP +
      CallAssembly(List.empty, "__aeabi_idivmod") // + put result in correct register
  }

  case class Not(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
    override def toString = s"EOR $target, $x, #1"
  }

  case class Neg(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
    override def toString = s"RSBS $target, $x, #0"
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

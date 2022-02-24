package backend

object asm {

  def intToAsmLit(i: Int): String = "#" + i

  sealed trait Asm

  // TODO: consider separators
  // somewhere I'm giving literals as a register........
  // TODO: discuss calling assembly functions vs calling wacc functions
  // TODO: discuss passing around List[Asm] implicitly in generator

  val SEP = "\n\t"

  case class Directive(value: String) extends Asm {
    override def toString: String = "." + value
  }

  case class Label(value: String) extends Asm {
    override def toString: String = value + ":"
  }

  // length of argRegs <= 4
  case class CallAssembly(argRegs: List[String], funcName: String) extends Asm {
    // replace with a call to to register
    override def toString: String = argRegs.zipWithIndex.map(x => Move(s"r${x._2}", x._1)()).mkString(SEP) +
                              s"BL $funcName"
  }

  case class Move(target: String, dest: String)(cond: Option[String] = None) extends Asm {
    override def toString = s"MOV${cond.getOrElse("")} $target, $dest"
  }

  case class Push(reg: String) extends Asm {
    override def toString = s"PUSH {$reg}"
  }

  case class Pop(reg: String) extends Asm {
    override def toString = s"POP {$reg}"
  }

  case class Or(x: String, y: String)(target: String = x) extends Asm {
    override def toString = s"ORR $target, $x, $y"
  }

  case class And(x: String, y: String)(target: String = x) extends Asm {
    override def toString = s"AND $target, $x, $y"
  }

  case class Compare(first: String, second: String) extends Asm {
    override def toString = s"CMP $first, $second"
  }

  case class Eq(x: String, y: String)(target: String = x) extends Asm {
  }

  case class Neq(x: String, y: String)(target: String = x) extends Asm {
  }

  case class Leq(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y) + SEP + Move(target, "#1")(Some("LE")) +
      Move(target, "#0")(Some("GT"))
  }

  case class Lt(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y) + SEP + Move(target, "#1")(Some("LT")) +
      Move(target, "#0")(Some("GE"))
  }

  case class Geq(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y) + SEP + Move(target, "#1")(Some("GE")) +
      Move(target, "#0")(Some("LT"))
  }

  case class Gt(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y) + SEP + Move(target, "#1")(Some("GT")) +
      Move(target, "#0")(Some("LE"))
  }

  case class Add(x: String, y: String)(target: String = x) extends Asm {
    override def toString = s"ADDS $target, $x, $y"
  }

  case class Sub(x: String, y: String)(target: String = x) extends Asm {
    //TODO: think about overflow errors
    override def toString = s"SUBS $target, $x, $y"
  }

  case class Mul(x: String, y: String)(target1: String = x, target2: String = y) extends Asm {
    override def toString = s"SMULL $x, $y, $x, $y"
    // TODO: include s"CMP $y, $x ASR #31\nBLNE ${label of overflow error function}"
    // result will be in register x
  }

  case class Div(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = CallAssembly(List(x, y), "p_check_divide_by_zero") + SEP +
      CallAssembly(List.empty, "__aeabi_idiv") + Move(target, "r0")(None)
  }

  case class Mod(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = CallAssembly(List(x, y), "p_check_divide_by_zero") + SEP +
      CallAssembly(List.empty, "__aeabi_idivmod") //TODO + put result in correct register
  }

  case class Not(x: String)(target: String = x) extends Asm {
    override def toString = s"EOR $target, $x, #1"
  }

  case class Neg(x: String)(target: String = x) extends Asm {
    override def toString = s"RSBS $target, $x, #0"
  }

  case class Len(x: String)(target: String = x) extends Asm {
  }

  case class Ord(x: String)(target: String = x) extends Asm {
  }

  case class Chr(x: String)(target: String = x) extends Asm {
  }


  case class Mov(target: String, x: String) extends Asm

  case class Malloc(target: String, x: String) extends Asm {
    def this(x: String) = this(x, x)
  }

  case class Ldr(target: String, pos: String, offset: String) extends Asm {
    def this(target: String, pos: String) = this(target, pos, intToAsmLit(0))
  }

  case class Str(value: String, pos: String, offset: String) extends Asm {
    def this(value: String, pos: String) = this(value, pos, intToAsmLit(0))
  }
}

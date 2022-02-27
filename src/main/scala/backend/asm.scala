package backend

object asm {

  val BYTE_SIZE = 4

  def intToAsmLit(i: Int): String = "=" + i

  def countToOffset(count: Int): Int = count * BYTE_SIZE

  sealed trait Asm

  // TODO: consider separators- which file
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

  case class Branch(label: String)(cond: String = "") extends Asm {
    override def toString: String = s"B$cond $label"
  }

  // length of argRegs <= 4
  case class Call(argRegs: List[String], funcName: String) extends Asm {
    // replace with a call to to register
    override def toString: String = argRegs.zipWithIndex.map(x => Mov(s"r${x._2}", x._1)()).mkString(SEP) +
                              SEP + Branch(funcName)("L")
  }

  case class Mov(target: String, dest: String)(cond: Option[String] = None) extends Asm {
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
    override def toString: String = Compare(x, y).toString + SEP +
      Mov(target, "#1")(Some("EQ")).toString + Mov(target, intToAsmLit(0))(Some("NE")).toString
  }

  case class Neq(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y).toString + SEP +
      Mov(target, "#1")(Some("NE")).toString + Mov(target, intToAsmLit(0))(Some("EQ")).toString
  }

  case class Leq(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y).toString + SEP +
      Mov(target, "#1")(Some("LE")).toString + Mov(target, intToAsmLit(0))(Some("GT")).toString
  }

  case class Lt(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y).toString + SEP +
      Mov(target, "#1")(Some("LT")).toString + Mov(target, intToAsmLit(0))(Some("GE")).toString
  }

  case class Geq(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y).toString + SEP +
      Mov(target, "#1")(Some("GE")).toString + Mov(target, intToAsmLit(0))(Some("LT")).toString
  }

  case class Gt(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Compare(x, y).toString + SEP +
      Mov(target, "#1")(Some("GT")).toString + Mov(target, intToAsmLit(0))(Some("LE")).toString
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
    override def toString: String = Call(List(x, y), "p_check_divide_by_zero").toString +
      SEP + Call(List.empty, "__aeabi_idiv") + Mov(target, "r0")(None).toString
  }

  case class Mod(x: String, y: String)(target: String = x) extends Asm {
    override def toString: String = Call(List(x, y), "p_check_divide_by_zero").toString +
      SEP + Call(List.empty, "__aeabi_idivmod").toString
    //TODO + put result in correct register
  }

  case class Not(x: String)(target: String = x) extends Asm {
    override def toString = s"EOR $target, $x, #1"
  }

  case class Neg(x: String)(target: String = x) extends Asm {
    override def toString = s"RSBS $target, $x, #0"
  }

  case class Len(x: String)(target: String = x) extends Asm {
    override def toString: String = Ldr(target, s"[$x]")().toString
  }

  case class Ord(x: String)(target: String = x) extends Asm {
  }

  case class Chr(x: String)(target: String = x) extends Asm {
  }

  //TODO: how to ? offset here are these arguments even the right way round?
  //TODO: add intToOffset = s"#$x" ?
  case class Ldr(target: String, source: String)(offset: String = "#0") extends Asm {
    override def toString = {
      offset match {
        case "#0" => s"Ldr $target, $source"
        case _ => s"Ldr $target, [$source, $offset]"
      }
    }
  }

  //TODO: how to ? offset here
  case class Str(source: String, dest: String)(offset: String = "#0") extends Asm {
    override def toString = {
      offset match {
        case "#0" => s"Str $source, [$dest]"
        case _ => s"Str $source, [$dest, $offset]"
      }
    }
  }
}

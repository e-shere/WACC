package backend

import backend.PredefinedFunctions.check_div_zero
import step._
import generator.genCallWithRegs

object asm {

  val BYTE_SIZE = 4

  def countToOffset(count: Int): Int = count * BYTE_SIZE

  sealed trait AsmArg

  sealed trait AsmMaybeReg extends AsmArg

  sealed trait AsmDefiniteArg extends AsmArg

  case class AsmReg(r: Int) extends AsmMaybeReg with AsmDefiniteArg {
    assert(r >= 0 && r <= 15)

    override def toString = r match {
      case 12 => "ip"
      case 13 => "sp"
      case 14 => "lr"
      case 15 => "pc"
      case x  => s"r$x"
    }
  }

  case class AsmInt(i: Int) extends AsmDefiniteArg {
    override def toString = s"#$i"

    def toLdrString = s"=$i"
  }

  case class AsmAnyReg(index: Int) extends AsmMaybeReg {
    def toReg(available: Seq[AsmReg]): AsmReg = available(index)
  }

  val _0 = AsmAnyReg(0)
  val _1 = AsmAnyReg(1)

  // Handling Chars separately would produce nicer assembly but requires ugly code
  // to convert an escape character back to the escaped form
  // Maybe do this later
  // case class AsmChar(c: Char) {
  //   override def toString = s"#'$c'"
  // }

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

  case class Mov(target: AsmDefiniteArg, dest: AsmDefiniteArg, cond: Option[String] = None) extends Asm {
    def this(target: AsmDefiniteArg, dest: AsmDefiniteArg) = this(target, dest, None)
    override def toString = s"MOV${cond.getOrElse("")} $target, $dest"
  }

  case class Push(reg: AsmDefiniteArg) extends Asm {
    override def toString = s"PUSH {$reg}"
  }

  case class Pop(reg: AsmDefiniteArg) extends Asm {
    override def toString = s"POP {$reg}"
  }

  case class Or(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString = s"ORR $target, $x, $y"
  }

  case class And(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString = s"AND $target, $x, $y"
  }

  case class Compare(first: AsmReg, second: AsmDefiniteArg) extends Asm {
    override def toString = s"CMP $first, $second"
  }

  case class Eq(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString: String = new Compare(x, y).toString + SEP +
      Mov(target, AsmInt(1), Some("EQ")).toString + Mov(target, AsmInt(0), Some("NE")).toString
  }

  case class Neq(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString: String = new Compare(x, y).toString + SEP +
      Mov(target, AsmInt(1), Some("NE")).toString + Mov(target, AsmInt(0), Some("EQ")).toString
  }

  case class Leq(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString: String = new Compare(x, y).toString + SEP +
      Mov(target, AsmInt(1), Some("LE")).toString + Mov(target, AsmInt(0), Some("GT")).toString
  }

  case class Lt(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString: String = new Compare(x, y).toString + SEP +
      Mov(target, AsmInt(1), Some("LT")).toString + Mov(target, AsmInt(0), Some("GE")).toString
  }

  case class Geq(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString: String = new Compare(x, y).toString + SEP +
      Mov(target, AsmInt(1), Some("GE")).toString + Mov(target, AsmInt(0), Some("LT")).toString
  }

  case class Gt(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString: String = new Compare(x, y).toString + SEP +
      Mov(target, AsmInt(1), Some("GT")).toString + Mov(target, AsmInt(0), Some("LE")).toString
  }

  case class Add(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    override def toString = s"ADDS $target, $x, $y"
  }

  case class Sub(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
    //TODO: think about overflow errors
    override def toString = s"SUBS $target, $x, $y"
  }

  case class Mul(x: AsmDefiniteArg, y: AsmDefiniteArg)(target1: AsmDefiniteArg = x, target2: AsmDefiniteArg = y) extends Asm {
    override def toString = s"SMULL $x, $y, $x, $y"
    // TODO: include s"CMP $y, $x ASR #31\nBLNE ${label of overflow error function}"
    // result will be in register x
  }

//  case class Mul (target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends Asm {
//    override def toString = s"SMULL $target, $y, $x, $y"
//  }

  case class Not(target: AsmReg, x: AsmDefiniteArg) extends Asm {
    override def toString = s"EOR $target, $x, #1"
  }

  case class Neg(target: AsmReg, x: AsmDefiniteArg) extends Asm {
    override def toString = s"RSBS $target, $x, #0"
  }

  case class Len(target: AsmReg, x: AsmDefiniteArg) extends Asm {
    override def toString: String = x match {
      case x2: AsmReg => Ldr.step(target, x2).mkString("\n")
      case x2: AsmInt => Ldr.step(target, x2).mkString("\n")
    }
  }

  case class Ord(target: AsmReg, x: AsmDefiniteArg) extends Asm {
  }

  case class Chr(target: AsmReg, x: AsmDefiniteArg) extends Asm {
  }

  //TODO: how to ? offset here are these arguments even the right way round?
  //TODO: add intToOffset = s"#$x" ?
  case class Ldr(target: AsmReg, source: AsmDefiniteArg, offset: AsmDefiniteArg) extends Asm {
    def this(target: AsmReg, value: AsmDefiniteArg) = this(target, value, AsmInt(0))

    override def toString = {
      source match {
        case i@AsmInt(_) => s"LDR $target, ${i.toLdrString}"
        case _ => s"LDR $target, [$source, $offset]"
      }
    }
  }

  //TODO: how to ? offset here
  case class Str(source: AsmReg, dest: AsmDefiniteArg, offset: AsmDefiniteArg) extends Asm {
    def this(target: AsmReg, dest: AsmDefiniteArg) = this(target, dest, AsmInt(0))

    override def toString = {
      offset match {
        case AsmInt(0) => s"Str $source, [$dest]"
        case _ => s"Str $source, [$dest, $offset]"
      }
    }
  }

  object Mov extends OutImm with OutIn {
    def apply(target: AsmReg, dest: AsmDefiniteArg): Mov = new Mov(target, dest)
  }
  
  object Or extends OutInImm with OutInIn
  object And extends OutInImm with OutInIn
  object Eq extends OutInImm with OutInIn
  object Neq extends OutInImm with OutInIn
  object Leq extends OutInImm with OutInIn
  object Lt extends OutInImm with OutInIn
  object Geq extends OutInImm with OutInIn
  object Gt extends OutInImm with OutInIn
  object Add extends OutInImm with OutInIn
  object Sub extends OutInImm with OutInIn
  object Mul extends OutInImm with OutInIn

  object Compare extends InImm with InIn

  object Not extends OutImm with OutIn
  object Neg extends OutImm with OutIn
  object Len extends OutImm with OutIn
  object Ord extends OutImm with OutIn
  object Chr extends OutImm with OutIn

  object Ldr extends OutImm with OutIn with OutInImm with OutInIn {
    def apply(target: AsmReg, source: AsmDefiniteArg): Ldr = new Ldr(target, source)
    def apply(target: AsmReg, source: AsmReg, offset: AsmDefiniteArg): Ldr = new Ldr(target, source, offset)
  }
  object Str extends InImm with InIn with InInImm with InInIn {
    def apply(target: AsmReg, dest: AsmDefiniteArg): Str = new Str(target, dest)
    def apply(target: AsmReg, dest: AsmReg, offset: AsmDefiniteArg): Str = new Str(target, dest, offset)
  }

  object implicits {
    def implicitAsmInt(i: Int): AsmInt = AsmInt(i)
  }
}

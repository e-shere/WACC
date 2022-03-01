package backend

import backend.PredefinedFunctions.check_div_zero
import step._
import step.implicits._
import generator.genCallWithRegs

object asm {

  val BYTE_SIZE = 4

  def countToOffset(count: Int): Int = count * BYTE_SIZE

  sealed trait AsmArg

  sealed trait AsmMaybeReg extends AsmArg

  sealed trait AsmDefiniteArg extends AsmArg

  case class AsmReg(r: Int) extends AsmMaybeReg with AsmDefiniteArg {
    assert(r >= -1 && r <= 15)

    override def toString = r match {
      case -1 => "INVALID REGISTER"
      case 12 => "ip"
      case 13 => "sp"
      case 14 => "lr"
      case 15 => "pc"
      case x  => s"r$x"
    }
  }

  val NO_REG = AsmReg(-1)

  case class AsmInt(i: Int) extends AsmDefiniteArg {
    override def toString = s"#$i"

    def toLdrString = s"=$i"
  }

  //case class AsmAnyReg(index: Int) extends AsmMaybeReg {
  //  def toReg(available: Seq[AsmReg]): AsmReg = available(index)
  //}

  //def &(index: Int): AsmAnyReg = {
  //  assert(index != 0)
  //  AsmAnyReg(index)
  //}
  sealed trait AsmAnyReg extends AsmArg

  case object Re1 extends AsmAnyReg
  case object Re2 extends AsmAnyReg
  case object ReNew extends AsmAnyReg

  // Handling Chars separately would produce nicer assembly but requires ugly code
  // to convert an escape character back to the escaped form
  // Maybe do this later
  // case class AsmChar(c: Char) {
  //   override def toString = s"#'$c'"
  // }
  
  sealed trait Asm

  sealed trait AsmInstr extends Asm {
    val opcode: String
    val cond: String = ""
    def argsToString: String
    override def toString: String = s"$opcode$cond $argsToString"
  }

  sealed trait AsmStandardInstr extends AsmInstr {
    val args: Seq[AsmDefiniteArg]
    def argsToString: String = args.mkString(", ")
  }

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

  case class Branch(cond: String = "")(label: String) extends AsmInstr {
    val opcode = "B"
    def argsToString: String = "$label"
  }

  case class Mov(cond: String = "")(args: AsmDefiniteArg *) extends AsmInstr { val opcode = "MOV" }

  case class Push(cond: String = "")(arg: AsmDefiniteArg) extends AsmInstr {
    val opcode = "PUSH"
    override def argsToString = s"{$arg}"
  }

  case class Pop(cond: String = "")(arg: AsmDefiniteArg) extends AsmInstr {
    val opcode = "POP"
    override def argsToString = s"{$arg}"
  }

  case class Or(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr { val opcode = "ORR" }

  case class And(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr { val opcode = "AND" }

  case class Compare(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr { val opcode = "CMP" }

  //case class Neq(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
  //  override def toString: String = new Compare(x, y).toString + SEP +
  //    Mov(target, AsmInt(1), Some("NE")).toString + Mov(target, AsmInt(0), Some("EQ")).toString
  //}

  //case class Leq(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
  //  override def toString: String = new Compare(x, y).toString + SEP +
  //    Mov(target, AsmInt(1), Some("LE")).toString + Mov(target, AsmInt(0), Some("GT")).toString
  //}

  //case class Lt(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
  //  override def toString: String = new Compare(x, y).toString + SEP +
  //    Mov(target, AsmInt(1), Some("LT")).toString + Mov(target, AsmInt(0), Some("GE")).toString
  //}

  //case class Geq(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
  //  override def toString: String = new Compare(x, y).toString + SEP +
  //    Mov(target, AsmInt(1), Some("GE")).toString + Mov(target, AsmInt(0), Some("LT")).toString
  //}

  //case class Gt(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
  //  override def toString: String = new Compare(x, y).toString + SEP +
  //    Mov(target, AsmInt(1), Some("GT")).toString + Mov(target, AsmInt(0), Some("LE")).toString
  //}

  case class Adds(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr { val opcode = "ADDS" }

  case class Subs(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr { val opcode = "SUBS" }

  case class SMull(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr { val opcode = "SMULL" }

  //case class Mul(x: AsmDefiniteArg, y: AsmDefiniteArg)(target1: AsmDefiniteArg = x, target2: AsmDefiniteArg = y) extends AsmStandardInstr {
  //  override def toString = s"SMULL $x, $y, $x, $y"
  //  // TODO: include s"CMP $y, $x ASR #31\nBLNE ${label of overflow error function}"
  //  // result will be in register x
  //}

//  case class Mul (cond: String = "")(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends AsmStandardInstr {
//    override def toString = s"SMULL $target, $y, $x, $y"
//  }

  case class Not(cond: String = "")(target: AsmReg, x: AsmReg) extends AsmInstr {
    val opcode = "EOR"
    def argsToString = s"$target, $x, #1"
  }

  case class Neg(cond: String = "")(target: AsmReg, x: AsmReg) extends AsmInstr {
    val opcode = "RSBS"
    override def toString = s"$target, $x, #0"
  }

  case class Ord(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
    ???
  }

  case class Chr(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
    ???
  }

  //TODO: how to ? offset here are these arguments even the right way round?
  //TODO: add intToOffset = s"#$x" ?
  case class Ldr(cond: String = "")(target: AsmReg, source: AsmDefiniteArg, offset: AsmDefiniteArg) extends Asm {
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

  object Neq
  object Leq
  object Lt
  object Geq
  object Gt
  object Mul

  object Len {
    def apply(cond: String = "")(args: AsmDefiniteArg *) = args match {
      case Seq(target: AsmReg, source) => new Ldr(cond)(target, source, AsmInt(0)) 
    }
  }

  //case class Eq(cond: String = "")(args: AsmDefiniteArg *) extends AsmStandardInstr {
  //  override def toString: String = new Compare(x, y).toString + SEP +
  //    Mov(target, AsmInt(1), Some("EQ")).toString + Mov(target, AsmInt(0), Some("NE")).toString
  //}
  
  object Eq {
    def apply(cond: String = "")(args: AsmDefiniteArg *) = (
           Compare(cond)(args.tail: _*)
      <++> Mov("EQ")(args.head, AsmInt(1))
      <++> Mov("NE")(args.head, AsmInt(0))
    )
  }

  //object Ldr {
  //  def apply(target: AsmReg, source: AsmDefiniteArg): Ldr = new Ldr(target, source)
  //  def apply(target: AsmReg, source: AsmReg, offset: AsmDefiniteArg): Ldr = new Ldr(target, source, offset)
  //}
  //object Str {
  //  def apply(target: AsmReg, dest: AsmDefiniteArg): Str = new Str(target, dest)
  //  def apply(target: AsmReg, dest: AsmReg, offset: AsmDefiniteArg): Str = new Str(target, dest, offset)
  //}

  object implicits {
    def implicitAsmInt(i: Int): AsmInt = AsmInt(i)
  }
}

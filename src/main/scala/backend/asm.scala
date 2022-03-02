package backend

import backend.PredefinedFunctions.check_div_zero
import step._
import step.implicits._
import generator.genCallWithRegs

object asm {

  val BYTE_SIZE = 4

  def countToOffset(count: Int): Int = count * BYTE_SIZE

  sealed trait AsmArg {
    type Indef <: AsmIndefArg
    type Def <: AsmDefArg
    type Maybe <: AsmMaybeArg
    def makeDefinite(re2: AsmDefReg, re1: AsmDefReg, reNew: AsmDefReg): Def
  }

  sealed trait AsmMaybeArg extends AsmArg {
    override type Indef = AsmIndefArg
    override type Def = AsmDefArg
    override type Maybe = AsmMaybeArg
  }

  sealed trait AsmDefArg extends AsmMaybeArg {
    override type Indef = AsmIndefArg
    override type Def = AsmDefArg
    override type Maybe = AsmMaybeArg
  }

  sealed trait AsmIndefArg extends AsmMaybeArg {
    override type Indef = AsmIndefArg
    override type Def = AsmDefArg
    override type Maybe = AsmMaybeArg
  }

  sealed trait AsmMaybeReg extends AsmMaybeArg {
    override type Indef = AsmIndefReg
    override type Def = AsmDefReg
    override type Maybe = AsmMaybeReg
  }

  sealed trait AsmIndefReg extends AsmMaybeReg with AsmIndefArg {
    override type Indef = AsmIndefReg
    override type Def = AsmDefReg
    override type Maybe = AsmMaybeReg
  }

  case class AsmDefReg(r: Int) extends AsmMaybeReg with AsmDefArg {

    override type Indef = AsmIndefReg
    override type Def = AsmDefReg
    override type Maybe = AsmMaybeReg

    assert(r >= -1 && r <= 15)

    override def toString: String = r match {
      case -1 => "INVALID REGISTER"
      case 12 => "ip"
      case 13 => "sp"
      case 14 => "lr"
      case 15 => "pc"
      case x  => s"r$x"
    }

    def makeDefinite(re2: AsmDefReg, re1: AsmDefReg, reNew: AsmDefReg): AsmDefReg = this
  }

  val NO_REG: AsmDefReg = AsmDefReg(-1)

  case class AsmInt(i: Int) extends AsmDefArg with AsmIndefArg with AsmMaybeArg {
    override type Indef = AsmInt
    override type Def = AsmInt
    override type Maybe = AsmInt
    override def toString = s"#$i"

    def toLdrString = s"=$i"

    def makeDefinite(re2: AsmDefReg, re1: AsmDefReg, reNew: AsmDefReg): AsmInt = this
  }

  case object Re2 extends AsmIndefReg {
    def makeDefinite(re2: AsmDefReg, re1: AsmDefReg, reNew: AsmDefReg): AsmDefReg = re2
  }

  case object Re1 extends AsmIndefReg {
    def makeDefinite(re2: AsmDefReg, re1: AsmDefReg, reNew: AsmDefReg): AsmDefReg = re1
  }

  case object ReNew extends AsmIndefReg {
    def makeDefinite(re2: AsmDefReg, re1: AsmDefReg, reNew: AsmDefReg): AsmDefReg = reNew
  }

  // Handling Chars separately would produce nicer assembly but requires ugly code
  // to convert an escape character back to the escaped form
  // Maybe do this later
  // case class AsmChar(c: Char) {
  //   override def toString = s"#'$c'"
  // }

  sealed trait Asm

  sealed trait AsmInstr extends Asm {
    val opcode: String
    val cond: String
    def argsToString: String
    override def toString: String = s"$opcode$cond $argsToString"
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
    def argsToString: String = s"$label"
  }

  case class Mov(cond: String = "")(target: AsmDefReg, source: AsmDefArg) extends AsmInstr {
    val opcode = "MOV"
    override def argsToString: String = s"$target, $source"
  }

  case class Push(cond: String = "")(arg: AsmDefArg) extends AsmInstr {
    val opcode = "PUSH"
    override def argsToString = s"{$arg}"
  }

  case class Pop(cond: String = "")(arg: AsmDefArg) extends AsmInstr {
    val opcode = "POP"
    override def argsToString = s"{$arg}"
  }

  case class Or(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg) extends AsmInstr {
    val opcode = "ORR"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class And(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg) extends AsmInstr {
    val opcode = "AND"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class Compare(cond: String = "")(x: AsmDefReg, y: AsmDefArg) extends AsmInstr {
    val opcode = "CMP"
    override def argsToString: String = s"$x, $y"
  }

  case class Adds(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg) extends AsmInstr {
    val opcode = "ADDS"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class Subs(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg) extends AsmInstr {
    val opcode = "SUBS"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class SMull(cond: String = "")(target1: AsmDefReg, target2: AsmDefReg, x: AsmDefReg, y: AsmDefReg) extends AsmInstr {
    val opcode = "SMULL"
    override def argsToString: String = s"$target1, $target2, $x, $y"
  }

  //case class Mul(x: AsmDefiniteArg, y: AsmDefiniteArg)(target1: AsmDefiniteArg = x, target2: AsmDefiniteArg = y) extends AsmStandardInstr {
  //  override def toString = s"SMULL $x, $y, $x, $y"
  //  // TODO: include s"CMP $y, $x ASR #31\nBLNE ${label of overflow error function}"
  //  // result will be in register x
  //}

//  case class Mul (cond: String = "")(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends AsmStandardInstr {
//    override def toString = s"SMULL $target, $y, $x, $y"
//  }

  case class Not(cond: String = "")(target: AsmDefReg, x: AsmDefArg) extends AsmInstr {
    val opcode = "EOR"
    def argsToString = s"$target, $x, #1"
  }

  case class Neg(cond: String = "")(target: AsmDefReg, x: AsmDefArg)  extends AsmInstr {
    val opcode = "RSBS"
    override def argsToString = s"$target, $x, #0"
  }

  case class Ldr(cond: String = "")(target: AsmDefReg, source: AsmDefArg)(offset: AsmInt = AsmInt(0)) extends AsmInstr {
    override val opcode: String = "LDR"
    override def argsToString: String = {
      source match {
        case i@AsmInt(_) => s"$target, ${i.toLdrString}"
        case _ => s"$target, [$source, $offset]"
      }
    }
  }

  // args(0): source, args(1): dest
  case class Str(cond: String = "")(source: AsmDefReg, target: AsmDefArg)(offset: AsmInt = AsmInt(0)) extends AsmInstr {
    override val opcode: String = "STR"
    override def argsToString: String = {
      offset match {
        case AsmInt(0) => s"$source, [$target]"
        case _ => s"$source, [$target, $offset]"
      }
    }
  }

  object Len {
    def apply(cond: String = "")(target: AsmDefReg, x: AsmDefArg): Step = Ldr(cond)(target, x)()
  }

  object Eq {
    def apply(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg): Step = (
           Compare(cond)(x, y)
      <++> Mov("EQ")(target, AsmInt(1))
      <++> Mov("NE")(target, AsmInt(0))
    )
  }

  object Neq {
    def apply(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg): Step = (
           Compare(cond)(x, y)
        <++> Mov("NE")(target, AsmInt(1))
        <++> Mov("EQ")(target, AsmInt(0))
      )
  }

  object Leq {
    def apply(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg): Step = (
           Compare(cond)(x, y)
        <++> Mov("LE")(target, AsmInt(1))
        <++> Mov("GT")(target, AsmInt(0))
    )
  }

  object Lt {
    def apply(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg): Step = (
           Compare(cond)(x, y)
        <++> Mov("LT")(target, AsmInt(1))
        <++> Mov("GE")(target, AsmInt(0))
      )
  }

  object Geq {
    def apply(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg): Step = (
           Compare(cond)(x, y)
        <++> Mov("GE")(target, AsmInt(1))
        <++> Mov("LT")(target, AsmInt(0))
      )
  }

  object Gt {
    def apply(cond: String = "")(target: AsmDefReg, x: AsmDefReg, y: AsmDefArg): Step = (
           Compare(cond)(x, y)
        <++> Mov("GT")(target, AsmInt(1))
        <++> Mov("LE")(target, AsmInt(0))
      )
  }

  object Mul {
    def apply: Step = ???
  }

  object implicits {
    def implicitAsmInt(i: Int): AsmInt = AsmInt(i)
  }
}

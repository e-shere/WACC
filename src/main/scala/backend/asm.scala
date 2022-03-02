package backend

import backend.PredefinedFunctions.check_div_zero
import backend.state.State
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

    override def toString: String = r match {
      case -1 => "INVALID REGISTER"
      case 12 => "ip"
      case 13 => "sp"
      case 14 => "lr"
      case 15 => "pc"
      case x  => s"r$x"
    }
  }

  val NO_REG: AsmReg = AsmReg(-1)

  sealed trait AsmImmediate extends AsmDefiniteArg

  case class AsmStateFunc[T <: AsmDefiniteArg](func: State => T) extends AsmArg

  case class AsmInt(i: Int) extends AsmImmediate  {
    override def toString = s"#$i"

    def toLdrString = s"=$i"
  }

  case class AsmString(s: String) extends AsmImmediate {
    override def toString = s"$s"
    def toLdrString = s"=$s"
  }

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
    val cond: String
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
    def argsToString: String = s"$label"
  }

  case class Mov(cond: String = "")(args: AsmDefiniteArg *) extends AsmInstr {
    val opcode = "MOV"
    override def argsToString: String = s"${args(0)}, ${args(1)}"
  }

  case class Push(cond: String = "")(arg: AsmDefiniteArg) extends AsmInstr {
    val opcode = "PUSH"
    override def argsToString = s"{$arg}"
  }

  case class Pop(cond: String = "")(arg: AsmDefiniteArg) extends AsmInstr {
    val opcode = "POP"
    override def argsToString = s"{$arg}"
  }

  case class Or(cond: String = "")(val args: AsmDefiniteArg *) extends AsmStandardInstr {
    val opcode = "ORR"
  }

  case class And(cond: String = "")(val args: AsmDefiniteArg *) extends AsmStandardInstr {
    val opcode = "AND"
  }

  case class Compare(cond: String = "")(val args: AsmDefiniteArg *) extends AsmStandardInstr {
    val opcode = "CMP"
  }

  case class Adds(cond: String = "")(val args: AsmDefiniteArg *) extends AsmStandardInstr {
    val opcode = "ADDS"
  }

  case class Subs(cond: String = "")(val args: AsmDefiniteArg *) extends AsmStandardInstr {
    val opcode = "SUBS"
  }

  case class SMull(cond: String = "")(val args: AsmDefiniteArg *) extends AsmStandardInstr {
    val opcode = "SMULL"
  }

  //case class Mul(x: AsmDefiniteArg, y: AsmDefiniteArg)(target1: AsmDefiniteArg = x, target2: AsmDefiniteArg = y) extends AsmStandardInstr {
  //  override def toString = s"SMULL $x, $y, $x, $y"
  //  // TODO: include s"CMP $y, $x ASR #31\nBLNE ${label of overflow error function}"
  //  // result will be in register x
  //}

//  case class Mul (cond: String = "")(target: AsmReg, x: AsmReg, y: AsmDefiniteArg) extends AsmStandardInstr {
//    override def toString = s"SMULL $target, $y, $x, $y"
//  }

  case class Not(cond: String = "")(args: AsmDefiniteArg *) extends AsmInstr {
    val opcode = "EOR"
    def argsToString = s"${args(0)}, ${args(1)}, #1"
  }

  case class Neg(cond: String = "")(args: AsmDefiniteArg *)  extends AsmInstr {
    val opcode = "RSBS"
    override def argsToString = s"${args(0)}, ${args(1)}, #0"
  }

  case class Ldr(cond: String = "")(args: AsmDefiniteArg *)(offset: AsmInt = AsmInt(0)) extends AsmInstr {
    override val opcode: String = "LDR"
    override def argsToString: String = {
      args(1) match {
        case i@AsmInt(_) => s"${args(0)}, ${i.toLdrString}"
        case s@AsmString(_) => s"${args(0)}, ${s.toLdrString}"
        case _ => s"${args(0)}, [${args(1)}, $offset]"
      }
    }
  }

  // args(0): source, args(1): dest
  case class Str(cond: String = "")(args: AsmDefiniteArg *)(offset: AsmInt = AsmInt(0))extends AsmInstr {
    override val opcode: String = "STR"
    override def argsToString: String = {
      offset match {
        case AsmInt(0) => s"${args(0)}, [${args(1)}]"
        case _ => s"${args(0)}, [${args(1)}, $offset]"
      }
    }
  }

  object Len {
    def apply(cond: String = "")(args: AsmDefiniteArg *): Step = args match {
      case Seq(target: AsmReg, source) => Ldr(cond)(target, source)()
    }
  }

  object Eq {
    def apply(cond: String = "")(args: AsmDefiniteArg *): Step = (
           Compare(cond)(args.tail: _*)
      <++> Mov("EQ")(args.head, AsmInt(1))
      <++> Mov("NE")(args.head, AsmInt(0))
    )
  }

  object Neq {
    def apply(cond: String = "")(args: AsmDefiniteArg *): Step = (
      Compare(cond)(args.tail: _*)
        <++> Mov("NE")(args.head, AsmInt(1))
        <++> Mov("EQ")(args.head, AsmInt(0))
      )
  }

  object Leq {
    def apply(cond: String = "")(args: AsmDefiniteArg *): Step = (
      Compare(cond)(args.tail: _*)
        <++> Mov("LE")(args.head, AsmInt(1))
        <++> Mov("GT")(args.head, AsmInt(0))
    )
  }

  object Lt {
    def apply(cond: String = "")(args: AsmDefiniteArg *): Step = (
      Compare(cond)(args.tail: _*)
        <++> Mov("LT")(args.head, AsmInt(1))
        <++> Mov("GE")(args.head, AsmInt(0))
      )
  }

  object Geq {
    def apply(cond: String = "")(args: AsmDefiniteArg *): Step = (
      Compare(cond)(args.tail: _*)
        <++> Mov("GE")(args.head, AsmInt(1))
        <++> Mov("LT")(args.head, AsmInt(0))
      )
  }

  object Gt {
    def apply(cond: String = "")(args: AsmDefiniteArg *): Step = (
      Compare(cond)(args.tail: _*)
        <++> Mov("GT")(args.head, AsmInt(1))
        <++> Mov("LE")(args.head, AsmInt(0))
      )
  }

  object Mul {
    def apply: Step = ???
  }

  object implicits {
    def implicitAsmInt(i: Int): AsmInt = AsmInt(i)
  }
}

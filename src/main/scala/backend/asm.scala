package backend

import backend.asm.ConditionCode.{AL, EQ, GE, GT, LE, LT, NE, VS}
import backend.state.State
import backend.step._
import backend.step.implicits._

object asm {
  val WORD_BYTES = 4

  def escapeToStr(c: Char): String = {
    c match {
      case '\u0000'=> "\\0"
      case '\n' => "\\n"
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '\'' => "\\'"
      case '\"' => "\\\""
      case _ => s"$c"

    }
  }

  sealed trait AsmArg

  val NO_REG: AsmReg = AsmReg(-1)
  val r0 = AsmReg(0)
  val r1 = AsmReg(1)
  val r2 = AsmReg(2)
  val ip = AsmReg(12)
  val sp = AsmReg(13)
  val lr = AsmReg(14)
  val pc = AsmReg(15)

  case class AsmReg(r: Int) extends AsmArg with ConstIndef[AsmReg] {

    override def toString: String = r match {
        // Consider factoring out the magic numbers
        case x if x >= 0 && x <= 11 => s"r$x"
        case 12 => "ip"
        case 13 => "sp"
        case 14 => "lr"
        case 15 => "pc"
        case _ => "INVALID REGISTER"
      }

    val constDef = this
  }

  case class AsmStateFunc[+T <: AsmArg](func: State => (T, List[Asm], State)) extends Indef[T] {
    def apply(data: ResolutionData): (T, List[Asm], State) = func(data.state)
  }

  case class AsmPureFunc[+T <: AsmArg](func: State => T) extends Indef[T] {
    def apply(data: ResolutionData): (T, List[Asm], State) = (func(data.state), Nil, data.state)
  }

  val zero = AsmInt(0)
  val word_size = AsmInt(WORD_BYTES)
  case class AsmInt(i: Int) extends AsmArg with ConstIndef[AsmInt] {
    override def toString = s"#$i"

    def toLdrString = s"=$i"
    
    val constDef = this
  }

  case class AsmString(s: String) extends AsmArg with ConstIndef[AsmString] {
    override def toString = s"$s"
    def toLdrString = s"=$s"

    val constDef = this
  }

  case class AsmChar(c: Char) extends AsmArg with ConstIndef[AsmChar] {
    override def toString: String = s"#'${escapeToStr(c)}'"
    def toLdrString = s"='${escapeToStr(c)}'"

    val constDef = this
  }

  sealed trait AsmIndefReg extends Indef[AsmReg]

  case object Re2 extends AsmIndefReg {
    def apply(data: ResolutionData): (AsmReg, List[Asm], State) = (data.re2, Nil, data.state)
//    override def toString: String = "Re2"
  }

  case object Re1 extends AsmIndefReg {
    def apply(data: ResolutionData): (AsmReg, List[Asm], State) = (data.re1, Nil, data.state)
//    override def toString: String = "Re1"
  }

  case object ReNew extends AsmIndefReg {
    def apply(data: ResolutionData): (AsmReg, List[Asm], State) = (data.reNew, Nil, data.state)
//    override def toString: String = "ReNew"
  }

  sealed trait Asm

  sealed trait AsmInstr extends Asm {
    val opcode: String
    val cond: ConditionCode.Value
    def argsToString: String
    override def toString: String = s"$opcode$cond $argsToString"
  }

  object ConditionCode extends Enumeration {
    type Cond = Value
    val EQ, NE, LT, LE, GT, GE, CS, VS = Value
    val AL: ConditionCode.Value = Value("")
  }

  val SEP = "\n\t"

  case class Directive(value: String) extends Asm {
    override def toString: String = "." + value
  }

  case class Label(value: String) extends Asm {
    override def toString: String = value + ":"
  }

  case class Branch(cond: ConditionCode.Value = AL)(label: String) extends AsmInstr {
    val opcode = "B"
    def argsToString: String = s"$label"
  }

  case class BranchLink(cond: ConditionCode.Value = AL)(label: String) extends AsmInstr {
    val opcode = "BL"
    def argsToString: String = s"$label"
  }

  case class Mov(cond: ConditionCode.Value = AL)(target: AsmReg, source: AsmArg) extends AsmInstr {
    val opcode = "MOV"
    override def argsToString: String = s"$target, $source"
  }

  case class Push(cond: ConditionCode.Value = AL)(arg: AsmArg) extends AsmInstr {
    val opcode = "PUSH"
    override def argsToString = s"{${arg}}"
  }

  case class Pop(cond: ConditionCode.Value = AL)(arg: AsmArg) extends AsmInstr {
    val opcode = "POP"
    override def argsToString = s"{${arg}}"
  }

  case class Or(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg) extends AsmInstr {
    val opcode = "ORR"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class And(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg) extends AsmInstr {
    val opcode = "AND"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class Compare(cond: ConditionCode.Value = AL)(x: AsmReg, y: AsmArg)(aux: String = "") extends AsmInstr {
    val opcode = "CMP"
    override def argsToString: String = s"$x, $y${if (aux.isEmpty) "" else ", "}$aux"
  }

  case class Adds(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg) extends AsmInstr {
    val opcode = "ADDS"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class Subs(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg) extends AsmInstr {
    val opcode = "SUBS"
    override def argsToString: String = s"$target, $x, $y"
  }

  case class SMull(cond: ConditionCode.Value = AL)(target1: AsmReg, target2: AsmReg, x: AsmReg, y: AsmReg) extends AsmInstr {
    val opcode = "SMULL"
    override def argsToString: String = s"$target1, $target2, $x, $y"
  }

  case class Not(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmArg) extends AsmInstr {
    val opcode = "EOR"
    def argsToString = s"$target, $x, #1"
  }

  case class Neg(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmArg)  extends AsmInstr {
    val opcode = "RSBS"
    override def argsToString = s"$target, $x, #0"
  }

  case class Ldr(cond: ConditionCode.Value = AL)(target: AsmReg, source: AsmArg)(offset: AsmInt = zero) extends AsmInstr {
    override val opcode: String = "LDR"
    override def argsToString: String = {
      source match {
        case i@AsmInt(_) => s"$target, ${i.toLdrString}"
        case s@AsmString(_) => s"$target, ${s.toLdrString}"
        case _ => s"$target, [$source, $offset]"
      }
    }
  }

  case class Str(cond: ConditionCode.Value = AL)(source: AsmReg, target: AsmArg)(offset: AsmInt = zero) extends AsmInstr {
    override val opcode: String = "STR"
    override def argsToString: String = {
      offset match {
        case AsmInt(0) => s"$source, [$target]"
        case _ => s"$source, [$target, $offset]"
      }
    }
  }

  object Len {
    def apply(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmArg): Step = Ldr(cond)(target, x)()
  }

  object Eq {
    def apply(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg): Step = (
           Compare(cond)(x, y)()
      >++> Mov(EQ)(target, AsmInt(1))
      >++> Mov(NE)(target, AsmInt(0))
    )
  }

  object Neq {
    def apply(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg): Step = (
           Compare(cond)(x, y)()
        >++> Mov(NE)(target, AsmInt(1))
        >++> Mov(EQ)(target, AsmInt(0))
      )
  }

  object Leq {
    def apply(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg): Step = (
           Compare(cond)(x, y)()
        >++> Mov(LE)(target, AsmInt(1))
        >++> Mov(GT)(target, AsmInt(0))
    )
  }

  object Lt {
    def apply(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg): Step = (
           Compare(cond)(x, y)()
        >++> Mov(LT)(target, AsmInt(1))
        >++> Mov(GE)(target, AsmInt(0))
      )
  }

  object Geq {
    def apply(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg): Step = (
           Compare(cond)(x, y)()
        >++> Mov(GE)(target, AsmInt(1))
        >++> Mov(LT)(target, AsmInt(0))
      )
  }

  object Gt {
    def apply(cond: ConditionCode.Value = AL)(target: AsmReg, x: AsmReg, y: AsmArg): Step = (
           Compare(cond)(x, y)()
        >++> Mov(GT)(target, AsmInt(1))
        >++> Mov(LE)(target, AsmInt(0))
      )
  }

  object implicits {
    def implicitAsmInt(i: Int): AsmInt = AsmInt(i)
  }
}

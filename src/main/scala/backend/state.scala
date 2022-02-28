package backend

import asm._
import backend.step.Step
import backend.step.implicits.implicitStep

import scala.language.implicitConversions

object state {

  val REG_START = AsmReg(4)
  val REG_END = AsmReg(9)
  val PLACEHOLDER_1 = AsmReg(10)
  val PLACEHOLDER_2 = AsmReg(11)
  val STACK_POINTER = AsmReg(13)
  val NEW_REG: RegState = RegState(REG_START)

  /*
  Reg documents the highest register of 4-9 which is not in use
  If reg > 9, reg documents the number of things in the stack + REG_END + 1
   */
  // TODO: ROB PLEASE DO SOME OFF BY ONE CHECKS HERE
  // TODO: unit test this
  case class RegState(reg: AsmReg) {
    def isReg: Boolean = reg.r >= REG_START.r && reg.r <= REG_END.r
    def isStack: Boolean = reg.r > REG_END.r
    def prev: RegState = RegState(AsmReg(reg.r - 1))
    def next: RegState = RegState(AsmReg(reg.r + 1))
    def read: (AsmReg, List[Asm], RegState) = {
      if (prev.isReg) (prev.reg, Nil, prev)
      else (PLACEHOLDER_1, List(Pop(PLACEHOLDER_1)), prev)
    }
    def read2: (AsmReg, AsmReg, List[Asm], RegState) = {
      if (isReg) (prev.reg, reg, Nil, prev.prev)
      else if (prev.isReg) (prev.reg, PLACEHOLDER_1, List(Pop(PLACEHOLDER_1)), prev.prev)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(Pop(PLACEHOLDER_2), Pop(PLACEHOLDER_1)), prev.prev)
    }
    def peek: (AsmReg, List[Asm], RegState) = {
      if (isReg) (prev.reg, Nil, this)
      else (PLACEHOLDER_1, List(Ldr(PLACEHOLDER_1, STACK_POINTER)), this)
    }
    def peek2: (AsmReg, AsmReg, List[Asm], RegState) = {
      if (isReg) (prev.reg, reg, Nil, this)
      else if (prev.isReg) (prev.reg, PLACEHOLDER_1, List(Ldr(PLACEHOLDER_1, STACK_POINTER)), this)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(Ldr(PLACEHOLDER_2, STACK_POINTER), new Ldr(PLACEHOLDER_1, STACK_POINTER, AsmInt(4))), this)
    }
    def write: (AsmReg, List[Asm], RegState) = {
      if (isReg) (reg, Nil, next)
      else (PLACEHOLDER_1, List(Push(PLACEHOLDER_1)), next)
    }
  }

  case class funcState() {

    //TODO: make these not private in generator?
    private val r0 = AsmReg(0)
    private val lr = AsmReg(14)
    private val pc = AsmReg(15)

    val includedFuncs: Set[PredefinedFunc] = Set.empty

    def getPredefFuncs(): Step = {
      includedFuncs.foldLeft(Step.identity)(
        (prev, f) => prev <++> f.toStep
      )
    }

    // TODO: enum or case classes of each type of predefined functions
    sealed trait PredefinedFunc {
      def toStep: Step
      val label: String
    }

    case class print_ln() {
      val label = "p_print_ln"
      def toStep: Step = Label(label) <++> Push(lr) <++> ??? <++>
        asm.Branch("fflush")("L") <++> Pop(pc)
    }

    case class print_int() {
      val label = "p_print_int"
      def toStep: Step = ???
    }

    case class print_string() {
      val label = "p_print_string"
      def toStep: Step = ???
    }

    case class print_bool() {
      val label = "p_print_bool"
      def toStep: Step = ???
    }

    case class print_ref() {
      val label = "p_print_ref"
      def toStep: Step = ???
    }

    case class throw_overflow() {
      val label = "p_throw_overflow"
      def toStep: Step = ???
    }

    case class throw_runtime() {
      val label = "p_throw_runtime"
      def toStep: Step =
        Branch(print_string().label)("L") <++> Mov(r0, AsmInt(-1)) <++> Branch("exit")("L")
    }

    case class check_div_zero() {
      val label = "p_check_div_zero"
      def toStep: Step = ???
    }

    case class free_array() {
      val label = "p_free_array"
      def toStep: Step = ???
    }

    case class free_pair() {
      val label = "p_free_pair"
      def toStep: Step = ???
    }

    case class check_null_pointer() {
      val label = "p_check_null_pointer"
      def toStep: Step = ???
    }

    case class check_array_bound() {
      val label = "p_check_array_bound"
      def toStep: Step = ???
    }

    case class read_char() {
      val label = "p_read_char"
      def toStep: Step = ???
    }

    case class read_int() {
      val label = "p_read_int"
      def toStep: Step = ???
    }

  }

}

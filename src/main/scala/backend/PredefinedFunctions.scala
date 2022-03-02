package backend

import backend.asm.ConditionCode._
import backend.asm._
import backend.generator.addPredefFunc
import backend.step.implicits.implicitStep
import backend.step._

object PredefinedFunctions {

  //TODO: make these not private in generator?
  private val r0 = AsmReg(0)
  private val r1 = AsmReg(1)
  private val r2 = AsmReg(2)
  private val lr = AsmReg(14)
  private val pc = AsmReg(15)
  private val word_size = AsmInt(4)
  private val zero = AsmInt(0)

  // TODO: enum or case classes of each type of predefined functions
  sealed trait PredefinedFunc {
    def toStep: Step
    val label: String
  }

  case class print_ln() extends PredefinedFunc {
    val label = "p_print_ln"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Ldr()(r0, zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("puts")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
      )
  }

  case class print_int() extends PredefinedFunc {
    val label = "p_print_int"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Mov()(r1, r0)
      >++> Ldr()(r0, zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }

  // print_ char is just "BL putchar"

  case class print_string() extends PredefinedFunc {
    val label = "p_print_string"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Ldr()(r1, r0)()
      >++> Adds()(r2, r0, word_size)
      >++> Ldr()(r0, zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }

  case class print_bool() extends PredefinedFunc {
    val label = "p_print_bool"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)
      /* <++> Load True if NE */
      /* <++> Load False if EQ */
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Ldr()(r0, zero)()
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }

  case class print_ref() extends PredefinedFunc {
    val label = "p_print_ref"
    def toStep: Step = (
      Label(label)
      >++> Push()(lr)
      >++> Mov()(r1, r0)
      >++> Ldr()(r0, zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }

  case class throw_overflow() extends PredefinedFunc {
    val label = "p_throw_overflow"
    def toStep: Step = (
           Label(label)
      /* <++> Load error message */
      >++> BranchLink()(throw_runtime().label)
    )
  }

  case class throw_runtime() extends PredefinedFunc {
    val label = "p_throw_runtime"
    def toStep: Step = (
           Label(label)
      >++> BranchLink()(print_string().label)
      >++> Mov()(r0, AsmInt(-1))
      >++> BranchLink()("exit")
      )
  }

  case class check_div_zero() extends PredefinedFunc {
    val label = "p_check_div_zero"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)
      /* <++> Load error message if EQ */
      >++> Branch(EQ)(throw_runtime().label)
      >++> Pop()(pc)
      )
  }

  case class free() extends PredefinedFunc {
    val label = "p_free"
    def toStep: Step = (
             Label(label)
        >++> Push()(lr)
        >++> check_null_pointer().toStep
        >++> BranchLink()("free")
        >++> Pop()(pc)
      )
  }

  case class check_null_pointer() extends PredefinedFunc {
    val label = "p_check_null_pointer"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)
      /* <++> Load error message if EQ */
      >++> Branch(EQ)(throw_runtime().label)
      >++> Pop()(pc)
    )
  }

  case class check_array_bound() extends PredefinedFunc {
    val label = "p_check_array_bound"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)
      /* <++> Load error message if LT */
      >++> BranchLink(LT)(throw_runtime.toString()) // Link, Less than
      >++> Ldr()(r1, r1)()
      >++> Compare()(r0, r1)
      /* <++> Load error message if CS */
      >++> BranchLink(CS)(throw_runtime.toString()) // Link, Carry set
      >++> Pop()(pc)
    )
  }

  case class read_byte() extends PredefinedFunc {
    val label = "p_read_byte"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Mov()(r1, r0)
      >++> Ldr()(r0, zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("scanf")
      >++> Pop()(pc)
    )
  }
}

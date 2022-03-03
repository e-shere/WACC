package backend

import backend.asm.ConditionCode._
import backend.asm._
import backend.generator.includeData
import backend.step._
import backend.step.implicits.implicitStep

object PredefinedFunctions {
  // TODO: enum or case classes of each type of predefined functions
  sealed trait PredefinedFunc {
    def toStep: Step
    val label: String
  }

  case class print_ln() extends PredefinedFunc {
    val label = "p_print_ln"
    val null_char = "\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> includeData(null_char)
      >++> Step.instr2Aux(Ldr())(r0, AsmStateFunc(_.data(null_char)))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("puts")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
      )
  }

  case class print_int() extends PredefinedFunc {
    val label = "p_print_int"
    val number_format = "%d\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Mov()(r1, r0)
      >++> includeData(number_format)
      >++> Step.instr2Aux(Ldr())(r0, AsmStateFunc(_.data(number_format)))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }
  case class print_char() extends PredefinedFunc {
    val label = "p_print_char"
    def toStep: Step = (
      Label(label)
        >++> Push()(lr)
        >++> BranchLink()("putchar")
        >++> Pop()(pc)
      )
  }

  case class print_string() extends PredefinedFunc {
    val label = "p_print_string"
    val string_format = "%.*s\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Ldr()(r1, r0)()
      >++> Adds()(r2, r0, word_size)
      >++> includeData(string_format)
      >++> Step.instr2Aux(Ldr())(r0, AsmStateFunc(_.data(string_format)))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }

  case class print_bool() extends PredefinedFunc {
    val label = "p_print_bool"
    val message_true = "true"
    val message_false = "false"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)()
      >++> includeData(message_true)
      >++> Step.instr2Aux(Ldr(NE))(r0, AsmStateFunc(_.data(message_true)))(zero)()
      >++> includeData(message_false)
      >++> Step.instr2Aux(Ldr(EQ))(r0, AsmStateFunc(_.data(message_false)))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Ldr()(r0, zero)()
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }

  case class print_ref() extends PredefinedFunc {
    val label = "p_print_ref"
    val pointer_format = "%p\\0"
    def toStep: Step = (
      Label(label)
      >++> Push()(lr)
      >++> Mov()(r1, r0)
      >++> includeData(pointer_format)
      >++> Step.instr2Aux(Ldr())(r0, AsmStateFunc(_.data(pointer_format)))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("printf")
      >++> Mov()(r0, zero)
      >++> BranchLink()("fflush")
      >++> Pop()(pc)
    )
  }

  case class throw_overflow() extends PredefinedFunc {
    val label = "p_throw_overflow"
    val message = "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0"
    def toStep: Step = (
           Label(label)
      >++> includeData(message)
      >++> Step.instr2Aux(Ldr())(r0, AsmStateFunc(_.data(message)))(zero)()
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
    val message = "DivideByZeroError: divide or modulo by zero\\n\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)()
      >++> includeData(message)
      >++> Step.instr2Aux(Ldr(EQ))(r0, AsmStateFunc(_.data(message)))(zero)()
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
    val message = "NullReferenceError: dereference a null reference\\n\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)()
      >++> includeData(message)
      >++> Step.instr2Aux(Ldr(EQ))(r0, AsmStateFunc(_.data(message)))(zero)()
      >++> Branch(EQ)(throw_runtime().label)
      >++> Pop()(pc)
    )
  }

  case class check_array_bound() extends PredefinedFunc {
    val label = "p_check_array_bound"
    val message_LT = "ArrayIndexOutOfBoundsError: negative index\\n\\0"
    val message_GT = "ArrayIndexOutOfBoundsError: index too large\\n\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)()
      >++> includeData(message_LT)
      >++> Step.instr2Aux(Ldr(LT))(r0, AsmStateFunc(_.data(message_LT)))(zero)()
      >++> BranchLink(LT)(throw_runtime.toString()) // Link, Less than
      >++> Ldr()(r1, r1)()
      >++> Compare()(r0, r1)()
      >++> includeData(message_GT)
      >++> Step.instr2Aux(Ldr(CS))(r0, AsmStateFunc(_.data(message_GT)))(zero)()
      >++> BranchLink(CS)(throw_runtime.toString()) // Link, Carry set
      >++> Pop()(pc)
    )
  }

  case class read_char() extends PredefinedFunc {
    val label = "p_read_char"
    val char_format =" %c\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Mov()(r1, r0)
      >++> includeData(char_format)
      >++> Step.instr2Aux(Ldr())(r0, AsmStateFunc(_.data(char_format)))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> BranchLink()("scanf")
      >++> Pop()(pc)
    )
  }

  case class read_int() extends PredefinedFunc {
    val label = "p_read_int"
    val number_format = "%d\\0"
    def toStep: Step = (
      Label(label)
        >++> Push()(lr)
        >++> Mov()(r1, r0)
        >++> includeData(number_format)
        >++> Step.instr2Aux(Ldr())(r0, AsmStateFunc(_.data(number_format)))(zero)()
        >++> Adds()(r0, r0, word_size)
        >++> BranchLink()("scanf")
        >++> Pop()(pc)
      )
  }
}

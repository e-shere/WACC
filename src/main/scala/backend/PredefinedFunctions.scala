package backend

import backend.asm.ConditionCode._
import backend.asm._
import backend.generator._
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
      >++> Step.instr2Aux(Ldr())(r0, useData(null_char))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> genBuiltinCall()("puts", 0, None)
      >++> Mov()(r0, zero)
      >++> genBuiltinCall()("fflush", 0, None)
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
      >++> Step.instr2Aux(Ldr())(r0, useData(number_format))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> genBuiltinCall()("printf", 0, None)
      >++> Mov()(r0, zero)
      >++> genBuiltinCall()("fflush", 0, None)
      >++> Pop()(pc)
    )
  }
  case class print_char() extends PredefinedFunc {
    val label = "p_print_char"
    def toStep: Step = (
      Label(label)
        >++> Push()(lr)
        >++> genBuiltinCall()("putchar", 0, None)
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
      >++> Step.instr2Aux(Ldr())(r0, useData(string_format))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> genBuiltinCall()("printf", 0, None)
      >++> Mov()(r0, zero)
      >++> genBuiltinCall()("fflush", 0, None)
      >++> Pop()(pc)
    )
  }

  case class print_bool() extends PredefinedFunc {
    val label = "p_print_bool"
    val message_true = "true\\0"
    val message_false = "false\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)()
      >++> Step.instr2Aux(Ldr(NE))(r0, useData(message_true))(zero)()
      >++> Step.instr2Aux(Ldr(EQ))(r0, useData(message_false))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> genBuiltinCall()("printf", 0, None)
      >++> Ldr()(r0, zero)()
      >++> genBuiltinCall()("fflush", 0, None)
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
      >++> Step.instr2Aux(Ldr())(r0, useData(pointer_format))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> genBuiltinCall()("printf", 0, None)
      >++> Mov()(r0, zero)
      >++> genBuiltinCall()("fflush", 0, None)
      >++> Pop()(pc)
    )
  }

  case class throw_overflow() extends PredefinedFunc {
    val label = "p_throw_overflow"
    val message = "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0"
    def toStep: Step = (
           Label(label)
      >++> Step.instr2Aux(Ldr())(r0, useData(message))(zero)()
      >++> BranchLink()(throw_runtime().label)
    )
  }

  case class throw_runtime() extends PredefinedFunc {
    val label = "p_throw_runtime"
    def toStep: Step = (
           Label(label)
      >++> BranchLink()(print_string().label)
      >++> Mov()(r0, AsmInt(-1))
      >++> genBuiltinCall()("exit", 0, None)
      )
  }

  case class check_div_zero() extends PredefinedFunc {
    val label = "p_check_div_zero"
    val message = "DivideByZeroError: divide or modulo by zero\\n\\0"
    def toStep: Step = (
           Label(label)
      >++> Push()(lr)
      >++> Compare()(r0, zero)()
      >++> Step.instr2Aux(Ldr(EQ))(r0, useData(message))(zero)()
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
        >++> genBuiltinCall()("free", 0, None)
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
      >++> Step.instr2Aux(Ldr(EQ))(r0, useData(message))(zero)()
      >++> genPredefCall(EQ)(throw_runtime(), 0, None)
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
      >++> Step.instr2Aux(Ldr(LT))(r0, useData(message_LT))(zero)()
      >++> genPredefCall(LT)(throw_runtime(), 0, None) // Link, Less than
      >++> Ldr()(r1, r1)()
      >++> Compare()(r0, r1)()
      >++> Step.instr2Aux(Ldr(CS))(r0, useData(message_GT))(zero)()
      >++> genPredefCall(CS)(throw_runtime(), 0, None) // Link, Carry set
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
      >++> Step.instr2Aux(Ldr())(r0, useData(char_format))(zero)()
      >++> Adds()(r0, r0, word_size)
      >++> genBuiltinCall()("scanf", 0, None)
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
        >++> Step.instr2Aux(Ldr())(r0, useData(number_format))(zero)()
        >++> Adds()(r0, r0, word_size)
        >++> genBuiltinCall()("scanf", 0, None)
        >++> Pop()(pc)
      )
  }
}

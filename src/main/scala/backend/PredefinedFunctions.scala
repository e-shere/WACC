package backend

import backend.asm._
import backend.step.Step
import backend.step.implicits.implicitStep

object PredefinedFunctions {

  //TODO: make these not private in generator?
  private val r0 = AsmReg(0)
  private val r1 = AsmReg(1)
  private val r2 = AsmReg(2)
  private val r3 = AsmReg(3)
  private val sp = AsmReg(13)
  private val lr = AsmReg(14)
  private val pc = AsmReg(15)
  private val WORD_BYTES = 4

  // TODO: enum or case classes of each type of predefined functions
  sealed trait PredefinedFunc {
    def toStep: Step
    val label: String
  }

  case class print_ln() {
    val label = "p_print_ln"
    def toStep: Step = Label(label) <++> Push(lr) <++> ??? <++>
      Branch("fflush")("L") <++> Pop(pc)
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
    def toStep: Step = (
           Label(label)
      /* <++> Load error message */
      <++> Branch(throw_runtime().label)("L")
    )
  }

  case class throw_runtime() {
    val label = "p_throw_runtime"
    def toStep: Step = (
           Label(label)
      <++> Branch(print_string().label)("L")
      <++> Mov(r0, AsmInt(-1))
      <++> Branch("exit")("L")
      )
  }

  case class check_div_zero() {
    val label = "p_check_div_zero"
    def toStep: Step = (
           Label(label)
      <++> Push(lr)
      <++> Compare.step(r0, AsmInt(0))
      /* <++> Load error message if EQ */
      <++> Branch(throw_runtime().label)("EQ")
      <++> Pop(pc)
      )
  }

  case class free() {
    val label = "p_free"
    def toStep: Step = (
             Label(label)
        <++> Push(lr)
        <++> check_null_pointer().toStep
        <++> Branch("free")("L")
        <++> Pop(pc)
      )
  }

  case class check_null_pointer() {
    val label = "p_check_null_pointer"
    def toStep: Step = (
           Label(label)
      <++> Push(lr)
      <++> Compare.step(r0, AsmInt(0))
      /* <++> Load error message if EQ */
      <++> Branch(throw_runtime().label)("EQ")
      <++> Pop(pc)
    )
  }

  case class check_array_bound() {
    val label = "p_check_array_bound"
    def toStep: Step = (
           Label(label)
      <++> Push(lr)
      <++> Compare.step(r0, AsmInt(0))
      /* <++> Load error message if LT */
      <++> Branch(throw_runtime.toString())("LLT") // Link, Less than
      <++> Ldr.step(r1, r1)
      <++> Compare.step(r0, r1)
      /* <++> Load error message if CS */
      <++> Branch(throw_runtime.toString())("LCS") // Link, Carry set
      <++> Pop(pc)
    )
  }

  case class read_char() {
    val label = "p_read_char"
    def toStep: Step = (
           Label(label)
      <++> Push(lr)
      <++> Mov.step(r1, r0)
      <++> Ldr.step(r0, AsmInt(0))
      <++> Add.step(r0, r0, AsmInt(WORD_BYTES))
      <++> Branch("scanf")("L")
      <++> Pop(pc)
    )
  }

  case class read_int() {
    val label = "p_read_int"
    def toStep: Step = (
           Label(label)
      <++> Push(lr)
      <++> Mov.step(r1, r0)
      <++> Ldr.step(r0, AsmInt(0))
      <++> Add.step(r0, r0, AsmInt(WORD_BYTES))
      <++> Branch("scanf")("L")
      <++> Pop(pc)
    )
  }
}

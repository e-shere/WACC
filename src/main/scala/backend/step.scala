package backend

import backend.asm._
import backend.state.{NEW_REG, PLACEHOLDER_1, REG_START, STACK_POINTER, State}
import backend.step.Step.discardAll
import scala.language.implicitConversions

object step {
  import implicits._

  case class Step(func: State => (List[Asm], State)) {
    override def toString: String = mkString("\n")

    def mkString(sep: String): String = this(NEW_REG)._1.mkString(sep)

    def apply(state: State): (List[Asm], State) = func(state)

    // >++> append
    // <++< prepend
    def >++>(next: Step): Step = Step((state: State) => {
      val (asm1, state1) = this(state)
      val (asm2, state2) = next(state1)
      (asm1 ++ asm2, state2)
    })

    def <++<(next: Step): Step = Step((state: State) => {
      val (asm1, state1) = (this >++> discardAll)(state)
      val (asm2, state2) = next(state1)
      (asm2 ++ asm1, state2)
    })
  }

  object Step {

    val identity: Step = Step((Nil, _))
    // This step is used between steps where the state of registers needs to be reset

    val discardAll: Step = Step(state => {
      (if (state.getStackOffset > 0) Step.asmInstr(Adds())(STACK_POINTER, STACK_POINTER, AsmInt(state.getStackOffset * BYTE_SIZE))()(state)._1 else Nil, State(REG_START, state.fState, state.data))
    })

    val discardTop: Step = Step(state => {
      if (state.isReg) (Nil, state.prev) else (List(Pop()(PLACEHOLDER_1)), state.prev)
    })

    //out: anyregs which are written to, not including NEWREG
    def stepInstr(f: (Seq[AsmDefiniteArg]) => Step)(args: AsmArg*)(out: AsmAnyReg*): Step = Step((state: State) => {
      if (args contains Re2) assert(!(args contains ReNew))

//      println(s"args: ${args.mkString(" ")}")
//      println(s"out: ${out.mkString(" ")}\n")

      val (re2, re1, asm1, state1) =
        if (args contains Re2) state.read2
        else if (args contains Re1) {
          val result = state.read
          (NO_REG, result._1, result._2, result._3)
        }
        else (NO_REG, NO_REG, Nil, state)

      val (reNew, asm2, state2) = (out contains Re2, out contains Re1, args contains ReNew) match {
        case (true, true, false) => {
          assert((args contains Re1) && (args contains Re2))
          val (re2w, re1w, asm2, state2) = state1.write2
          assert(re2w == re2)
          assert(re1w == re1)
          (NO_REG, asm2, state2)
        }
        case (true, false, false) => {
          assert(args contains Re2)
          val (re2w, asm2, state2) = state1.prev.write
          assert(re2w == re2)
          (NO_REG, asm2, state2)
        }
        case (false, true, false) => {
          assert(args contains Re1)
          val (re1w, asm2, state2) = state1.write
          assert(re1w == re1)
          (NO_REG, asm2, state2)
        }
        case (false, true, true) => {
          assert(args contains Re1)
          val (re1w, reNw, asm2, state2) = state1.write2
          assert(re1w == re1)
          (reNw, asm2, state2)
        }
        case (false, false, true) => {
          val (reNw, asm2, state2) = state1.write
          (reNw, asm2, state2)
        }
        case (false, false, false) => (NO_REG, Nil, state1)
      }

      val argsDefinite: Seq[AsmDefiniteArg] = args.map {
        case Re1 => re1
        case Re2 => re2
        case ReNew => reNew
        case arg: AsmDefiniteArg => arg
        case AsmStateFunc(func) => func(state1)
      }

      val (asmF, stateF) = f.apply(argsDefinite)(state1)

      assert(state1.reg == stateF.reg)
      (asm1 ++ asmF ++ asm2, state2)
    })

    def asmInstr(f: Seq[AsmDefiniteArg] => Asm)(args: AsmArg*)(out: AsmAnyReg*): Step = {
      stepInstr((x: Seq[AsmDefiniteArg]) => f(x))(args: _*)(out: _*)
    }

    def genericStepInstr[T](f: (Seq[AsmDefiniteArg]) => T => Step)(args: AsmArg*)(aux: T)(out: AsmAnyReg*): Step =
      stepInstr(f(_)(aux))(args: _*)(out: _*)

    def genericAsmInstr[T](f: (Seq[AsmDefiniteArg]) => T => Asm)(args: AsmArg*)(aux: T)(out: AsmAnyReg*): Step =
      stepInstr(f(_)(aux))(args: _*)(out: _*)


  }
  object implicits {
    implicit def implicitStep(node: Asm): Step = Step((List(node), _))
  }
}

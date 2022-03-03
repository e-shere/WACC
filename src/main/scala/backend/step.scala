package backend

import backend.asm._
import backend.state.{NEW_REG, PLACEHOLDER_1, REG_START, STACK_POINTER, State}
import backend.step.Step.discardAll
import scala.language.implicitConversions

object step {
  import implicits._

  case class ResolutionData(state: State, re2: AsmReg, re1: AsmReg, reNew: AsmReg)

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
      (if (state.getStackOffset > 0) Step.instr3(Adds())(STACK_POINTER, STACK_POINTER, AsmInt(state.getStackOffset * BYTE_SIZE))()(state)._1 else Nil, State(REG_START, state.fState, state.data))
    })

    val discardTop: Step = Step(state => {
      if (state.isReg) (Nil, state.prev) else (List(Pop()(PLACEHOLDER_1)), state.prev)
    })

    def instr1[T1 <: AsmArg](f: T1 => Step)(arg1: ResolutionData => T1)(out: AsmIndefReg *): Step = {
      val f4: (T1, AsmInt, AsmInt, AsmInt) => Step = (a, _, _, _) => f(a)
      instr4[T1, AsmInt, AsmInt, AsmInt](f4)(arg1, AsmInt(0), AsmInt(0), AsmInt(0))(out: _*)
    }

    def instr2[T1 <: AsmArg, T2 <: AsmArg](f: (T1, T2) => Step)(arg1: ResolutionData => T1, arg2: ResolutionData => T2)(out: AsmIndefReg *): Step = {
      val f4: (T1, T2, AsmInt, AsmInt) => Step = (a, b, _, _) => f(a, b)
      instr4[T1, T2, AsmInt, AsmInt](f4)(arg1, arg2, AsmInt(0), AsmInt(0))(out: _*)
    }

    def instr3[T1 <: AsmArg, T2 <: AsmArg, T3 <: AsmArg](f: (T1, T2, T3) => Step)(arg1: ResolutionData => T1, arg2: ResolutionData => T2, arg3: ResolutionData => T3)(out: AsmIndefReg *): Step = {
      val f4: (T1, T2, T3, AsmInt) => Step = (a, b, c, _) => f(a, b, c)
      instr4[T1, T2, T3, AsmInt](f4)(arg1, arg2, arg3, AsmInt(0))(out: _*)
    }

    def instr4[T1 <: AsmArg, T2 <: AsmArg, T3 <: AsmArg, T4 <: AsmArg](f: (T1, T2, T3, T4) => Step)(arg1: ResolutionData => T1, arg2: ResolutionData => T2, arg3: ResolutionData => T3, arg4: ResolutionData => T4)(out: AsmIndefReg *): Step = Step((state: State) => {
      val args: Set[ResolutionData => AsmArg] = Set(arg1, arg2, arg3, arg4)

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
        case (true, true, true) => (NO_REG, Nil, state1) // Already ruled out
        case (true, true, false) => {
          assert((args contains Re1) && (args contains Re2))
          val (re2w, re1w, asm2, state2) = state1.write2
          assert(re2w == re2)
          assert(re1w == re1)
          (NO_REG, asm2, state2)
        }
        case (true, false, true) => (NO_REG, Nil, state1) // Already ruled out
        case (true, false, false) => {
          assert(args contains Re2)
          val (re2w, asm2, state2) = state1.prev.write
          assert(re2w == re2)
          (NO_REG, asm2, state2)
        }
        case (false, true, true) => {
          assert(args contains Re1)
          val (re1w, reNw, asm2, state2) = state1.write2
          assert(re1w == re1)
          (reNw, asm2, state2)
        }
        case (false, true, false) => {
          assert(args contains Re1)
          val (re1w, asm2, state2) = state1.write
          assert(re1w == re1)
          (NO_REG, asm2, state2)
        }
        case (false, false, true) => {
          val (reNw, asm2, state2) = state1.write
          (reNw, asm2, state2)
        }
        case (false, false, false) => (NO_REG, Nil, state1)
      }

      val (asmF, stateF) = f(
        arg1(ResolutionData(state1, re2, re1, reNew)), 
        arg2(ResolutionData(state1, re2, re1, reNew)), 
        arg3(ResolutionData(state1, re2, re1, reNew)), 
        arg4(ResolutionData(state1, re2, re1, reNew))
      )(state1)

      (asm1 ++ asmF ++ asm2, stateF.copy(reg = state2.reg))
    })

    def instr1Aux[T1 <: AsmArg, T](f: T1 => T => Step)(arg1: ResolutionData => T1)(aux: T)(out: AsmIndefReg *): Step = {
      instr1[T1](f(_)(aux))(arg1)(out: _*)
    }

    def instr2Aux[T1 <: AsmArg, T2 <: AsmArg, T](f: (T1, T2) => T => Step)(arg1: ResolutionData => T1, arg2: ResolutionData => T2)(aux: T)(out: AsmIndefReg *): Step = {
      instr2[T1, T2](f(_, _)(aux))(arg1, arg2)(out: _*)
    }

    def instr3Aux[T1 <: AsmArg, T2 <: AsmArg, T3 <: AsmArg, T](f: (T1, T2, T3) => T => Step)(arg1: ResolutionData => T1, arg2: ResolutionData => T2, arg3: ResolutionData => T3)(aux: T)(out: AsmIndefReg *): Step = {
      instr3[T1, T2, T3](f(_, _, _)(aux))(arg1, arg2, arg3)(out: _*)
    }

    def instr4Aux[T1 <: AsmArg, T2 <: AsmArg, T3 <: AsmArg, T4 <: AsmArg, T](f: (T1, T2, T3, T4) => T => Step)(arg1: ResolutionData => T1, arg2: ResolutionData => T2, arg3: ResolutionData => T3, arg4: ResolutionData => T4)(aux: T)(out: AsmIndefReg *): Step = {
      val fAux: (T1, T2, T3, T4) => Step = f(_, _, _, _)(aux)
      instr4[T1, T2, T3, T4](f(_, _, _, _)(aux))(arg1, arg2, arg3, arg4)(out: _*)
    }

  }
  object implicits {
    implicit def implicitStep(node: Asm): Step = Step((List(node), _))
  }
}

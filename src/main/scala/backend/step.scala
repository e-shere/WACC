package backend

import backend.asm._
import backend.state.{NEW_REG, REG_START, State}

import scala.language.implicitConversions

object step {
  import implicits._

  case class Step(func: State => (List[Asm], State)) {
    override def toString = mkString("\n")

    def mkString(sep: String): String = this (NEW_REG)._1.mkString(sep)

    def apply(state: State): (List[Asm], State) = func(state)

    def <++>(next: Step): Step = Step((state: State) => {
      val (asm1, state1) = this (state)
      val (asm2, state2) = next(state1)
      (asm1 ++ asm2, state2)
    })
  }

  object Step {

    val identity: Step = Step((Nil, _))
    // This step is used between steps where the state of registers needs to be reset
    val discardAll: Step = Step(state => (Nil, State(REG_START, state.fState)))

    val discardTop: Step = Step(state => (Nil, state.prev))

    def stepInstr(f: (Seq[AsmDefiniteArg]) => Step)(args: AsmArg *)(out: AsmAnyReg *): Step = Step((state: State) => {
      // TODO: remove this?
      val outSet = out.toSet
      
      val (re1, re2, asm1, state1) = 
        if (args contains Re2) state.read2
        else if (args contains Re1) {
          val result = state.read
          (result._1, NO_REG, result._2, result._3)
        }
        else (NO_REG, NO_REG, Nil, state)

      val (re1w, asm2, state2) = if (out contains Re1) {
        assert(args contains Re1)
        state1.write 
      } else (NO_REG, asm1, state1)

      assert (re1 == re1w)
      
      val (re2w, asm3, state3) = if (out contains Re2) {
        assert(args contains Re2)
        state1.write 
      } else (NO_REG, asm2, state2)

      assert (re2 == re2w)

      val (reNew, asm4, state4) = if (args contains ReNew) state1.write else (NO_REG, asm1, state1)

      val argsDefinite: Seq[AsmDefiniteArg] = args.map {
        case Re1 => re1
        case Re2 => re2
        case ReNew => reNew
        case arg: AsmDefiniteArg => arg
      }

      val (asmF, stateF) = f.apply(argsDefinite)(state1)

      assert(state1 == stateF)

      (asm1 ++ asmF ++ asm2 ++ asm3 ++ asm4, state4)
    })

    def asmInstr(f: Seq[AsmDefiniteArg] => Asm)(args: AsmArg *)(out: AsmAnyReg *): Step = stepInstr((x: Seq[AsmDefiniteArg]) => f(x))(args: _*)(out: _*)

    def instr[T](f: (Seq[AsmDefiniteArg], T) => Step)(args: AsmArg *)(aux: T)(out: AsmAnyReg *): Step = 
      stepInstr(f(_, aux))(args: _*)(out: _*)
  }

  object implicits {
    implicit def implicitStep(node: Asm): Step = Step((List(node), _))
  }
}

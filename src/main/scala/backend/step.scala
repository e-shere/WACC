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
    // TODO: don't discard function state!
    val discardAll: Step = Step(state => (Nil, State(REG_START, state.fState)))

    val discardTop: Step = Step(state => (Nil, state.prev))

    // Read from register, then free the register
    def r(fs: (AsmReg) => Asm*): Step = Step((state: State) => {
      val (reg, asm1, state1) = state.read
      (asm1 ++ fs.map(_ (reg)), state1)
    })

    // Write to a new register
    def w(f: (AsmReg) => Asm): Step = Step((state: State) => {
      val (reg, asm1, state1) = state.write
      (f(reg) +: asm1, state1)
    })

    // Read from a register and overwrite it
    def ro(fs: (AsmReg) => Asm*): Step = Step((state: State) => {
      val (reg1, asm1, state1) = state.read
      val (reg2, asm2, state2) = state1.write
      assert(reg1 == reg2)
      (asm1 ++ fs.map(_ (reg1)) ++ asm2, state2)
    })

    // Read from a register, preserve it, write to a new one
    def rw(fs: (AsmReg, AsmReg) => Asm*): Step = Step((state: State) => {
      val (reg1, asm1, state1) = state.peek
      val (reg2, asm2, state2) = state1.write
      (asm1 ++ fs.map(_ (reg2, reg1)) ++ asm2, state2)
    })

    // Read two registers, free one and overwrite the other
    def rro(fs: (AsmReg, AsmReg) => Asm*): Step = Step((state: State) => {
      val (xReg, yReg, asm1, state1) = state.read2
      val (tReg, asm2, state2) = state1.write
      assert(xReg == tReg)
      (asm1 ++ fs.map(_ (xReg, yReg)) ++ asm2, state2)
    })

    // Read a register but do not discard its value afterwards
    // Note that the output Asm CANNOT change the register
    def p(fs: (AsmReg) => Asm*): Step = Step((state: State) => {
      val (xReg, asm1, state1) = state.peek
      (asm1 ++ fs.map(_ (xReg)) ++ asm1, state1)
    })

    // WARNING!
    // Read two registers
    // Can only be used with fs that do not modify registers at all i.e. store
    def pp(fs: (AsmReg, AsmReg) => Asm*): Step = Step((state: State) => {
      val (xReg, yReg, asm1, state1) = state.peek2
      (asm1 ++ fs.map(_ (xReg, yReg)) ++ asm1, state1)
    })
  }

  sealed trait StepBuilder

  trait OutImm extends StepBuilder {
    def apply(out: AsmReg, in: AsmDefiniteArg): Asm

    def step(out: AsmMaybeReg, in: AsmInt): Step = out match {
      case outReg@AsmReg(_) => apply(outReg, in) // uses implicit conversion
      case AsmAnyReg(0) => Step.w(apply(_, in))
    }
  }

  trait OutIn extends StepBuilder {
    def apply(out: AsmReg, in: AsmDefiniteArg): Asm

    def step(out: AsmMaybeReg, in: AsmMaybeReg): Step = (out, in) match {
      case (AsmAnyReg(0), inReg@AsmReg(_)) => Step.w(apply(_, inReg))
      case (outReg@AsmReg(_), AsmAnyReg(0)) => Step.r(apply(outReg, _))
      case (AsmAnyReg(0), AsmAnyReg(0)) => Step.ro(reg => apply(reg, reg))
      case (AsmAnyReg(1), AsmAnyReg(0)) => Step.rw(apply(_, _))
    }
  }

  trait OutInImm extends StepBuilder {
    def apply(out: AsmReg, in1: AsmReg, in2: AsmDefiniteArg): Asm

    def step(out: AsmMaybeReg, in1: AsmMaybeReg, in2: AsmInt): Step = (out, in1) match {
      case (AsmAnyReg(0), AsmAnyReg(0)) => Step.ro(reg => apply(reg, reg, in2))
      case (AsmAnyReg(1), AsmAnyReg(0)) => Step.rw(apply(_, _, in2))
    }
  }

  trait OutInIn extends StepBuilder {
    def apply(out: AsmReg, in1: AsmReg, in2: AsmDefiniteArg): Asm

    def step(out: AsmMaybeReg, in1: AsmMaybeReg, in2: AsmMaybeReg): Step = (out, in1, in2) match {
      case (AsmAnyReg(0), AsmAnyReg(0), AsmAnyReg(0)) => Step.ro(reg => apply(reg, reg, reg))
      case (AsmAnyReg(0), AsmAnyReg(0), AsmAnyReg(1)) => Step.rro((tReg, yReg) => apply(tReg, tReg, yReg))
      case (AsmAnyReg(0), AsmAnyReg(1), AsmAnyReg(0)) => Step.rro((tReg, xReg) => apply(tReg, xReg, tReg))
      case (AsmAnyReg(1), AsmAnyReg(0), AsmAnyReg(0)) => Step.rw((tReg, xReg) => apply(tReg, xReg, xReg))
      //case (AsmAnyReg(1), AsmAnyReg(0), AsmAnyReg(1)) => Step.pp((xReg, yReg) => apply(yReg, xReg, yReg))
    }
  }

  trait InImm extends StepBuilder {
    def apply(in1: AsmReg, in2: AsmDefiniteArg): Asm

    def step(in1: AsmMaybeReg, in2: AsmInt): Step = in1 match {
      case (in1Reg@AsmReg(_)) => apply(in1Reg, in2)
      case AsmAnyReg(0) => Step.p(apply(_, in2))
    }
  }

  trait InIn extends StepBuilder {
    def apply(in1: AsmReg, in2: AsmDefiniteArg): Asm

    def step(in1: AsmMaybeReg, in2: AsmMaybeReg): Step = (in1, in2) match {
      case (AsmAnyReg(0), in2Reg@AsmReg(_)) => Step.r(apply(_, in2Reg))
      case (in1Reg@AsmReg(_), AsmAnyReg(0)) => Step.r(apply(in1Reg, _))
      case (AsmAnyReg(0), AsmAnyReg(0)) => Step.p(reg => apply(reg, reg))
      case (AsmAnyReg(0), AsmAnyReg(1)) => Step.pp(apply(_, _))
      case (AsmAnyReg(1), AsmAnyReg(0)) => Step.pp((x, y) => apply(y, x))
    }
  }

  trait InInImm extends StepBuilder {
    def apply(in1: AsmReg, in2: AsmReg, in3: AsmDefiniteArg): Asm

    def step(in1: AsmMaybeReg, in2: AsmMaybeReg, in3: AsmInt): Step = (in1, in2) match {
      case (AsmAnyReg(0), in2Reg@AsmReg(_)) => Step.r(apply(_, in2Reg, in3))
      case (in1Reg@AsmReg(_), AsmAnyReg(0)) => Step.r(apply(in1Reg, _, in3))
      case (AsmAnyReg(0), AsmAnyReg(0)) => Step.p(reg => apply(reg, reg, in3))
      case (AsmAnyReg(0), AsmAnyReg(1)) => Step.pp(apply(_, _, in3))
      case (AsmAnyReg(1), AsmAnyReg(0)) => Step.pp((x, y) => apply(y, x, in3))
    }
  }

  trait InInIn extends StepBuilder {
    def apply(in1: AsmReg, in2: AsmReg, in3: AsmDefiniteArg): Asm

    //TODO: complete
    def step(in1: AsmMaybeReg, in2: AsmMaybeReg, in3: AsmMaybeReg): Step = (in1, in2) match {
      //case (AsmAnyReg(0), in2Reg @ AsmReg(_)) => Step.r(apply(_, in2Reg))
      //case (in1Reg @ AsmReg(_), AsmAnyReg(0)) => Step.r(apply(in1Reg, _))
      //case (AsmAnyReg(0), AsmAnyReg(0))        => Step.p(reg => apply(reg, reg))
      //case (AsmAnyReg(0), AsmAnyReg(1))        => Step.pp(apply(_, _))
      //case (AsmAnyReg(1), AsmAnyReg(0))        => Step.pp((x, y) => apply(y, x))
      case _ => ???
    }
  }

  object implicits {
    implicit def implicitStep(node: Asm): Step = Step((List(node), _))
  }
}

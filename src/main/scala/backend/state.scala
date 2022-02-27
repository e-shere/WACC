package backend

import asm._
import scala.language.implicitConversions
import scala.util.matching.Regex

object state {
  import implicits._

  val REG_START = AsmReg(4)
  val REG_END = AsmReg(9)
  val PLACEHOLDER_1 = AsmReg(10)
  val PLACEHOLDER_2 = AsmReg(11)
  val STACK_POINTER = AsmReg(13)

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
  
  case class Step(func: RegState => (List[Asm], RegState)) {
    override def toString = mkString("\n")

    def mkString(sep: String): String = this(NEW_REG)._1.mkString(sep) 

    def apply(state: RegState): (List[Asm], RegState) = func(state)

    def <++>(next: Step): Step = Step((state: RegState) => {
      val (asm1, state1) = this(state)
      val (asm2, state2) = next(state1)
      (asm1 ++ asm2, state2)
    })
  }

  object Step {
    val identity: Step = Step((Nil, _))
    // This step is used between steps where the state of registers needs to be reset
    val discard: Step = Step(_ => (Nil, NEW_REG))

    // Read from register, then free the register
    def r(fs: (AsmReg) => Asm *): Step = Step((state: RegState) => {
      val (reg, asm1, state1) = state.read
      (asm1 ++ fs.map(_(reg)), state1)
    })
    
    // Write to a new register
    def w(f: (AsmReg) => Asm): Step = Step((state: RegState) => {
      val (reg, asm1, state1) = state.write
      (f(reg) +: asm1, state1)
    })

    // Read from a register and overwrite it
    def ro(fs: (AsmReg) => Asm *): Step = Step((state: RegState) => {
      val (reg1, asm1, state1) = state.read
      val (reg2, asm2, state2) = state1.write
      assert(reg1 == reg2)
      (asm1 ++ fs.map(_(reg1)) ++ asm2, state2)
    })

    // Read from a register, preserve it, write to a new one
    def rw(fs: (AsmReg, AsmReg) => Asm *): Step = Step((state: RegState) => {
      val (reg1, asm1, state1) = state.peek
      val (reg2, asm2, state2) = state1.write
      (asm1 ++ fs.map(_(reg2, reg1)) ++ asm2, state2)
    })

    // Read two registers, free one and overwrite the other
    def rro(fs: (AsmReg, AsmReg) => Asm *): Step = Step((state: RegState) => {
      val (xReg, yReg, asm1, state1) = state.read2
      val (tReg, asm2, state2) = state1.write
      assert(xReg == tReg)
      (asm1 ++ fs.map(_(xReg, yReg)) ++ asm2, state2)
    })

    def p(fs: (AsmReg) => Asm *): Step = Step((state: RegState) => {
      val (xReg, asm1, state1) = state.peek
      (asm1 ++ fs.map(_(xReg)) ++ asm1, state1)
    })

    // WARNING!
    // Can only be used with fs that do not modify registers at all i.e. store
    def pp(fs: (AsmReg, AsmReg) => Asm *): Step = Step((state: RegState) => {
      val (xReg, yReg, asm1, state1) = state.peek2
      (asm1 ++ fs.map(_(xReg, yReg)) ++ asm1, state1)
    })
  }

  sealed trait StepBuilder

  //trait R extends StepBuilder {
  //  def apply(reg: AsmReg): Asm

  //  def r: Step = Step.r(apply)
  //}

  //trait W extends StepBuilder {
  //  def apply(reg: AsmReg): Asm

  //  def w: Step = Step.w(apply)
  //}

  //trait RW extends StepBuilder {
  //  def apply(target: AsmReg, reg: AsmReg): Asm

  //  def rw: Step = Step.rw(apply)
  //}

  //trait RO extends StepBuilder {
  //  def apply(reg: AsmReg): Asm

  //  def ro: Step = Step.ro(apply)
  //}

  //trait RRO1 extends StepBuilder {
  //  def apply(x: AsmReg, y: AsmReg): Asm

  //  def rro1: Step = Step.rro(apply)
  //}

  //trait P extends StepBuilder {
  //  def apply(reg: AsmReg): Asm

  //  def p: Step = Step.p(apply)
  //}

  //trait PP extends StepBuilder {
  //  def apply(x: AsmReg, y: AsmReg): Asm

  //  def pp: Step = Step.pp(apply)
  //}

  //trait RRO2 extends StepBuilder {
  //  def apply(x: AsmReg, y: AsmReg): Asm

  //  def rro2: Step = Step.rro(apply) <++> Mov.rro1
  //}
  //

  trait OutImm extends StepBuilder {
    def apply(out: AsmReg, in: AsmDefiniteArg): Asm

    def step(out: AsmMaybeReg, in: AsmInt): Step = out match {
      case outReg @ AsmReg(_)    => apply(outReg, in) // uses implicit conversion
      case AsmAnyReg(0) => Step.w(apply(_, in))
    }
  }

  trait OutIn extends StepBuilder {
    def apply(out: AsmReg, in: AsmDefiniteArg): Asm

    def step(out: AsmMaybeReg, in: AsmMaybeReg): Step = (out, in) match {
      case (AsmAnyReg(0), inReg @ AsmReg(_))    => Step.w(apply(_, inReg))
      case (outReg @ AsmReg(_),    AsmAnyReg(0)) => Step.w(apply(outReg, _))
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
      case (in1Reg @ AsmReg(_)) => apply(in1Reg, in2)
      case AsmAnyReg(0)          => Step.p(apply(_, in2))
    }
  }

  trait InIn extends StepBuilder {
    def apply(in1: AsmReg, in2: AsmDefiniteArg): Asm

    def step(in1: AsmMaybeReg, in2: AsmMaybeReg): Step = (in1, in2) match {
      case (AsmAnyReg(0), in2Reg @ AsmReg(_)) => Step.r(apply(_, in2Reg))
      case (in1Reg @ AsmReg(_), AsmAnyReg(0)) => Step.r(apply(in1Reg, _))
      case (AsmAnyReg(0), AsmAnyReg(0))        => Step.p(reg => apply(reg, reg))
      case (AsmAnyReg(0), AsmAnyReg(1))        => Step.pp(apply(_, _))
      case (AsmAnyReg(1), AsmAnyReg(0))        => Step.pp((x, y) => apply(y, x))
    }
  }

  trait InInImm extends StepBuilder {
    def apply(in1: AsmReg, in2: AsmReg, in3: AsmDefiniteArg): Asm

    def step(in1: AsmMaybeReg, in2: AsmMaybeReg, in3: AsmInt): Step = (in1, in2) match {
      case (AsmAnyReg(0), in2Reg @ AsmReg(_)) => Step.r(apply(_, in2Reg, in3))
      case (in1Reg @ AsmReg(_), AsmAnyReg(0)) => Step.r(apply(in1Reg, _, in3))
      case (AsmAnyReg(0), AsmAnyReg(0))        => Step.p(reg => apply(reg, reg, in3))
      case (AsmAnyReg(0), AsmAnyReg(1))        => Step.pp(apply(_, _, in3))
      case (AsmAnyReg(1), AsmAnyReg(0))        => Step.pp((x, y) => apply(y, x, in3))
    }
  }

  trait InInIn extends StepBuilder {
    def apply(in1: AsmReg, in2: AsmReg, in3: AsmDefiniteArg): Asm

    def step(in1: AsmMaybeReg, in2: AsmMaybeReg, in3: AsmMaybeReg): Step = (in1, in2) match {
      //case (AsmAnyReg(0), in2Reg @ AsmReg(_)) => Step.r(apply(_, in2Reg))
      //case (in1Reg @ AsmReg(_), AsmAnyReg(0)) => Step.r(apply(in1Reg, _))
      //case (AsmAnyReg(0), AsmAnyReg(0))        => Step.p(reg => apply(reg, reg))
      //case (AsmAnyReg(0), AsmAnyReg(1))        => Step.pp(apply(_, _))
      //case (AsmAnyReg(1), AsmAnyReg(0))        => Step.pp((x, y) => apply(y, x))
      case _ => ???
    }
  }


  val NEW_REG: RegState = RegState(REG_START)


  object implicits {
    implicit def implicitStep(node: Asm): Step = Step((List(node), _))
  }

}

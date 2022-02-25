package backend

import asm._
import scala.language.implicitConversions

object state {

  val REG_START = 4
  val REG_END = 9
  val PLACEHOLDER_1 = "r10"
  val PLACEHOLDER_2 = "r11"
  val STACK_POINTER = "sp"

  /*
  Reg documents the highest register of 4-9 which is not in use
  If reg > 9, reg documents the number of things in the stack + REG_END + 1
   */
  case class RegState(reg: Int) {
    def isReg: Boolean = reg >= REG_START && reg <= REG_END + 1
    def isStack: Boolean = reg > REG_END
    def prev: RegState = RegState(reg - 1)
    def next: RegState = RegState(reg + 1)
    def read: (String, List[Asm], RegState) = {
      if (isReg) (regToString(reg), Nil, prev)
      else (PLACEHOLDER_1, List(Pop(PLACEHOLDER_1)), prev)
    }
    def read2: (String, String, List[Asm], RegState) = {
      if (isReg) (regToString(prev.reg), regToString(reg), Nil, prev.prev)
      else if (prev.isReg) (regToString(prev.reg), PLACEHOLDER_1, List(Pop(PLACEHOLDER_1)), prev.prev)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(Pop(PLACEHOLDER_2), Pop(PLACEHOLDER_1)), prev.prev)
    }
    def peek: (String, List[Asm], RegState) = {
      if (isReg) (regToString(reg), Nil, this)
      else (PLACEHOLDER_1, List(new Ldr(PLACEHOLDER_1, STACK_POINTER)()), this)
    }
    def peek2: (String, String, List[Asm], RegState) = {
      if (isReg) (regToString(prev.reg), regToString(reg), Nil, this)
      else if (prev.isReg) (regToString(prev.reg), PLACEHOLDER_1, List(new Ldr(PLACEHOLDER_1, STACK_POINTER)()), this)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(new Ldr(PLACEHOLDER_2, STACK_POINTER)(), Ldr(PLACEHOLDER_1, STACK_POINTER)(intToAsmLit(4))), this)
    }
    def write: (String, List[Asm], RegState) = {
      if (isReg) (regToString(next.reg), Nil, next)
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
    def r(fs: (String) => Asm *): Step = Step((state: RegState) => {
      val (reg, asm1, state1) = state.read
      (asm1 ++ fs.map(_(reg)), state1)
    })
    
    // Write to a new register
    def w(f: (String) => Asm): Step = Step((state: RegState) => {
      val (reg, asm1, state1) = state.write
      (f(reg) +: asm1, state1)
    })

    // Read from a register and overwrite it
    def ro(fs: (String) => Asm *): Step = Step((state: RegState) => {
      val (reg1, asm1, state1) = state.read
      val (reg2, asm2, state2) = state1.write
      assert(reg1 == reg2)
      (asm1 ++ fs.map(_(reg1)) ++ asm2, state2)
    })

    // Read from a register, preserve it, write to a new one
    def rw(fs: (String, String) => Asm *): Step = Step((state: RegState) => {
      val (reg1, asm1, state1) = state.peek
      val (reg2, asm2, state2) = state1.write
      (asm1 ++ fs.map(_(reg2, reg1)) ++ asm2, state2)
    })

    // Read two registers, free one and overwrite the other
    def rro(fs: (String, String) => Asm *): Step = Step((state: RegState) => {
      val (xReg, yReg, asm1, state1) = state.read2
      val (tReg, asm2, state2) = state1.write
      assert(xReg == tReg)
      (asm1 ++ fs.map(_(xReg, yReg)) ++ asm2, state2)
    })
  }

  // Compose several steps into one
  //def combineSteps(steps: List[Step]): Step = (state: RegState) => {
  //  steps.foldLeft[(List[Asm], RegState)]((Nil, state))((prev, step) => prev match {
  //    case (asm1, state1) => {
  //      val (asm2, state2) = step(state1)
  //      (asm1 ++ asm2, state2)
  //    }
  //  })
  //}
  
  def regToString(reg: Int): String = "r" + reg

  val NEW_REG: RegState = RegState(REG_START)


  object implicits {
    implicit def implicitStep(node: Asm): Step = Step((List(node), _))
  }

}

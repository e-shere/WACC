package backend

import asm._
import backend.PredefinedFunctions.PredefinedFunc
import backend.step.Step
import backend.step.implicits.implicitStep
import frontend.symbols.TypeTable

import scala.language.implicitConversions

object state {

  val REG_START = AsmReg(4)
  val REG_END = AsmReg(9)
  val PLACEHOLDER_1 = AsmReg(10)
  val PLACEHOLDER_2 = AsmReg(11)
  val STACK_POINTER = AsmReg(13)
  val NEW_REG: State = State(REG_START, Set())

  /*
  Reg documents the highest register of 4-9 which is not in use
  If reg > 9, reg documents the number of things in the stack + REG_END + 1
   */
  // TODO: ROB PLEASE DO SOME OFF BY ONE CHECKS HERE
  // TODO: unit test this

  type FuncState = Set[PredefinedFunc]

  case class State(reg: AsmReg, fState: FuncState) {

    def isReg: Boolean = reg.r >= REG_START.r && reg.r <= REG_END.r
    def isStack: Boolean = reg.r > REG_END.r
    def prev: State = State(AsmReg(reg.r - 1), this.fState)
    def next: State = State(AsmReg(reg.r + 1), this.fState)
    def read: (AsmReg, List[Asm], State) = {
      if (prev.isReg) (prev.reg, Nil, prev)
      else (PLACEHOLDER_1, List(Pop()(PLACEHOLDER_1)), prev)
    }
    def read2: (AsmReg, AsmReg, List[Asm], State) = {
      if (isReg) (prev.reg, reg, Nil, prev.prev)
      else if (prev.isReg) (prev.reg, PLACEHOLDER_1, List(Pop()(PLACEHOLDER_1)), prev.prev)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(Pop()(PLACEHOLDER_2), Pop()(PLACEHOLDER_1)), prev.prev)
    }
    def peek: (AsmReg, List[Asm], State) = {
      if (isReg) (prev.reg, Nil, this)
      // TODO: added 0 offset here. Check
      else (PLACEHOLDER_1, List(Ldr()(PLACEHOLDER_1, STACK_POINTER)(AsmInt(0))), this)
    }
    def peek2: (AsmReg, AsmReg, List[Asm], State) = {
      if (isReg) (prev.reg, reg, Nil, this)
      // TODO: added 0 offset here. Check
      else if (prev.isReg) (prev.reg, PLACEHOLDER_1, List(Ldr()(PLACEHOLDER_1, STACK_POINTER)(AsmInt(0))), this)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(Ldr()(PLACEHOLDER_2, STACK_POINTER)(AsmInt(0)), Ldr()(PLACEHOLDER_1, STACK_POINTER)(AsmInt(4))), this)
    }
    def write: (AsmReg, List[Asm], State) = {
      if (isReg) (reg, Nil, next)
      // TODO: added 0 offset here. Check
      else (PLACEHOLDER_1, List(Push()(PLACEHOLDER_1)), next)
    }
  }
}

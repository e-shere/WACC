package backend

import asm._
import backend.PredefinedFunctions.PredefinedFunc

import scala.language.implicitConversions

object state {

  // Consider factoring out the magic numbers
  val NUM_BASE_REG = 9
  val REG_START = AsmReg(4)
  val REG_END = AsmReg(NUM_BASE_REG)
  val PLACEHOLDER_1 = AsmReg(10)
  val PLACEHOLDER_2 = AsmReg(11)
  val STACK_POINTER = AsmReg(13)
  val NEW_REG: State = State(REG_START, Set(), Map.empty)

  /*
  Reg documents the highest register of 4-9 which is not in use
  If reg > 9, reg documents the number of things in the stack + REG_END + 1
   */
  // TODO: ROB PLEASE DO SOME OFF BY ONE CHECKS HERE
  // TODO: unit test this

  type FuncState = Set[PredefinedFunc]
  // Maps from the string value we want to insert to
  type StringData = Map[String, AsmString]

  case class State(reg: AsmReg, fState: FuncState, data: StringData) {

    def isReg: Boolean = reg.r >= REG_START.r && reg.r <= REG_END.r

    def isStack: Boolean = reg.r > REG_END.r

    def prev: State = State(AsmReg(reg.r - 1), this.fState, this.data)

    def next: State = State(AsmReg(reg.r + 1), this.fState, this.data)

    def getStackOffset: Int = if (reg.r - NUM_BASE_REG > 0) reg.r - NUM_BASE_REG else 0

    def read: (AsmReg, List[Asm], State) = {
      assert(reg.r > 4)
      if (prev.isReg) (prev.reg, Nil, prev)
      else (PLACEHOLDER_1, List(Pop()(PLACEHOLDER_1)), prev)
    }

    def read2: (AsmReg, AsmReg, List[Asm], State) = {
      assert(reg.r > 5)
      if (prev.isReg) (prev.prev.reg, prev.reg, Nil, prev.prev)
      else if (prev.prev.isReg) (prev.prev.reg, PLACEHOLDER_1, List(Pop()(PLACEHOLDER_1)), prev.prev)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(Pop()(PLACEHOLDER_2), Pop()(PLACEHOLDER_1)), prev.prev)
    }

    def write: (AsmReg, List[Asm], State) = {
      if (isReg) (reg, Nil, next)
      else (PLACEHOLDER_1, List(Push()(PLACEHOLDER_1)), next)
    }

    def write2: (AsmReg, AsmReg, List[Asm], State) = {
      if (next.isReg) (reg, next.reg, Nil, next.next)
      else if (isReg) (reg, PLACEHOLDER_1, List(Push()(PLACEHOLDER_1)), next.next)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(Push()(PLACEHOLDER_1), Push()(PLACEHOLDER_2)), next.next)
    }
  }
}

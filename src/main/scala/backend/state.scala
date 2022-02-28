package backend

import asm._
import backend.PredefinedFunctions.PredefinedFunc
import backend.step.Step
import backend.step.implicits.implicitStep

import scala.language.implicitConversions

object state {

  val REG_START = AsmReg(4)
  val REG_END = AsmReg(9)
  val PLACEHOLDER_1 = AsmReg(10)
  val PLACEHOLDER_2 = AsmReg(11)
  val STACK_POINTER = AsmReg(13)
  val NEW_REG: RegState = RegState(REG_START)

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

  case class funcState() {

    val includedFuncs: Set[PredefinedFunc] = Set.empty

    def getPredefFuncs(): Step = {
      includedFuncs.foldLeft(Step.identity)(
        (prev, f) => prev <++> f.toStep
      )
    }

  }

}

package backend

import frontend.ast._
import asm._

object generator {

  val REG_START = 4
  val REG_END = 9
  val PLACEHOLDER_START = 10
  val PLACEHOLDER_END = 11

  /*
  Reg documents the highest register of 4-9 which is not in use
  Placeholder documents the lowest register of 10-11 which is not in use
  If reg > 9, reg documents the number of things in the stack + REG_END + 1
   */
  case class RegState(reg: Int, placeholder: Int) {
    def isReg: Boolean = reg >= REG_START && reg <= REG_END + 1
    def isStack: Boolean = reg > REG_END
    def prev: RegState = RegState(reg - 1, placeholder)
    def next: RegState = RegState(reg + 1, placeholder)
    def read: (RegState, String, List[Asm]) = if (isReg) (prev, "r" + reg, Nil) else {
      (RegState(reg + 1, placeholder - 1), "r" + placeholder, List(/* pop into rplaceholder */))
    }
    def write: (RegState, String, List[Asm]) = if (isReg) (next, "r" + reg, Nil) else {
      (RegState(reg - 1, PLACEHOLDER_END), "r" + PLACEHOLDER_END, List(/* push from r11 */))
    }
  }

  val NEW_REG = RegState(REG_START, PLACEHOLDER_END)

  def genProgram(program: WaccProgram): List[Asm] = program match {
    case WaccProgram(funcs, stats) => {
      funcs.flatMap(f => genFunc(f.id.id, f.args.length, f.body)) ++ genFunc("main", 0, stats)
    }
  }

  def genFunc(name: String, argc: Int, stats: List[Stat]): List[Asm] = {
    Label(name) +: genStats(stats) :+ Directive("ltorg")
  }

  def genStats(stats: List[Stat]): List[Asm] = {
    stats.flatMap(genStat(_))
  }

  def genStat(stat: Stat): List[Asm] = {
    stat match {
      case Skip() => Nil
      case stat@Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => Nil
      case Read(lhs) => Nil
      case Free(expr) => Nil
      case Return(expr) => Nil
      case Exit(expr) => Nil
      case Print(expr) => Nil
      case Println(expr) => Nil
      case If(expr, thenStats, elseStats) => Nil
      case While(expr, doStats) => Nil
      case Scope(stats) => genStats(stats)
    } 
  }
}

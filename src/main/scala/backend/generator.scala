package backend

import frontend.ast
import frontend.ast._
import asm._

object generator {

  val REG_START = 4
  val REG_END = 9
  val PLACEHOLDER_1 = 10
  val PLACEHOLDER_2 = 11

  /*
  Reg documents the highest register of 4-9 which is not in use
  If reg > 9, reg documents the number of things in the stack + REG_END + 1
   */
  case class RegState(reg: Int) {
    def isReg: Boolean = reg >= REG_START && reg <= REG_END + 1
    def isStack: Boolean = reg > REG_END
    def prev: RegState = RegState(reg - 1)
    def next: RegState = RegState(reg + 1)
    def read: (String, List[Asm], RegState) = if (isReg) (regToString(reg), Nil, prev) else {
      ("r" + PLACEHOLDER_1, List(Pop(regToString(PLACEHOLDER_1))), prev)
    }
    def read2: (String, String, List[Asm], RegState) = {
      if (isReg) (regToString(prev.reg), regToString(reg), Nil, prev.prev)
      else if (prev.isReg) (regToString(prev.reg), regToString(PLACEHOLDER_1), List(Pop(regToString(PLACEHOLDER_1))), prev.prev)
      else (regToString(PLACEHOLDER_1), regToString(PLACEHOLDER_2), List(Pop(regToString(PLACEHOLDER_2)), Pop(regToString(PLACEHOLDER_1))), prev.prev)
    }
    def write: (String, List[Asm], RegState) = if (isReg) (regToString(reg), Nil, next) else {
      ("r" + PLACEHOLDER_1, List(Push(regToString(PLACEHOLDER_1))), next)
    }
  }
  
  def regToString(reg: Int) = "r" + reg

  val NEW_REG = RegState(REG_START)

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
    implicit val initialState: RegState = NEW_REG
    stat match {
      case Skip() => Nil
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => {
        val (rhsAsm, state) = genRhs(rhs)(NEW_REG)
        val lhsAsm = genLhs(lhs)(state)
        rhsAsm ++ lhsAsm
      }
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

  def genExpr2(x: Expr, y: Expr)(implicit state: RegState): (String, String, List[Asm], RegState) = {
      val (asm1, state1) = genExpr(x)
      val (asm2, state2) = genExpr(y)(state1)
      val (yReg, asm3, state3) = state2.read
      val (xReg, asm4, state4) = state3.read
      (xReg, yReg, asm1 ++ asm2 ++ asm3 ++ asm4, state4)
  }

  def genBinOp(x: Expr, y: Expr, f: (String, String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    val (xReg, yReg, list, newState) = genExpr2(x, y)
    (list :+ f(xReg, yReg), newState)
  }

  def genUnOp(x: Expr, f: (String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    val (asm1, state1) = genExpr(x)
    val (xReg, asm2, state2) = state1.read
    (asm1 ++ asm2 :+ f(xReg), state2)
  }

  def genExpr(expr: Expr)(implicit state: RegState): (List[Asm], RegState) = expr match {
    case ast.Or(x, y)  => genBinOp(x, y, new asm.Or(_, _))
    case ast.And(x, y) => genBinOp(x, y, new asm.And(_, _)) 
    case ast.Eq(x, y)  => genBinOp(x, y, new asm.Eq(_, _)) 
    case ast.Neq(x, y) => genBinOp(x, y, new asm.Neq(_, _)) 
    case ast.Leq(x, y) => genBinOp(x, y, new asm.Leq(_, _)) 
    case ast.Lt(x, y)  => genBinOp(x, y, new asm.Lt(_, _)) 
    case ast.Geq(x, y) => genBinOp(x, y, new asm.Geq(_, _)) 
    case ast.Gt(x, y)  => genBinOp(x, y, new asm.Gt(_, _)) 
    case ast.Add(x, y) => genBinOp(x, y, new asm.Add(_, _)) 
    case ast.Sub(x, y) => genBinOp(x, y, new asm.Sub(_, _)) 
    case ast.Mul(x, y) => genBinOp(x, y, new asm.Mul(_, _)) 
    case ast.Div(x, y) => genBinOp(x, y, new asm.Div(_, _)) 
    case ast.Mod(x, y) => genBinOp(x, y, new asm.Mod(_, _)) 
    case ast.Not(x)    => genUnOp(x, new asm.Not(_))
    case ast.Neg(x)    => genUnOp(x, new asm.Neg(_))
    case ast.Len(x)    => genUnOp(x, new asm.Len(_))
    case ast.Ord(x)    => genUnOp(x, new asm.Ord(_))
    case ast.Chr(x)    => genUnOp(x, new asm.Chr(_))
  }

  def genRhs(rhs: AssignRhs)(implicit state: RegState): (List[Asm], RegState) = rhs match {
    case ArrayLiter(exprs) => {
      // r0 := exprs.length
      // malloc
      // rn := state.write
      // mov rn r0
      // for expr, i in exprs: str expr [rn + i]
      // return rn
      ???
    }
    case NewPair(fst, snd) => {
      ???
    }
    case Fst(expr) => ???
    case Snd(expr) => ???
    case Call(id, args) => ???
    case expr: Expr => genExpr(expr)
  }

  def genLhs(lhs: AssignLhs)(implicit state: RegState): List[Asm] = {
    ???
  }
}

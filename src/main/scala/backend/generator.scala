package backend

import frontend.ast
import frontend.ast._
import asm._

object generator {

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
      else (PLACEHOLDER_1, List(new Ldr(PLACEHOLDER_1, STACK_POINTER)), this)
    }
    def peek2: (String, String, List[Asm], RegState) = {
      if (isReg) (regToString(prev.reg), regToString(reg), Nil, this)
      else if (prev.isReg) (regToString(prev.reg), PLACEHOLDER_1, List(new Ldr(PLACEHOLDER_1, STACK_POINTER)), this)
      else (PLACEHOLDER_1, PLACEHOLDER_2, List(new Ldr(PLACEHOLDER_2, STACK_POINTER), Ldr(PLACEHOLDER_1, STACK_POINTER, intToAsmLit(4))), this)
    }
    def write: (String, List[Asm], RegState) = {
      if (isReg) (regToString(next.reg), Nil, next)
      else (PLACEHOLDER_1, List(Push(PLACEHOLDER_1)), next)
    }
  }
  
  type Step = RegState => (List[Asm], RegState)

  def r(f: (String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    val (reg, asm1, state1) = state.read
    (asm1 :+ f(reg), state1)
  }
  
  def w(f: (String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    val (reg, asm1, state1) = state.write
    (f(reg) +: asm1, state1)
  }

  def rw(f: (String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    val (reg1, asm1, state1) = state.read
    val (reg2, asm2, state2) = state1.write
    assert(reg1 == reg2)
    (asm1 :+ f(reg1) :++ asm2, state2)
  }

  def rrw(f: (String, String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    val (xReg, yReg, asm1, state1) = state.read2
    val (tReg, asm2, state2) = state1.write
    assert(xReg == tReg)
    (asm1 :+ f(xReg, yReg) :++ asm2, state2)
  }

  def combineSteps(steps: List[Step])(implicit state: RegState): (List[Asm], RegState) = {
    steps.foldLeft[(List[Asm], RegState)]((Nil, state))((prev, step) => prev match {
      case (asm1, state1) => {
        val (asm2, state2) = step(state1)
        (asm1 ++ asm2, state2)
      }
    })
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
    stats.flatMap(genStat)
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

  def genBinOp(x: Expr, y: Expr, f: (String, String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {

    combineSteps(List(
      genExpr(x)(_),
      genExpr(y)(_),
      rrw(f(_, _))(_)
    ))
  }

  def genUnOp(x: Expr, f: (String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    combineSteps(List(
      genExpr(x)(_),
      rw(f(_))(_)
    ))
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
      combineSteps(List(
        w(Mov(_, intToAsmLit((exprs.length + 1) * 4)))(_),
        rw(new Malloc(_))(_),
        
      ))

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

package backend

import frontend.ast
import frontend.ast._
import asm._
import frontend.symbols.TypeTable

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
  
  type Step = RegState => (List[Asm], RegState)

  def r(fs: (String) => Asm *)(implicit state: RegState): (List[Asm], RegState) = {
    val (reg, asm1, state1) = state.read
    (asm1 ++ fs.map(_(reg)), state1)
  }
  
  def w(f: (String) => Asm)(implicit state: RegState): (List[Asm], RegState) = {
    val (reg, asm1, state1) = state.write
    (f(reg) +: asm1, state1)
  }

  def ro(fs: (String) => Asm *)(implicit state: RegState): (List[Asm], RegState) = {
    val (reg1, asm1, state1) = state.read
    val (reg2, asm2, state2) = state1.write
    assert(reg1 == reg2)
    (asm1 ++ fs.map(_(reg1)) ++ asm2, state2)
  }

  def rw(fs: (String, String) => Asm *)(implicit state: RegState): (List[Asm], RegState) = {
    val (reg1, asm1, state1) = state.peek
    val (reg2, asm2, state2) = state1.write
    (asm1 ++ fs.map(_(reg2, reg1)) ++ asm2, state2)
  }

  def rro(fs: (String, String) => Asm *)(implicit state: RegState): (List[Asm], RegState) = {
    val (xReg, yReg, asm1, state1) = state.read2
    val (tReg, asm2, state2) = state1.write
    assert(xReg == tReg)
    (asm1 ++ fs.map(_(xReg, yReg)) ++ asm2, state2)
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
      funcs.flatMap(f => genFunc(f.id.id, f.args.length, f.body)(f.symbols.get)) ++ genFunc("main", 0, stats)(program.mainSymbols.get)
    }
  }

  def genFunc(name: String, argc: Int, stats: List[Stat])(implicit symbols: TypeTable): List[Asm] = {
    // todo: deal with ambiguity nicely
    List(asm.Func(Label(name), genStats(stats)))
  }

  def genStats(stats: List[Stat])(implicit symbols: TypeTable): List[Asm] = {
    stats.flatMap(genStat)
  }

  def genStat(stat: Stat)(implicit symbols: TypeTable): List[Asm] = {
    implicit val initialState: RegState = NEW_REG
    stat match {
      case Skip() => Nil
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => {
        val (rhsAsm, state) = genRhs(rhs)(NEW_REG, symbols)
        val lhsAsm = genLhs(lhs)(state, symbols)
        rhsAsm ++ lhsAsm
        // TODO: where does the evaluation of the rhs go? I assume the mov to stack is done by lhs?
      }
      case Read(lhs) => Nil
      case Free(expr) => Nil
      case Return(expr) => Nil
      case Exit(expr) => Nil
      case Print(expr) => Nil
      case Println(expr) => Nil
      case If(expr, thenStats, elseStats) => Nil
      case While(expr, doStats) => Nil
      case s@Scope(stats) => genStats(stats)(s.typeTable.get)
    } 
  }

  def genBinOp(x: Expr, y: Expr, f: (String, String) => Asm)
              (implicit state: RegState, symbols: TypeTable): (List[Asm], RegState) = {

    combineSteps(List(
      genExpr(x)(_, symbols),
      genExpr(y)(_, symbols),
      rro(f(_, _))(_)
    ))
  }

  def genUnOp(x: Expr, f: (String) => Asm)(implicit state: RegState, symbols: TypeTable): (List[Asm], RegState) = {
    combineSteps(List(
      genExpr(x)(_, symbols),
      ro(f(_))(_)
    ))
  }

  def genExpr(expr: Expr)(implicit state: RegState, symbols: TypeTable): (List[Asm], RegState) = expr match {
    case ast.Or(x, y)  => genBinOp(x, y, new asm.Or(_, _)())
    case ast.And(x, y) => genBinOp(x, y, new asm.And(_, _)())
    case ast.Eq(x, y)  => genBinOp(x, y, new asm.Eq(_, _)())
    case ast.Neq(x, y) => genBinOp(x, y, new asm.Neq(_, _)())
    case ast.Leq(x, y) => genBinOp(x, y, new asm.Leq(_, _)())
    case ast.Lt(x, y)  => genBinOp(x, y, new asm.Lt(_, _)())
    case ast.Geq(x, y) => genBinOp(x, y, new asm.Geq(_, _)())
    case ast.Gt(x, y)  => genBinOp(x, y, new asm.Gt(_, _)())
    case ast.Add(x, y) => genBinOp(x, y, new asm.Add(_, _)())
    case ast.Sub(x, y) => genBinOp(x, y, new asm.Sub(_, _)())
    case ast.Mul(x, y) => genBinOp(x, y, new asm.Mul(_, _)())
    case ast.Div(x, y) => genBinOp(x, y, new asm.Div(_, _)())
    case ast.Mod(x, y) => genBinOp(x, y, new asm.Mod(_, _)())
    case ast.Not(x)    => genUnOp(x, new asm.Not(_)())
    case ast.Neg(x)    => genUnOp(x, new asm.Neg(_)())
    case ast.Len(x)    => genUnOp(x, new asm.Len(_)())
    case ast.Ord(x)    => genUnOp(x, new asm.Ord(_)())
    case ast.Chr(x)    => genUnOp(x, new asm.Chr(_)())
    case _ => (Nil, state)
  }

  def genRhs(rhs: AssignRhs)(implicit state: RegState, symbols: TypeTable): (List[Asm], RegState) = rhs match {
    case ArrayLiter(exprs) => {
      val setupArray: Step = combineSteps(List(
        w(Mov(_, intToAsmLit(exprs.length))())(_),
        w(Mov(_, intToAsmLit((exprs.length + 1) * 4))())(_),
        ro(new Malloc(_)())(_), // replace sizeInBytes with a pointer to the array
        rro(
          new Str(_, _)(), // Store sizeInElements in array[0]
          Mov(_, _)() // replace sizeInElements with array pointer
        )(_),
      ))(_)

      def putElem(expr: Expr, i: Int): Step = {
        combineSteps(List(
          genExpr(expr)(_, symbols), // put value on the stack
          rro((pos, value) => new Str(value, pos)(intToAsmLit((i + 1) * 4)))(_) // store value at pos, pos remains on the stack
        ))(_)
      }

      combineSteps(
        List(setupArray)
        ++
        exprs.zipWithIndex.map(v => putElem(v._1, v._2))
      )
    }
    case NewPair(fst, snd) => {
      ???
    }
    case Fst(expr) => ???
    case Snd(expr) => ???
    case Call(id, args) => ???
    case expr: Expr => genExpr(expr)
  }

  def genLhs(lhs: AssignLhs)(implicit state: RegState, symbols: TypeTable): List[Asm] = {
    ???
  }
}

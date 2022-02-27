package backend

import frontend.ast
import frontend.ast._
import asm._
import state._
import state.Step._
import state.implicits._
import frontend.symbols.TypeTable
import scala.annotation.tailrec

object generator {

  // TODO: consider naming conventions for dynamically created unique labels
  private var uniqueNameGen = -1
  private def getUniqueName: Int = {uniqueNameGen += 1; uniqueNameGen}

  def genProgram(program: WaccProgram): Step = program match {
    case WaccProgram(funcs, stats) => (
      Directive("text\n") <++> Directive("global main") <++>
        funcs.foldLeft(Step.identity)((prev, f) => prev <++> genFunc(f.id.id, f.args.length, f.body)(f.symbols.get))
      <++> genMain(0, stats)(program.mainSymbols.get)
    )
  }

  // TODO: set sp
  def genMain(argc: Int, stats: List[Stat])(implicit symbols: TypeTable): Step = (
    Label("main")
    <++> Push("lr")
    <++> genStats(stats)
    <++> Ldr("r0", "=0")()
    <++> Pop("pc")
    <++> Directive("ltorg")
    <++> Step.discard
  )

  // TODO: set sp
  // Note that each ASM node here is implicitly converted to a step
  def genFunc(name: String, argc: Int, stats: List[Stat])(implicit symbols: TypeTable): Step = (
         Label(name)
    <++> Push("lr")
    <++> genStats(stats)
    <++> Pop("pc")
    <++> Directive("ltorg")
    <++> Step.discard
  )

  def genStats(stats: List[Stat])(implicit symbols: TypeTable): Step = {
    stats.foldLeft(Step.identity)(_ <++> genStat(_) <++> Step.discard)
  }

  //TODO

  // TODO: dynamically add doStats, thenStats and elseStats as functions instead?
  @tailrec
  def genStat(stat: Stat)(implicit symbols: TypeTable): Step = {
    stat match {
      case Skip() => Step.identity
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => genRhs(rhs) <++> genLhs(lhs)
      case Read(lhs) => ???
      case Free(expr) => ??? // Need to find out if is pair or array
      case Return(expr) => genExpr(expr) <++> r(reg => Mov(regToString(0), reg)())
      case Exit(expr) => genExpr(expr) <++> r(reg => CallAssembly(List(reg), "exit"))
      case Print(expr) => ???
      case Println(expr) => ???
      case s@If(expr, thenStats, elseStats) => {
        val l = getUniqueName
        val thenLabel = s"L_then_$l"
        val elseLabel = s"L_else_$l"
        val doneLabel = s"L_done_$l"
        genExpr(expr) <++>
        r(reg => Compare(reg, "#1")) <++>
        Branch(thenLabel)("LEQ") <++>
        Branch(elseLabel)("L") <++>
        Branch(doneLabel)() <++>
        Label("then") <++>
        genStats(thenStats)(s.thenTypeTable.get) <++>
        Label("else") <++>
        genStats(elseStats)(s.elseTypeTable.get) <++>
        Label("done")
      }
      case s@While(expr, doStats) =>
        val l = getUniqueName
        val topLabel = s"L_while_cond_$l"
        val endLabel = s"L_while_end$l"
        Label(topLabel) <++>
        genExpr(expr) <++>
        r(reg => Compare(reg, "#0")) <++>
        Branch(endLabel)("EQ") <++>
        genStats(doStats)(s.doTypeTable.get) <++>
        Label(endLabel)
      case s@Scope(stats) => genStats(stats)(s.typeTable.get)
    } 
  }

  def genBinOp(x: Expr, y: Expr, f: (String, String) => Asm)(implicit symbols: TypeTable): Step = (
           genExpr(x) 
      <++> genExpr(y)
      <++> rro(f(_, _))
    )

  def genUnOp(x: Expr, f: (String) => Asm)(implicit symbols: TypeTable): Step = {
    genExpr(x) <++> ro(f(_))
  }

  def genExpr(expr: Expr)(implicit symbols: TypeTable): Step = {
    expr match {
      case ast.Or(x, y)  => genBinOp(x, y, asm.Or(_, _)())
      case ast.And(x, y) => genBinOp(x, y, asm.And(_, _)())
      case ast.Eq(x, y)  => genBinOp(x, y, asm.Eq(_, _)())
      case ast.Neq(x, y) => genBinOp(x, y, asm.Neq(_, _)())
      case ast.Leq(x, y) => genBinOp(x, y, asm.Leq(_, _)())
      case ast.Lt(x, y)  => genBinOp(x, y, asm.Lt(_, _)())
      case ast.Geq(x, y) => genBinOp(x, y, asm.Geq(_, _)())
      case ast.Gt(x, y)  => genBinOp(x, y, asm.Gt(_, _)())
      case ast.Add(x, y) => genBinOp(x, y, asm.Add(_, _)())
      case ast.Sub(x, y) => genBinOp(x, y, asm.Sub(_, _)())
      case ast.Mul(x, y) => genBinOp(x, y, asm.Mul(_, _)())
      case ast.Div(x, y) => genBinOp(x, y, asm.Div(_, _)())
      case ast.Mod(x, y) => genBinOp(x, y, asm.Mod(_, _)())
      case ast.Not(x)    => genUnOp(x, asm.Not(_)())
      case ast.Neg(x)    => genUnOp(x, asm.Neg(_)())
      case ast.Len(x)    => genUnOp(x, asm.Len(_)())
      case ast.Ord(x)    => genUnOp(x, asm.Ord(_)())
      case ast.Chr(x)    => genUnOp(x, asm.Chr(_)())
      // TODO: deal with int overflow
      case ast.IntLiter(x) => w(reg => Ldr(reg, s"=$x")())
      case ast.BoolLiter(x) => w(reg => Ldr(reg, intToAsmLit(x.compare(false)))())
        // TODO: let ldr take a char directly
      case ast.CharLiter(x) => w(reg => Ldr(reg, intToAsmLit(x.toInt))())
      case ast.StrLiter(x) => ???
      case ast.ArrayLiter(x) => ???
        // TODO- lots more cases
      case _             => ???
    }
  }

  // TODO
  def genRhs(rhs: AssignRhs)(implicit symbols: TypeTable): Step = {
    rhs match {
        // [0,5,7,2]
      case ArrayLiter(exprs) => (
             w(Mov(_, intToAsmLit(exprs.length))())
        <++> w(Mov(_, intToAsmLit((exprs.length + 1) * 4))())
        <++> ro(reg => CallAssembly(List(reg), "malloc")) // replace sizeInBytes with a pointer to the array
        <++> rro(
          Str(_, _)(), // Store sizeInElements in array[0]
          Mov(_, _)() // replace sizeInElements with array pointer
        )
             // -> size, ------
             // -> pointer to array, nothing
        <++> exprs.zipWithIndex.foldLeft(Step.identity)((prev, v) => (
               prev 
          <++> genExpr(v._1) // put value in a register
                 // TODO: intToOffset
          <++> rro((pos, value) => Str(value, pos)(s"#${(v._2 + 1) * 4}")) // store value at pos, pos remains on the stack
        ))
      )
      case NewPair(fst, snd) => (
             w(Mov(_, intToAsmLit(4 * 2))())
        <++> ro(reg => CallAssembly(List(reg), "malloc"))
        <++> genExpr(fst)
        <++> rro((pos, value) => Str(value, pos)())
        <++> genExpr(snd)
             // TODO: intToOffset
        <++> rro((pos, value) => Str(value, pos)(s"#4"))
      )
      case Fst(expr) => (
             genExpr(expr)
        <++> ro(reg => Ldr(reg, reg)())
      )
      case Snd(expr) => (
             genExpr(expr)
        <++> ro(reg => Ldr(reg, reg)(s"#4"))
      )
      case Call(id, args) => ???
      case expr: Expr => genExpr(expr)
    }
  }

  def genLhs(lhs: AssignLhs)(implicit symbols: TypeTable): Step = lhs match {
    case id@Ident(_) => {
      val offset = countToOffset(symbols.getOffset(id).get)
      r(reg => Str(reg, STACK_POINTER)(s"#$offset"))
      // TODO: account for movement in stack pointer
    }
    case ArrayElem(id, index) => {
      // this is the case a[3] = x
      val offset = countToOffset(symbols.getOffset(id).get)
      genExpr(index) <++> r(reg => Str(reg, STACK_POINTER)(s"#${offset + 1}"))

      // nodes
      // r? = index + offset
      // r(reg => Str(
      // STR R(reg with id) [SP, R(reg with index + offset)]
      ???
    }
    case Fst(expr) => {
      ???
    }
    case Snd(expr) => {
      ???
    }
  }
}

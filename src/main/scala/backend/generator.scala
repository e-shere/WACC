package backend

import frontend.ast
import frontend.ast._
import asm._
import state._
import state.Step._
import state.implicits._
import frontend.symbols.TypeTable

object generator {
  def genProgram(program: WaccProgram): Step = program match {
    case WaccProgram(funcs, stats) => (
      funcs.foldLeft(Step.identity)((prev, f) => prev <++> genFunc(f.id.id, f.args.length, f.body)(f.symbols.get))
      <++> genFunc("main", 0, stats)(program.mainSymbols.get)
    )
  }

  def genFunc(name: String, argc: Int, stats: List[Stat])(implicit symbols: TypeTable): Step = (
         Label(name)
    <++> Push("lr")
    <++> genStats(stats)
    <++> Pop("pc")
    <++> Directive("ltorg")
  )

  def genStats(stats: List[Stat])(implicit symbols: TypeTable): Step = {
    stats.foldLeft(Step.identity)(_ <++> genStat(_) <++> Step.discard)
  }

  def genStat(stat: Stat)(implicit symbols: TypeTable): Step = {
    stat match {
      case Skip() => Step.identity
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => genRhs(rhs) <++> genLhs(lhs)
      case Read(lhs) => Step.identity
      case Free(expr) => Step.identity
      case Return(expr) => Step.identity
      case Exit(expr) => genExpr(expr) <++> r(reg => CallAssembly(List(reg), "exit"))
      case Print(expr) => Step.identity
      case Println(expr) => Step.identity
      case If(expr, thenStats, elseStats) => Step.identity
      case While(expr, doStats) => Step.identity
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
      case _             => Step.identity
    }
  }

  def genRhs(rhs: AssignRhs)(implicit symbols: TypeTable): Step = {
    rhs match {
      case ArrayLiter(exprs) => (
             w(Mov(_, intToAsmLit(exprs.length))())
        <++> w(Mov(_, intToAsmLit((exprs.length + 1) * 4))())
        <++> ro(new Malloc(_)()) // replace sizeInBytes with a pointer to the array
        <++> rro(
          new Str(_, _)(), // Store sizeInElements in array[0]
          Mov(_, _)() // replace sizeInElements with array pointer
        )
        <++> exprs.zipWithIndex.foldLeft(Step.identity)((prev, v) => (
               prev 
          <++> genExpr(v._1) // put value on the stack
          <++> rro((pos, value) => new Str(value, pos)(intToAsmLit((v._2 + 1) * 4))) // store value at pos, pos remains on the stack
        ))
      )
      case NewPair(fst, snd) => (
             w(Mov(_, intToAsmLit(4 * 2))())
        <++> ro(new Malloc(_)())
        <++> genExpr(fst)
        <++> rro((pos, value) => new Str(value, pos)())
        <++> genExpr(snd)
        <++> rro((pos, value) => new Str(value, pos)(intToAsmLit(4)))
      )
      case Fst(expr) => (
             genExpr(expr)
        <++> ro(reg => Ldr(reg, reg)())
      )
      case Snd(expr) => (
             genExpr(expr)
        <++> ro(reg => Ldr(reg, reg)(intToAsmLit(4)))
      )
      case Call(id, args) => ???
      case expr: Expr => genExpr(expr)
    }
  }

  def genLhs(lhs: AssignLhs)(implicit symbols: TypeTable): Step = lhs match {
    case id@Ident(_) => {
      val offset = countToOffset(symbols.getOffset(id).get)
      r(reg => Str(reg, STACK_POINTER)(intToAsmLit(offset)))
      // TODO: account for movement in stack pointer
    }
    case arrElem@ArrayElem(id, index) => {
      val offset = countToOffset(symbols.getOffset(id).get)
      genExpr(index) <++> r(reg => Str(reg, STACK_POINTER)(intToAsmLit(offset + 1)))

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

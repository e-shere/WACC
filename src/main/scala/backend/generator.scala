package backend

import frontend.ast
import frontend.ast._
import asm._
import backend.PredefinedFunctions.{PredefinedFunc, check_div_zero}
import step._
import frontend.symbols.TypeTable

import scala.annotation.tailrec
import backend.state.{STACK_POINTER, State}
import backend.step.implicits.implicitStep

object generator {

  private val r0 = AsmReg(0)
  private val r1 = AsmReg(1)
  private val lr = AsmReg(14)
  private val pc = AsmReg(15)

  // TODO: consider naming conventions for dynamically created unique labels
  private var uniqueNameGen = -1
  private def getUniqueName: Int = {uniqueNameGen += 1; uniqueNameGen}

  def genProgram(program: WaccProgram): Step = {
    val WaccProgram(funcs, stats) = program
    ( Directive("text\n")
      <++> Directive("global main")
      <++> funcs.foldLeft(Step.identity)((prev, f) => prev <++> genFunc(f.id.id, f.args.length, f.body)(f.symbols.get))
      <++> genMain(0, stats)(program.mainSymbols.get)
      <++> genPredefFuncs
    )
  }

  // TODO: set sp
  def genMain(argc: Int, stats: List[Stat])(implicit symbols: TypeTable): Step = (
         Label("main")
    <++> Push()(lr)
    <++> genStats(stats)
    <++> Ldr()(r0, AsmInt(0), AsmInt(0))
    <++> Pop()(pc)
    <++> Directive("ltorg")
    <++> Step.discardAll
  )

  // TODO: set sp
  // Note that each ASM node here is implicitly converted to a step
  def genFunc(name: String, argc: Int, stats: List[Stat])(implicit symbols: TypeTable): Step = (
         Label(name)
    <++> Push()(lr)
    <++> genStats(stats)
    <++> Pop()(pc)
    <++> Directive("ltorg")
    <++> Step.discardAll
  )

  def genStats(stats: List[Stat])(implicit symbols: TypeTable): Step = {
    stats.foldLeft(Step.identity)(_ <++> genStat(_) <++> Step.discardAll)
  }

  // TODO: dynamically add doStats, thenStats and elseStats as functions instead?
  @tailrec
  def genStat(stat: Stat)(implicit symbols: TypeTable): Step = {
    stat match {
      case Skip() => Step.identity
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      // In the Assign case, we use genLhs to store the offset on the stack
      // that we access the given lhs variable from
        // TODO: check
      case Assign(lhs, rhs) => genRhs(rhs) <++> genLhs(lhs) // <++> Str.step(&(-2), STACK_POINTER, &(-1))
      case Read(lhs) => ???
      case Free(expr) =>
        ??? // TODO: add free_pair to auxState set
        // TODO: switch on free array vs free pair
//        genExpr(expr) <++> genCallWithRegs(free_pair().label, 1)
      case Return(expr) => genExpr(expr) <++> Step.instr(Mov() _)(r0, Re1)()
      case Exit(expr) => genExpr(expr) <++> Step.instr(Mov() _)(r0, Re1)() <++> genCallWithRegs("exit", 1, None)
      case Print(expr) => ???
      case Println(expr) => ???
      case s@If(expr, thenStats, elseStats) => {
        val l = getUniqueName
        val thenLabel = s"L_then_$l"
        val elseLabel = s"L_else_$l"
        val doneLabel = s"L_done_$l"
            (genExpr(expr)
        <++> Step.instr(Compare() _)(Re1, AsmInt(1))()
        <++> Branch(thenLabel)("EQ")
        <++> Branch(elseLabel)("")
        <++> Label(thenLabel)
        <++> genStats(thenStats)(s.thenTypeTable.get)
        <++> Branch()(doneLabel)
        <++> Label(elseLabel)
        <++> genStats(elseStats)(s.elseTypeTable.get)
        <++> Label(doneLabel))
      }
      case s@While(expr, doStats) =>
        val l = getUniqueName
        val topLabel = s"L_while_cond_$l"
        val endLabel = s"L_while_end$l"
          (Label(topLabel)
        <++> genExpr(expr)
        <++> Step.instr(Compare() _)(Re1, AsmInt(0))()
        <++> Branch(endLabel)("EQ")
        <++> genStats(doStats)(s.doTypeTable.get)
        <++> Branch()(topLabel)
        <++> Label(endLabel))
      case s@Scope(stats) => genStats(stats)(s.typeTable.get)
    } 
  }

  def genBinOp(x: Expr, y: Expr, step: Step)(implicit symbols: TypeTable): Step = (
           genExpr(x) 
      <++> genExpr(y)
      <++> step
    )

  def genUnOp(x: Expr, step: Step)(implicit symbols: TypeTable): Step = {
    genExpr(x) <++> step
  }

  def genExpr(expr: Expr)(implicit symbols: TypeTable): Step = {
    expr match {
      case ast.Or(x, y)  => genBinOp(x, y, Step.instr(asm.Or() _)(Re2, Re2, Re1)(Re2))
      case ast.And(x, y) => genBinOp(x, y, asm.And.step(&(-2), &(-2), &(-1)))
      case ast.Eq(x, y)  => genBinOp(x, y, asm.Eq.step(&(-2), &(-2), &(-1)))
      case ast.Neq(x, y) => genBinOp(x, y, asm.Neq.step(&(-2), &(-2), &(-1)))
      case ast.Leq(x, y) => genBinOp(x, y, asm.Leq.step(&(-2), &(-2), &(-1)))
      case ast.Lt(x, y)  => genBinOp(x, y, asm.Lt.step(&(-2), &(-2), &(-1)))
      case ast.Geq(x, y) => genBinOp(x, y, asm.Geq.step(&(-2), &(-2), &(-1)))
      case ast.Gt(x, y)  => genBinOp(x, y, asm.Gt.step(&(-2), &(-2), &(-1)))
      case ast.Add(x, y) => genBinOp(x, y, asm.Add.step(&(-2), &(-2), &(-1)))
      case ast.Sub(x, y) => genBinOp(x, y, asm.Sub.step(&(-2), &(-2), &(-1)))
//      case ast.Mul(x, y) => genBinOp(x, y, asm.Mul.step(_0, _0, _1))
      case ast.Div(x, y) => genDiv
      case ast.Mod(x, y) => genMod
      case ast.Not(x)    => genUnOp(x, asm.Not.step(_0, _0))
      case ast.Neg(x)    => genUnOp(x, asm.Neg.step(_0, _0))
      case ast.Len(x)    => genUnOp(x, asm.Len.step(_0, _0))
      case ast.Ord(x)    => genUnOp(x, asm.Ord.step(_0, _0))
      case ast.Chr(x)    => genUnOp(x, asm.Chr.step(_0, _0))
      case ast.IntLiter(x) => Ldr.step(_0, AsmInt(x))
      case ast.BoolLiter(x) => Ldr.step(_0, AsmInt(x.compare(false)))
        // TODO: let ldr take a char directly
      case ast.CharLiter(x) => Ldr.step(_0, AsmInt(x.toInt))
      // There is some code repetition between StrLiter and ArrLiter - we might want to refactor this
      case ast.StrLiter(x) => (
        Mov.step(_0, AsmInt(x.length))
          <++> Mov.step(_0, AsmInt((x.length + 1) * 4))
          <++> genCallWithRegs("malloc", 1, Some(r0)) // replace sizeInBytes with a pointer to the array
          <++> Str.step(_0, _1) // TODO: avoid this register leak (the bottom register isn't used again)
          // -> size, ------
          // -> pointer to array, nothing
          <++> x.zipWithIndex.foldLeft(Step.identity)((prev, v) => (
          prev
            <++> asm.Chr.step(_0, AsmInt(v._1)) // Presumably this adds the char to the top of regState?
            // Does this not lose the place where we malloc? Solved on line 168
            // TODO: intToOffset
            <++> Str.step(_1, _0, AsmInt((v._2 + 1) * 4)) // store value at pos, pos remains on the stack
            <++> Step.discardTop //Ensure that the top of regState is the pointer from malloc
          ))
      )
      case ast.ArrayLiter(x) => (
        Mov.step(_0, AsmInt(x.length))
          <++> Mov.step(_0, AsmInt((x.length + 1) * 4))
          <++> genCallWithRegs("malloc", 1, Some(r0)) // replace sizeInBytes with a pointer to the array
          <++> Str.step(_0, _1) // TODO: avoid this register leak (the bottom register isn't used again)
          // -> size, ------
          // -> pointer to array, nothing
          <++> x.zipWithIndex.foldLeft(Step.identity)((prev, v) => (
          prev
            <++> genExpr(v._1) // put value in a register
            // Does this not lose the place where we malloc? Solved on line 168
            // TODO: intToOffset
            <++> Str.step(_1, _0, AsmInt((v._2 + 1) * 4)) // store value at pos, pos remains on the stack
            <++> Step.discardTop //Ensure that the top of regState is the pointer from malloc
          ))
      )
      case ArrayElem(id, index) => (genExpr(id)
        <++> genExpr(index)
        <++> asm.Add.step(_0,_0, AsmInt(1))
      // TODO
//        <++> asm.Mul.step(_0, _0, AsmInt(BYTE_SIZE))
        <++> asm.Add.step(_0, _0, _1))
      case idd@Ident(id) => {
        val offset = countToOffset(symbols.getOffset(idd).get)
        Ldr.step(_0, STACK_POINTER, AsmInt(offset))
      }
      case Null() => ???
      case Paren(expr) => genExpr(expr) // same symbol table?
    }
  }

  // TODO
  def genRhs(rhs: AssignRhs)(implicit symbols: TypeTable): Step = {
    rhs match {
        // [0,5,7,2]
      case arr@ArrayLiter(exprs) => genExpr(arr)
      case NewPair(fst, snd) => (
             Mov.step(_0, AsmInt(4 * 2))
        <++> genCallWithRegs("malloc", 1, Some(r0))
        <++> genExpr(fst)
        <++> Str.step(_1, _0)
        <++> Step.discardTop
        <++> genExpr(snd)
             // TODO: intToOffset
        <++> Str.step(_1, _0, AsmInt(4))
      )
      case Fst(expr) => (
             genExpr(expr)
        <++> Ldr.step(_0, _0)
      )
      case Snd(expr) => (
             genExpr(expr)
        <++> Ldr.step(_0, _0, AsmInt(4))
      )
      case ast.Call(id, args) => //TODO: a variable number of reads into the Nil
          args.foldLeft(Step.identity)(_ <++> genExpr(_)) //<++>
//          rn(regs => Call(regs, id.id)) // regs is a list of registers of size n
      case expr: Expr => genExpr(expr)
    }
  }

  def genLhs(lhs: AssignLhs)(implicit symbols: TypeTable): Step = lhs match {
    case id@Ident(_) => {
      val offset = countToOffset(symbols.getOffset(id).get)
      Mov.step(_0, AsmInt(offset))
      // TODO: account for movement in stack pointer
    }
    case arrElem@ArrayElem(id, index) => genExpr(arrElem)
    case Fst(id@Ident(_)) => genExpr(id)
    case Snd(id@Ident(_)) => (
      genExpr(id)
      <++> asm.Add.step(_0, _0, AsmInt(BYTE_SIZE)) // Should the offset be 4 or 1?
    )
  }

  // puts the memory location of the object in question in a register
  def genLocation(lhs: AssignLhs)(implicit symbols: TypeTable): Step = lhs match {
    case id@Ident(_) => {
      val offset = countToOffset(symbols.getOffset(id).get)
      Mov.step(_0, AsmInt(offset))
      // TODO: account for movement in stack pointer
    }
    case ArrayElem(id, index) => (
           genExpr(id)
      <++> genExpr(index)
      <++> asm.Add.step(_0,_0, AsmInt(1))
      <++> Mov.step(_0, AsmInt(BYTE_SIZE))
      <++> genMul()
      <++> asm.Add.step(_0, _0, _1))
    case Fst(expr) => genExpr(expr)
    case Snd(expr) => (
      genExpr(expr)
      <++> asm.Add.step(_0, _0, AsmInt(BYTE_SIZE)) // Should the offset be 4 or 1?
    )
  }

  // TODO
  def genMul(): Step = (
    ???
  )

  def genDiv: Step = (
    genCallWithRegs(check_div_zero().label, 2, None)
    <++> genCallWithRegs("__aeabi_idiv", 0, Some(r0))
    )

  def genMod: Step = (
    genCallWithRegs(check_div_zero().label, 2, None)
    <++> genCallWithRegs("__aeabi_idivmod", 0, Some(r1))
    )

  def genCallWithRegs(name: String, argc: Int, resultReg: Option[AsmReg]): Step = {
    assert(argc >= 0 && argc <= 4)
    (0 until argc).reverse.foldLeft(Step.identity)((prev, num) => {
      prev <++> Mov.step(AsmReg(num), _0)
    })
    Branch(name)("L") <++>
      (resultReg match {
      case None => Step.identity
      case Some(reg) => assert(reg.r >= 0 && reg.r <=3)
          Mov.step(_0, reg)
    })
  }

  def genPredefFuncs: Step = {
    Step((s: State) => s.fState.foldLeft(Step.identity)(
      (prev, f) => prev <++> f.toStep)(s))
  }

  def addPredefFunc(f: PredefinedFunc): Step = {
    Step((s: State) => (Nil, s.copy(fState = s.fState + f)))
  }
}


package backend

import frontend.ast
import frontend.ast._
import asm._
import backend.PredefinedFunctions._
import backend.asm.ConditionCode._
import step._
import frontend.symbols.TypeTable

import scala.annotation.tailrec
import backend.state.{STACK_POINTER, State}
import backend.step.implicits.implicitStep

object generator {
  // TODO: consider naming conventions for dynamically created unique labels
  private var uniqueNameGen = -1
  private def getUniqueName: Int = {uniqueNameGen += 1; uniqueNameGen}

  def genProgram(program: WaccProgram): Step = {
    val WaccProgram(funcs, stats) = program
    ( Directive("text\n")
      >++> Directive("global main")
      >++> funcs.foldLeft(Step.identity)((prev, f) => prev >++> genFunc(f.id.id, f.args.length, f.body)(f.symbols.get, program.printSymbols))
      >++> genMain(0, stats)(program.mainSymbols.get, program.printSymbols)
      >++> genPredefFuncs
      <++< genData
    )
  }

  // TODO: set sp
  def genMain(argc: Int, stats: List[Stat])
             (implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = (
         Label("main")
    >++> Push()(lr)
    >++> genBlock(stats)
    >++> Ldr()(r0, zero)(zero)
    >++> Pop()(pc)
    >++> Directive("ltorg")
    >++> Step.discardAll
  )

  // TODO: set sp
  // Note that each ASM node here is implicitly converted to a step
  def genFunc(name: String, argc: Int, stats: List[Stat])
             (implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = (
         Label(name)
    >++> Push()(lr)
    >++> genBlock(stats)
    >++> Pop()(pc)
    >++> Directive("ltorg")
    >++> Step.discardAll
  )

  def genBlock(stats: List[Stat])(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = (
    Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(countToOffset(symbols.symbols.size)))(zero)()
    >++> Step.instr3(Subs())(STACK_POINTER, STACK_POINTER, Re1)()
    >++> stats.foldLeft(Step.identity)(_ >++> genStat(_) >++> Step.discardAll)
    >++> Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(countToOffset(symbols.symbols.size)))(zero)()
    >++> Step.instr3(Adds())(STACK_POINTER, STACK_POINTER, Re1)()
    )

  def genStat(stat: Stat)(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = {
    stat match {
      case Skip() => Step.identity
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => (genRhs(rhs) >++> genLhs(lhs)
        >++> Step.instr2Aux(asm.Str())(Re2, Re1)(zero)())
      case Read(lhs) =>
        val readFunc: PredefinedFunc = printTable.get(lhs.pos) match {
          case Some(IntType()) => read_int()
          case Some(CharType()) => read_char()
          case Some(_) => ???
          case None => ??? // Should be unreachable
        }
        (genCallWithRegs(readFunc.label, 0, Some(r0))
        >++> genLhs(lhs)
        >++> Step.instr2Aux(asm.Str())(Re2, Re1)(zero)()
        >++> addPredefFunc(readFunc)
        )
      case Free(expr) => (genExpr(expr)
        >++> genCallWithRegs(free().label, 1, None)
        >++> addPredefFunc(free())
        >++> addPredefFunc(check_null_pointer())
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(print_string())
        )
      case Return(expr) => genExpr(expr) >++> Step.instr2(Mov())(r0, Re1)()
      case Exit(expr) => genExpr(expr) >++> genCallWithRegs("exit", 1, None)
      case Print(expr) =>
        val printFunc: PredefinedFunc = printTable.get(expr.pos) match {
          case Some(StringType()) => print_string()
          case Some(BoolType()) =>  print_bool()
          case Some(CharType()) =>  print_char()
          case Some(IntType()) => print_int()
          case Some(_) => print_ref()
          case None => ???   // Unreachable case statement
        }
        (genExpr(expr)
        >++> (genCallWithRegs(printFunc.label, 1, None)
        >++> addPredefFunc(printFunc))
        )
      case Println(expr) => (genStat(Print(expr)(NO_POS))
        >++> genCallWithRegs(print_ln().label, 0, None)
        >++> addPredefFunc(print_ln())
        )
      case s@If(expr, thenStats, elseStats) => {
        val l = getUniqueName
        val thenLabel = s"L_then_$l"
        val elseLabel = s"L_else_$l"
        val doneLabel = s"L_done_$l"
            (genExpr(expr)
        >++> Step.instr2Aux(Compare())(Re1, AsmInt(1))("")()
        >++> Branch(EQ)(thenLabel)
        >++> Branch()(elseLabel)
        >++> Label(thenLabel)
        >++> genBlock(thenStats)(s.thenTypeTable.get, printTable)
        >++> Branch()(doneLabel)
        >++> Label(elseLabel)
        >++> genBlock(elseStats)(s.elseTypeTable.get, printTable)
        >++> Label(doneLabel))
      }
      case s@While(expr, doStats) =>
        val l = getUniqueName
        val topLabel = s"L_while_cond_$l"
        val endLabel = s"L_while_end$l"
          (Label(topLabel)
        >++> genExpr(expr)
        >++> Step.instr2Aux(Compare())(Re1, zero)("")()
        >++> Branch(EQ)(endLabel)
        >++> genBlock(doStats)(s.doTypeTable.get, printTable)
        >++> Branch()(topLabel)
        >++> Label(endLabel))
      case s@Scope(stats) => genBlock(stats)(s.typeTable.get, printTable)
    }
  }

  def genBinOp(x: Expr, y: Expr, step: Step)(implicit symbols: TypeTable): Step = (
           genExpr(x)
      >++> genExpr(y)
      >++> step
    )

  def genUnOp(x: Expr, step: Step)(implicit symbols: TypeTable): Step = {
    genExpr(x) >++> step
  }

  def genExpr(expr: Expr)(implicit symbols: TypeTable): Step = {
    expr match {
      case ast.Or(x, y)  => genBinOp(x, y, Step.instr3(asm.Or())(Re2, Re2, Re1)(Re2))
      case ast.And(x, y) => genBinOp(x, y, Step.instr3(asm.And())(Re2, Re2, Re1)(Re2))
      case ast.Eq(x, y)  => genBinOp(x, y, Step.instr3(asm.Eq())(Re2, Re2, Re1)(Re2))
      case ast.Neq(x, y) => genBinOp(x, y, Step.instr3(asm.Neq())(Re2, Re2, Re1)(Re2))
      case ast.Leq(x, y) => genBinOp(x, y, Step.instr3(asm.Leq())(Re2, Re2, Re1)(Re2))
      case ast.Lt(x, y)  => genBinOp(x, y, Step.instr3(asm.Lt())(Re2, Re2, Re1)(Re2))
      case ast.Geq(x, y) => genBinOp(x, y, Step.instr3(asm.Geq())(Re2, Re2, Re1)(Re2))
      case ast.Gt(x, y)  => genBinOp(x, y, Step.instr3(asm.Gt())(Re2, Re2, Re1)(Re2))
      case ast.Add(x, y) => genBinOp(x, y, Step.instr3(asm.Adds())(Re2, Re2, Re1)(Re2))
      case ast.Sub(x, y) => genBinOp(x, y, Step.instr3(asm.Subs())(Re2, Re2, Re1)(Re2))
      case ast.Mul(x, y) => genBinOp(x, y, genMul())
      case ast.Div(x, y) => genBinOp(x, y, genDiv)
      case ast.Mod(x, y) => genBinOp(x, y, genMod)
      case ast.Not(x)    => genUnOp(x, Step.instr2(asm.Not())(Re1, Re1)(Re1))
      case ast.Neg(x)    => genUnOp(x, Step.instr2(asm.Neg())(Re1, Re1)(Re1))
      case ast.Len(x)    => genUnOp(x, Step.instr2(asm.Len())(Re1, Re1)(Re1))
      case ast.Ord(x)    => genUnOp(x, Step.identity)
      case ast.Chr(x)    => genUnOp(x, Step.identity)
      case ast.IntLiter(x) => Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(x))(zero)()
      case ast.BoolLiter(x) => Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(x.compare(false)))(zero)()
      case ast.CharLiter(x) => Step.instr2(asm.Mov())(ReNew, AsmChar(x))()
      // TODO: There is some code repetition between StrLiter and ArrLiter - we might want to refactor this
      case ast.StrLiter(x) =>
        val msg = x.flatMap(escapeToStr)
        includeData(msg) >++> Step.instr2Aux(asm.Ldr())(ReNew, AsmStateFunc(_.data(msg)))(zero)()
      case ast.ArrayLiter(x) => (
        Step.instr2(asm.Mov())(ReNew, AsmInt(x.length))()
          >++> Step.instr2(asm.Mov())(ReNew, AsmInt((x.length + 1) * WORD_BYTES))()
          >++> genCallWithRegs("malloc", 1, Some(r0)) // replace sizeInBytes with a pointer to the array
          >++> Step.instr2Aux(asm.Str())(Re2, Re1)(zero)(Re2, Re1)
          >++> Step.instr2(asm.Mov())(Re2, Re1)(Re2)
          >++> x.zipWithIndex.foldLeft(Step.identity)((prev, v) => (
          prev
            >++> genExpr(v._1) // put value in a register
            >++> Step.instr2Aux(asm.Str())(Re1, Re2)(AsmInt((v._2 + 1) * WORD_BYTES))(Re2)
          ))
      )
      case s@ArrayElem(id, index) => genLhs(s)
      case idd@Ident(_) => genLhs(idd) >++> Step.instr2Aux(asm.Ldr())(Re1, Re1)(zero)(Re1)
      case Null() => Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(0))(zero)()
      case Paren(expr) => genExpr(expr)
    }
  }

  def genRhs(rhs: AssignRhs)(implicit symbols: TypeTable): Step = {
    rhs match {
      case arr@ArrayLiter(_) => genExpr(arr)
      case NewPair(fst, snd) => (
        Step.instr2(asm.Mov())(ReNew, AsmInt(4 * 2))()
        >++> genCallWithRegs("malloc", 1, Some(r0))
        >++> genExpr(fst)
        >++> Step.instr2Aux(asm.Str())(Re1, Re2)(zero)(Re2)
        >++> genExpr(snd)
        >++> Step.instr2Aux(asm.Str())(Re1, Re2)(word_size)(Re2)
      )
      case Fst(expr) => (
             genExpr(expr)
        >++> genCallWithRegs(check_null_pointer().label, 1, Some(r0))
        >++> Step.instr2Aux(asm.Ldr())(Re1, Re1)(zero)(Re1)
        >++> addPredefFunc(check_null_pointer())
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(throw_overflow())
        >++> addPredefFunc(print_string())
      )
      case Snd(expr) => (
             genExpr(expr)
        >++> genCallWithRegs(check_null_pointer().label, 1, Some(r0))
        >++> Step.instr2Aux(asm.Ldr())(Re1, Re1)(word_size)(Re1)
        >++> addPredefFunc(check_null_pointer())
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(throw_overflow())
        >++> addPredefFunc(print_string())
      )
      case ast.Call(id, args) => (
        // We reverse the arguments to match the order in which they are put on the stack
        args.reverse.foldLeft(Step.identity)(_ >++> genExpr(_) >++> Step.instr1(Push())(Re1)())
          >++> BranchLink()(id.id)
          >++> Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(countToOffset(args.length)))(zero)()
          >++> Step.instr3(asm.Adds())(STACK_POINTER, STACK_POINTER, Re1)()
          >++> Step.instr2(Mov())(ReNew, r0)()
        )
      case expr: Expr => genExpr(expr)
    }
  }


  // puts the memory location of the object in question in a register
  def genLhs(lhs: AssignLhs)(implicit symbols: TypeTable): Step = lhs match {
    case id@Ident(_) =>
      // This stores the actual location in a new register
      (Step.instr2Aux(asm.Ldr())(ReNew, AsmStateFunc(s => AsmInt(countToOffset(symbols.getOffset(id).get + s.getStackOffset))))(zero)()
        >++> Step.instr3(asm.Adds())(ReNew, STACK_POINTER, Re1)())
    case ArrayElem(id, index) => (genExpr(id)
      >++> genExpr(index)
      >++> genExpr(id)
      >++> genExpr(index)
      >++> genCallWithRegs(check_array_bound().label, 2, None)
      >++> Step.instr3(asm.Adds())(Re1, Re1, AsmInt(1))(Re1)
      >++> Step.instr2(asm.Mov())(ReNew, AsmInt(4))()
      >++> genMul()
      >++> Step.instr3(asm.Adds())(Re2, Re2, Re1)(Re2)
      >++> addPredefFunc(check_array_bound())
      >++> addPredefFunc(throw_runtime())
      >++> addPredefFunc(print_string())
      )
    case Fst(expr) => genExpr(expr)
    case Snd(expr) => (
      genExpr(expr)
      >++> Step.instr3(asm.Adds())(Re1, Re1, word_size)(Re1)
    )
  }

  // TODO
  def genMul(): Step = (
    Step.instr4(SMull())(Re2, Re1, Re2, Re1)(Re2, Re1)
    >++> Step.instr2Aux(Compare())(Re1, Re2)("ASR #31")(Re2)
    >++> BranchLink(NE)(throw_overflow().label)
    >++> addPredefFunc(throw_overflow())
    >++> addPredefFunc(throw_runtime())
    >++> addPredefFunc(print_string())
  )

  def genDiv: Step = (
    genCallWithRegs(check_div_zero().label, 2, None)
    >++> genCallWithRegs("__aeabi_idiv", 0, Some(r0))
    >++> addPredefFunc(check_div_zero())
    >++> addPredefFunc(throw_runtime())
    >++> addPredefFunc(print_string())
    )

  def genMod: Step = (
    genCallWithRegs(check_div_zero().label, 2, None)
    >++> genCallWithRegs("__aeabi_idivmod", 0, Some(r1))
    >++> addPredefFunc(check_div_zero())
    >++> addPredefFunc(throw_runtime())
    >++> addPredefFunc(print_string())
    )

  // resultReg is which of r0/r1/r2 we want
  // this will be stored in a new register
  def genCallWithRegs(name: String, argc: Int, resultReg: Option[AsmReg]): Step = {
    assert(argc >= 0 && argc <= 4)
    (
      (0 until argc).reverse.foldLeft(Step.identity)((prev, num) => {
      prev >++> Step.instr2(asm.Mov())(AsmReg(num), Re1)()
    })
      >++> BranchLink()(name)
      >++> (resultReg match {
        case None => Step.identity
        case Some(reg) => assert(reg.r >= 0 && reg.r <=3)
          Step.instr2(asm.Mov())(ReNew, reg)()
      })
    )
  }

  def genPredefFuncs: Step = {
    Step((s: State) => s.fState.foldLeft(Step.identity)(
      (prev, f) => prev >++> f.toStep)(s), "genPredefFuncs")
  }

  def addPredefFunc(f: PredefinedFunc): Step = {
    Step((s: State) => (Nil, s.copy(fState = s.fState + f)), s"addPredefFunc(${f.label})")
  }

  def genData: Step = (
    Step((s: State) => (if (s.data.isEmpty) Step.identity else
      Directive("data")
      >++> s.data.foldLeft(Step.identity)(
      (prevStep, entry) => prevStep
        >++> Label(entry._2.toString)
        >++> Directive(s"word ${entry._1.length}")
        >++> Directive(s"ascii \"${entry._1}\"")
    ))(s), s"genData")
  )

  def includeData(msg: String): Step = {
    Step((s: State) => (Nil, s.copy(data =
      if (s.data.contains(msg)) s.data
      else s.data + (msg -> AsmString(s"msg_$getUniqueName")))), s"includeData($msg)")
  }
}


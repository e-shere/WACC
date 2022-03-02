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
      >++> Directive("global main")
      >++> funcs.foldLeft(Step.identity)((prev, f) => prev >++> genFunc(f.id.id, f.args.length, f.body)(f.symbols.get))
      >++> genMain(0, stats)(program.mainSymbols.get)
      >++> genPredefFuncs
      <++< genData
    )
  }

  // TODO: set sp
  def genMain(argc: Int, stats: List[Stat])(implicit symbols: TypeTable): Step = (
         Label("main")
    >++> Push()(lr)
    >++> genBlock(stats)
    >++> Ldr()(r0, AsmInt(0))(AsmInt(0))
    >++> Pop()(pc)
    >++> Directive("ltorg")
    >++> Step.discardAll
  )

  // TODO: set sp
  // Note that each ASM node here is implicitly converted to a step
  def genFunc(name: String, argc: Int, stats: List[Stat])(implicit symbols: TypeTable): Step = (
         Label(name)
    >++> Push()(lr)
    >++> genBlock(stats)
    >++> Pop()(pc)
    >++> Directive("ltorg")
    >++> Step.discardAll
  )

  def genBlock(stats: List[Stat])(implicit symbols: TypeTable): Step = (
    Subs()(STACK_POINTER, STACK_POINTER, AsmInt(countToOffset(symbols.symbols.size)))
    >++> stats.foldLeft(Step.identity)(_ >++> genStat(_) >++> Step.discardAll)
    >++> Adds()(STACK_POINTER, STACK_POINTER, AsmInt(countToOffset(symbols.symbols.size)))
    )

  @tailrec
  def genStat(stat: Stat)(implicit symbols: TypeTable): Step = {
    stat match {
      case Skip() => Step.identity
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => (genRhs(rhs) >++> genLhs(lhs))
      case Read(lhs) => (genLhs(lhs)
        >++> genCallWithRegs(read_byte.toString(), 1, Some(r0))
        >++> addPredefFunc(read_byte())
        )
      case Free(expr) => (genExpr(expr)
        >++> genCallWithRegs(free.toString(), 1, None)
        >++> addPredefFunc(free())
        >++> addPredefFunc(check_null_pointer())
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(print_string())
        )
      case Return(expr) => genExpr(expr) >++> Step.asmInstr(Mov())(r0, Re1)()
      case Exit(expr) => genExpr(expr) >++> Step.asmInstr(Mov())(r0, Re1)() >++> genCallWithRegs("exit", 1, None)
      // TODO: call the right print function
      case Print(expr) => (genExpr(expr)
        >++> genCallWithRegs("???", 1, None)
        >++> addPredefFunc(print_string())
        )
      case Println(expr) => (genExpr(expr)
        >++> genCallWithRegs(print_ln.toString(), 1, None)
        >++> addPredefFunc(print_ln())
        )
      case s@If(expr, thenStats, elseStats) => {
        val l = getUniqueName
        val thenLabel = s"L_then_$l"
        val elseLabel = s"L_else_$l"
        val doneLabel = s"L_done_$l"
            (genExpr(expr)
        >++> Step.asmInstr(Compare())(Re1, AsmInt(1))()
        >++> Branch(EQ)(thenLabel)
        >++> Branch()(elseLabel)
        >++> Label(thenLabel)
        >++> genBlock(thenStats)(s.thenTypeTable.get)
        >++> Branch()(doneLabel)
        >++> Label(elseLabel)
        >++> genBlock(elseStats)(s.elseTypeTable.get)
        >++> Label(doneLabel))
      }
      case s@While(expr, doStats) =>
        val l = getUniqueName
        val topLabel = s"L_while_cond_$l"
        val endLabel = s"L_while_end$l"
          (Label(topLabel)
        >++> genExpr(expr)
        >++> Step.asmInstr(Compare())(Re1, AsmInt(0))()
        >++> Branch(EQ)(endLabel)
        >++> genBlock(doStats)(s.doTypeTable.get)
        >++> Branch()(topLabel)
        >++> Label(endLabel))
      case s@Scope(stats) => genBlock(stats)(s.typeTable.get)
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
      case ast.Or(x, y)  => genBinOp(x, y, Step.asmInstr(asm.Or())(Re2, Re2, Re1)(Re2))
      case ast.And(x, y) => genBinOp(x, y, Step.asmInstr(asm.And())(Re2, Re2, Re1)(Re2))
      case ast.Eq(x, y)  => genBinOp(x, y, Step.stepInstr(asm.Eq())(Re2, Re2, Re1)(Re2))
      case ast.Neq(x, y) => genBinOp(x, y, Step.stepInstr(asm.Neq())(Re2, Re2, Re1)(Re2))
      case ast.Leq(x, y) => genBinOp(x, y, Step.stepInstr(asm.Leq())(Re2, Re2, Re1)(Re2))
      case ast.Lt(x, y)  => genBinOp(x, y, Step.stepInstr(asm.Lt())(Re2, Re2, Re1)(Re2))
      case ast.Geq(x, y) => genBinOp(x, y, Step.stepInstr(asm.Geq())(Re2, Re2, Re1)(Re2))
      case ast.Gt(x, y)  => genBinOp(x, y, Step.stepInstr(asm.Gt())(Re2, Re2, Re1)(Re2))
      case ast.Add(x, y) => genBinOp(x, y, Step.asmInstr(asm.Adds())(Re2, Re2, Re1)(Re2))
      case ast.Sub(x, y) => genBinOp(x, y, Step.asmInstr(asm.Subs())(Re2, Re2, Re1)(Re2))
      case ast.Mul(x, y) => genBinOp(x, y, genMul())
      case ast.Div(x, y) => genBinOp(x, y, genDiv)
      case ast.Mod(x, y) => genBinOp(x, y, genMod)
      case ast.Not(x)    => genUnOp(x, Step.asmInstr(asm.Not())(Re1, Re1)(Re1))
      case ast.Neg(x)    => genUnOp(x, Step.asmInstr(asm.Neg())(Re1, Re1)(Re1))
      case ast.Len(x)    => genUnOp(x, Step.stepInstr(asm.Len())(Re1, Re1)(Re1))
      case ast.Ord(x)    => ???
      case ast.Chr(x)    => ???
      case ast.IntLiter(x) => Step.genericAsmInstr(asm.Ldr())(ReNew, AsmInt(x))(AsmInt(0))()
      case ast.BoolLiter(x) => Step.genericAsmInstr(asm.Ldr())(ReNew, AsmInt(x.compare(false)))(AsmInt(0))()
        // TODO: let ldr take a char directly
      case ast.CharLiter(x) => Step.genericAsmInstr(asm.Ldr())(ReNew, AsmInt(x.toInt))(AsmInt(0))()
      // There is some code repetition between StrLiter and ArrLiter - we might want to refactor this
      case ast.StrLiter(x) =>
        includeData(x) >++> Step.genericAsmInstr(asm.Ldr())(ReNew, AsmStateFunc(_.data(x)))(AsmInt(0))()
      case ast.ArrayLiter(x) => (
        Step.asmInstr(asm.Mov())(ReNew, AsmInt(x.length))()
          >++> Step.asmInstr(asm.Mov())(ReNew, AsmInt((x.length + 1) * 4))()
          >++> genCallWithRegs("malloc", 1, Some(r0)) // replace sizeInBytes with a pointer to the array
          >++> Step.genericAsmInstr(asm.Str())(Re2, Re1)(AsmInt(0))(Re1)
          // -> size, ------
          // -> pointer to array, nothing
          >++> x.zipWithIndex.foldLeft(Step.identity)((prev, v) => (
          prev
            >++> genExpr(v._1) // put value in a register
            // Does this not lose the place where we malloc? Solved on line 168
            // TODO: intToOffset
            >++> Step.genericAsmInstr(asm.Str())(Re1, Re2)(AsmInt((v._2 + 1) * 4))(Re2)
            // Str.step(_1, _0, AsmInt((v._2 + 1) * 4)) // store value at pos, pos remains on the stack
            >++> Step.discardTop //Ensure that the top of regState is the pointer from malloc
          ))
      )
      case ArrayElem(id, index) => (genExpr(id)
        >++> genExpr(index)
        >++> Step.asmInstr(asm.Adds())(Re1, Re1, AsmInt(1))(Re1)
// TODO:       <++> asm.Mul.step(_0, _0, AsmInt(BYTE_SIZE))
        >++> Step.asmInstr(asm.Adds())(Re2, Re2, Re1)(Re2)
        )
      case idd@Ident(id) => (
        genLocation(idd)
        >++> Step.genericAsmInstr(Ldr())(Re1, Re1)(AsmInt(0))(Re1)
      )
      case Null() => ???
      case Paren(expr) => genExpr(expr)
    }
  }

  // TODO
  def genRhs(rhs: AssignRhs)(implicit symbols: TypeTable): Step = {
    rhs match {
        // [0,5,7,2]
      case arr@ArrayLiter(exprs) => genExpr(arr)
      case NewPair(fst, snd) => (
        Step.asmInstr(asm.Mov())(Re1, AsmInt(4 * 2))(Re1)
        >++> genCallWithRegs("malloc", 1, Some(r0))
        >++> genExpr(fst)
        >++> Step.genericAsmInstr(asm.Str())(Re2, Re1)(AsmInt(0))(Re2)
          // Str.step(_1, _0)
        >++> Step.discardTop
        >++> genExpr(snd)
             // TODO: intToOffset
        >++> Step.genericAsmInstr(asm.Str())(Re2, Re1)(AsmInt(4))(Re2)
          //Str.step(_1, _0, AsmInt(4))
      )
      case Fst(expr) => (
             genExpr(expr)
        >++> Step.genericAsmInstr(asm.Ldr())(Re2, Re1)(AsmInt(0))(Re1)
      )
      case Snd(expr) => (
             genExpr(expr)
        >++> Step.genericAsmInstr(asm.Ldr())(Re1, Re1)(AsmInt(4))(Re1)
      )
      case ast.Call(id, args) => (
          args.foldLeft(Step.identity)(_ >++> genExpr(_) >++> Step.asmInstr(Push())(Re1)())
          >++> BranchLink()(id.id)
          >++> Adds()(STACK_POINTER, STACK_POINTER, AsmInt(countToOffset(args.length)))
          >++> Step.asmInstr(Mov())(ReNew, r0)()
        )
      case expr: Expr => genExpr(expr)
    }
  }

  def genLhs(lhs: AssignLhs)(implicit symbols: TypeTable): Step = (
    genLocation(lhs)
    // re1 contains offset
    // re2 contains the value
      >++> Step.genericAsmInstr(asm.Str())(Re2, Re1)(AsmInt(0))()
  )

  // puts the memory location of the object in question in a register
  def genLocation(lhs: AssignLhs)(implicit symbols: TypeTable): Step = lhs match {
    case id@Ident(_) => Step({state =>
      val offset = countToOffset(symbols.getOffset(id).get + state.getStackOffset)
      // This stores the actual location in a new register
      Step.asmInstr(asm.Adds())(ReNew, STACK_POINTER, AsmInt(offset))()(state)
    })
    case ArrayElem(id, index) => (
           genExpr(id)
      >++> genExpr(index)
      >++> Step.asmInstr(asm.Adds())(Re1, Re1, AsmInt(1))(Re1)
      >++> Step.asmInstr(asm.Mov())(Re1, AsmInt(BYTE_SIZE))(ReNew)
      >++> genMul()
      >++> Step.asmInstr(asm.Adds())(Re2, Re2, Re1)(Re2)
      )
    case Fst(expr) => genExpr(expr)
    case Snd(expr) => (
      genExpr(expr)
      >++> Step.asmInstr(asm.Adds())(Re1, Re1, AsmInt(BYTE_SIZE))(Re1) // Should the offset be 4 or 1?
    )
  }

  // TODO
  def genMul(): Step = (
    Step.asmInstr(SMull())(Re2, Re1, Re2, Re1)(Re2, Re1)
    >++> Step.asmInstr(Compare())(Re1, Re2, AsmString("ASR 31"))(Re2)
    >++> BranchLink(NE)(throw_overflow().label)
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

  def genCallWithRegs(name: String, argc: Int, resultReg: Option[AsmReg]): Step = {
    assert(argc >= 0 && argc <= 4)
    (
      (0 until argc).reverse.foldLeft(Step.identity)((prev, num) => {
      prev >++> Step.asmInstr(asm.Mov())(AsmReg(num), Re1)()
    })
      >++> BranchLink()(name)
      >++> (resultReg match {
        case None => Step.identity
        case Some(reg) => assert(reg.r >= 0 && reg.r <=3)
          Step.asmInstr(asm.Mov())(ReNew, reg)()
      })
    )
  }

  def genPredefFuncs: Step = {
    Step((s: State) => s.fState.foldLeft(Step.identity)(
      (prev, f) => prev >++> f.toStep)(s))
  }

  def addPredefFunc(f: PredefinedFunc): Step = {
    Step((s: State) => (Nil, s.copy(fState = s.fState + f)))
  }

  def genData: Step = (
    Step((s: State) => (if (s.data.isEmpty) Step.identity else
      Directive("data")
      >++> s.data.foldLeft(Step.identity)(
      (prevStep, entry) => prevStep
        >++> Label(entry._2.toString)
        >++> Directive(s"word ${entry._1.length}")
        >++> Directive(s"ascii \"${entry._1}\"")
    ))(s))
  )

  def includeData(msg: String): Step = {
    Step((s: State) => (Nil, s.copy(data = s.data + (msg -> AsmString(s"msg_$getUniqueName")))))
  }
}


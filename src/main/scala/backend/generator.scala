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
import frontend.semanticChecker.FuncType

object generator {
  private var uniqueNameGen = -1
  private def getUniqueName: Int = {uniqueNameGen += 1; uniqueNameGen}

  // Note that each ASM node here is implicitly converted to a step
  def genProgram(program: WaccProgram): Step = {
    val WaccProgram(funcs, stats) = program
    ( Directive("text\n")
      >++> Directive("global main")
      >++> funcs.foldLeft(Step.identity)((prev, f) => prev >++> genFunc(f.id.id, f.args.length, f.body)(f.symbols.get, program.printSymbols, program.funcSymbols.get))
      >++> genMain(0, stats)(program.mainSymbols.get, program.printSymbols, program.funcSymbols.get)
      >++> genPredefFuncs
      <++< genData
    )
  }

  def genMain(argc: Int, stats: List[Stat])
             (implicit symbols: TypeTable, printTable: Map[(Int, Int), Type], funcSymbols: Map[Ident, FuncType]): Step = (
         Label("main")
    >++> Push()(lr)
    >++> genBlock(stats)
    >++> Ldr()(r0, zero)(zero)
    >++> Pop()(pc)
    >++> Directive("ltorg")
    >++> Step.discardAll
  )

  def genFunc(name: String, argc: Int, stats: List[Stat])
             (implicit symbols: TypeTable, printTable: Map[(Int, Int), Type], funcSymbols: Map[Ident, FuncType]): Step = (
         Label(name)
    >++> Push()(lr)
    >++> genBlock(stats)
    >++> Pop()(pc)
    >++> Directive("ltorg")
    >++> Step.discardAll
  )

  // genBlock is called on the entry of each new scope
  // and is responsible for modifying the stackpointer on entry and exit
  def genBlock(stats: List[Stat])(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type], funcSymbols: Map[Ident, FuncType]): Step = (
    Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(symbols.counter))(zero)()
    >++> Step.instr3(Subs())(STACK_POINTER, STACK_POINTER, Re1)()
    >++> stats.foldLeft(Step.identity)(_ >++> genStat(_) >++> Step.discardAll)
    >++> Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(symbols.counter))(zero)()
    >++> Step.instr3(Adds())(STACK_POINTER, STACK_POINTER, Re1)()
    )

  // genStat is responsible for generating steps for each stat in the AST
  // the state should NOT be modified
  def genStat(stat: Stat)(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type], funcSymbols: Map[Ident, FuncType]): Step = {
    stat match {
      case Skip() => Step.identity
      case Declare(_, id, rhs) => genStat(Assign(id, rhs)(stat.pos))
      case Assign(lhs, rhs) => (genRhs(rhs) >++> genLhs(lhs)
        >++> Step.instr2Aux(str(lhs match {
        case idd@Ident(_) => symbols.getType(idd).get
        case _ => printTable(lhs.pos)
      }))(Re2, Re1)(zero)())
      case Read(lhs) =>
        val readFunc: PredefinedFunc = printTable.get(lhs.pos) match {
          case Some(IntType()) => read_int()
          case Some(CharType()) => read_char()
          case Some(_) => ???
          case None => ??? // Should be unreachable
        }
        (genLhs(lhs)
        >++> genCallWithRegs(readFunc.label, 1, None)
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
          case Some(ArrayType(CharType()))=> print_string()
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
        >++> genBlock(thenStats)(s.thenTypeTable.get, printTable, funcSymbols)
        >++> Branch()(doneLabel)
        >++> Label(elseLabel)
        >++> genBlock(elseStats)(s.elseTypeTable.get, printTable, funcSymbols)
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
        >++> genBlock(doStats)(s.doTypeTable.get, printTable, funcSymbols)
        >++> Branch()(topLabel)
        >++> Label(endLabel))
      case s@Scope(stats) => genBlock(stats)(s.typeTable.get, printTable, funcSymbols)
    }
  }

  // Evaluate the left, then right parts of the expression, place both on the register stack
  // in order and then apply the binary operator on them
  def genBinOp(x: Expr, y: Expr, step: Step)(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = (
           genExpr(x)
      >++> genExpr(y)
      >++> step
    )

  // Evaluate the argument, place on the register stack and apply the unary operator on them
  def genUnOp(x: Expr, step: Step)(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = {
    genExpr(x) >++> step
  }

  // Evaluate the (potentially very nested) expression, and push the result on the register stack
  def genExpr(expr: Expr)(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = {
    expr match {
      case ast.Or(x, y)  => genBinOp(x, y, Step.instr3(asm.Or())(Re2, Re2, Re1)(Re2))
      case ast.And(x, y) => genBinOp(x, y, Step.instr3(asm.And())(Re2, Re2, Re1)(Re2))
      case ast.Eq(x, y)  => genBinOp(x, y, Step.instr3(asm.Eq())(Re2, Re2, Re1)(Re2))
      case ast.Neq(x, y) => genBinOp(x, y, Step.instr3(asm.Neq())(Re2, Re2, Re1)(Re2))
      case ast.Leq(x, y) => genBinOp(x, y, Step.instr3(asm.Leq())(Re2, Re2, Re1)(Re2))
      case ast.Lt(x, y)  => genBinOp(x, y, Step.instr3(asm.Lt())(Re2, Re2, Re1)(Re2))
      case ast.Geq(x, y) => genBinOp(x, y, Step.instr3(asm.Geq())(Re2, Re2, Re1)(Re2))
      case ast.Gt(x, y)  => genBinOp(x, y, Step.instr3(asm.Gt())(Re2, Re2, Re1)(Re2))
      case ast.Add(x, y) => (
        genBinOp(x, y, Step.instr3(asm.Adds())(Re2, Re2, Re1)(Re2))
        >++> BranchLink(VS)(throw_overflow().label)
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(throw_overflow())
        >++> addPredefFunc(print_string())
        )
      case ast.Sub(x, y) => (
        genBinOp(x, y, Step.instr3(asm.Subs())(Re2, Re2, Re1)(Re2))
        >++> BranchLink(VS)(throw_overflow().label)
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(throw_overflow())
        >++> addPredefFunc(print_string())
        )
      case ast.Mul(x, y) => genBinOp(x, y, genMul())
      case ast.Div(x, y) => genBinOp(x, y, genDiv)
      case ast.Mod(x, y) => genBinOp(x, y, genMod)
      case ast.Not(x)    => genUnOp(x, Step.instr2(asm.Not())(Re1, Re1)(Re1))
      case ast.Neg(x)    => genUnOp(x, Step.instr2(asm.Neg())(Re1, Re1)(Re1))
      case ast.Len(x)    => genUnOp(x, Step.instr2(asm.Len())(Re1, Re1)(Re1))
      case ast.Ord(x)    => genUnOp(x, Step.identity)
      case ast.Chr(x)    => genUnOp(x, Step.identity)
      case ast.IntLiter(x) => Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(x))(zero)()
      case ast.BoolLiter(x) => Step.instr2(asm.Mov())(ReNew, AsmInt(x.compare(false)))()
      case ast.CharLiter(x) => Step.instr2(asm.Mov())(ReNew, AsmChar(x))()
      case ast.StrLiter(x) =>
        includeData(x) >++> Step.instr2Aux(asm.Ldr())(ReNew, AsmStateFunc(_.data(x)))(zero)()
      case ast.ArrayLiter(x) => {
        val size:Int = printTable(expr.pos) match {
          case ArrayType(b) => b.size
          case _ => -1 // This should be unreachable provided the semantic checker is correct
        }
        (Step.instr2(asm.Mov())(ReNew, AsmInt(x.length))()
          >++> Step.instr2(asm.Mov())(ReNew, AsmInt((x.length * size) + WORD_BYTES))()
          >++> genCallWithRegs("malloc", 1, Some(r0))
          >++> Step.instr2Aux(asm.Str())(Re2, Re1)(zero)(Re2, Re1)
          >++> Step.instr2(asm.Mov())(Re2, Re1)(Re2)
          >++> x.zipWithIndex.foldLeft(Step.identity)((prev, v) =>
          prev
            >++> genExpr(v._1)
            >++> Step.instr2Aux(if (size == 1) asm.Strb() else asm.Str())(Re1, Re2)(AsmInt(WORD_BYTES + (v._2 * size)))(Re2)
          )
      )}
      case s@ArrayElem(id, _) => genLhs(s) >++> Step.instr2Aux(ldr(symbols.getType(id).get))(Re1, Re1)(zero)(Re1)
      case idd@Ident(_) => genLhs(idd) >++> Step.instr2Aux(ldr(symbols.getType(idd).get))(Re1, Re1)(zero)(Re1)
      case Null() => Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(0))(zero)()
      case Paren(expr) => genExpr(expr)
    }
  }

  // Evaluate types of rhs, and push value onto the register stack.
  // Many of these cases are handled in genExpr.
  def genRhs(rhs: AssignRhs)(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type], funcSymbols: Map[Ident, FuncType]): Step = {
    rhs match {
      case arr@ArrayLiter(_) => genExpr(arr)
      case NewPair(fst, snd) =>
        val fstType = printTable(fst.pos)
        val sndType = printTable(snd.pos)
        (
        Step.instr2(asm.Mov())(ReNew, AsmInt(fstType.size + sndType.size))()
        >++> genCallWithRegs("malloc", 1, Some(r0))
        >++> genExpr(fst)
        >++> Step.instr2Aux(str(fstType))(Re1, Re2)(zero)(Re2)
        >++> genExpr(snd)
        >++> Step.instr2Aux(str(sndType))(Re1, Re2)(AsmInt(fstType.size))(Re2)
      )
      case fst@Fst(expr) =>
        val fstType = printTable(expr.pos) match {
          case PairType(ty, _) => ty.toType
          case _ => ??? // This should be unreachable provided the semantic checker is correct
        }
        genLhs(fst) >++> Step.instr2Aux(ldr(fstType))(Re1, Re1)(zero)(Re1)
      case snd@Snd(expr) =>
        val (fstType, sndType) = printTable(expr.pos) match {
          case PairType(tyf, tys) => (tyf.toType, tys.toType)
          case _ => ??? // This should be unreachable provided the semantic checker is correct
        }
        genLhs(snd) >++> Step.instr2Aux(ldr(sndType))(Re1, Re1)(AsmInt(fstType.size))(Re1)
      case ast.Call(id, args) => (
        // We reverse the arguments to match the order in which they are put on the stack
        args.reverse.foldLeft(Step.identity)(_ >++> genExpr(_) >++> Step.instr1(Push())(Re1)())
          >++> BranchLink()(id.id)
          >++> Step.instr2Aux(asm.Ldr())(ReNew, AsmInt(funcSymbols(id).paramTypes.foldLeft(0)(_ + _.size)))(zero)()
          >++> Step.instr3(asm.Adds())(STACK_POINTER, STACK_POINTER, Re1)()
          >++> Step.instr2(Mov())(ReNew, r0)()
        )
      case expr: Expr => genExpr(expr)
    }
  }


  // Evaluates the lhs of an expression, and puts the memory location of the
  // object in question on the register stack
  def genLhs(lhs: AssignLhs)(implicit symbols: TypeTable, printTable: Map[(Int, Int), Type]): Step = lhs match {
    case id@Ident(_) =>
      (Step.instr2Aux(asm.Ldr())(ReNew, AsmStateFunc(s => AsmInt(symbols.getOffset(id).get + s.getStackOffset)))(zero)()
        >++> Step.instr3(asm.Adds())(ReNew, STACK_POINTER, Re1)())
    case ArrayElem(id, index) => {
      val ty: Type = id match {
        case ArrayElem(_, _) => ArrayType(AnyType()(NO_POS))(NO_POS)
        case idd@Ident(_) => symbols.getType(idd).get
      }
      (genExpr(id)
        >++> genExpr(index)
        >++> genExpr(index)
        >++> genExpr(id)
        >++> genCallWithRegs(check_array_bound().label, 2, None)
        >++> Step.instr2(asm.Mov())(ReNew, AsmInt(ty.size))()
        >++> genMul()
        >++> Step.instr3(asm.Adds())(Re1, Re1, word_size)(Re1)
        >++> Step.instr3(asm.Adds())(Re2, Re2, Re1)(Re2)
        >++> addPredefFunc(check_array_bound())
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(print_string())
        )
    }
    case Fst(expr) =>
      printTable(expr.pos) match {
        case PairType(ty, _) => ty.toType
        case _ => AnyType // This should be unreachable provided the semantic checker is correct
      }
      (genExpr(expr)
        >++> genCallWithRegs(check_null_pointer().label, 1, Some(r0))
        >++> addPredefFunc(check_null_pointer())
        >++> addPredefFunc(throw_runtime())
        >++> addPredefFunc(print_string())
        )
    case Snd(expr) =>
      val (_, _) = printTable(expr.pos) match {
        case PairType(tyf, tys) => (tyf.toType, tys.toType)
        case _ => (AnyType, AnyType) // This should be unreachable provided the semantic checker is correct
      }
      (
        genExpr(expr)
          >++> genCallWithRegs(check_null_pointer().label, 1, Some(r0))
          >++> addPredefFunc(check_null_pointer())
          >++> addPredefFunc(throw_runtime())
          >++> addPredefFunc(print_string())
        )
  }

  // Generate a multiply step, assuming the first two arguments are already on the
  // register stack, and places the result where the first argument was
  def genMul(): Step = (
    Step.instr4(SMull())(Re2, Re1, Re2, Re1)(Re2, Re1)
    >++> Step.instr2Aux(Compare())(Re1, Re2)("ASR #31")(Re2)
    >++> BranchLink(NE)(throw_overflow().label)
    >++> addPredefFunc(throw_overflow())
    >++> addPredefFunc(throw_runtime())
    >++> addPredefFunc(print_string())
  )

  // Generate a divide step, assuming the first two arguments are already on the
  // register stack, and places the result where the first argument was
  def genDiv: Step = (
    genCallWithRegs(check_div_zero().label, 2, None)
    >++> genCallWithRegs("__aeabi_idiv", 0, Some(r0))
    >++> addPredefFunc(check_div_zero())
    >++> addPredefFunc(throw_runtime())
    >++> addPredefFunc(print_string())
    )

  // Generate a mod step, assuming the first two arguments are already on the
  // register stack, and places the result where the first argument was
  def genMod: Step = (
    genCallWithRegs(check_div_zero().label, 2, None)
    >++> genCallWithRegs("__aeabi_idivmod", 0, Some(r1))
    >++> addPredefFunc(check_div_zero())
    >++> addPredefFunc(throw_runtime())
    >++> addPredefFunc(print_string())
    )

  // Generate a call to an assembly function with argc many args.
  // This loads argc many args into r0-r4
  // You can also optionally specify a resultReg, which determines which,
  // if any, of the result registers (r0-r4) should be pushed onto the register stack
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

  // Generates steps for all predefined functions which are used in the assembly file
  def genPredefFuncs: Step = {
    Step((s: State) => s.fState.foldLeft(Step.identity)(
      (prev, f) => prev >++> f.toStep)(s), "genPredefFuncs")
  }

  def ldr(ty: Type): (AsmReg, AsmArg) => AsmInt => Step = if (ty.size == 1) asm.Ldrb() else asm.Ldr()

  def str(ty: Type): (AsmReg, AsmArg) => AsmInt => Step = if (ty.size == 1) asm.Strb() else asm.Str()

  // Add a predefined function to the state
  def addPredefFunc(f: PredefinedFunc): Step = {
    Step((s: State) => (Nil, s.copy(fState = s.fState + f)), s"addPredefFunc(${f.label})")
  }

  // Generate steps which include data in the assembly file
  def genData: Step = (
    Step((s: State) => (if (s.data.isEmpty) Step.identity else
      Directive("data")
      >++> s.data.foldLeft(Step.identity)(
      (prevStep, entry) => prevStep
        >++> Label(entry._2.toString)
        >++> Directive(s"word ${entry._1.length}")
        >++> Directive(s"ascii \"${entry._1.flatMap(escapeToStr)}\"")
    ))(s), s"genData")
  )

  // Add extra data to the state
  def includeData(msg: String): Step = {
    Step((s: State) => (Nil, s.copy(data =
      if (s.data.contains(msg)) s.data
      else s.data + (msg -> AsmString(s"msg_$getUniqueName")))), s"includeData($msg)")
  }
}


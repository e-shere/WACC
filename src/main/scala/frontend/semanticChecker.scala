package frontend

import frontend.Errors._
import frontend.ast._
import frontend.symbols._

import java.io.File
import scala.collection.mutable
import scala.io.Source

object semanticChecker {

  private val ANY_TYPE = AnyType()(NO_POS)
  private val INT_TYPE = IntType()(NO_POS)
  private val BOOL_TYPE = BoolType()(NO_POS)
  private val CHAR_TYPE = CharType()(NO_POS)
  private val STRING_TYPE = StringType()(NO_POS)
  private val PAIR_TYPE = PairType(ANY_TYPE, ANY_TYPE)(NO_POS)
  private val ARRAY_TYPE = ArrayType(ANY_TYPE)(NO_POS)
  private val COMP_ARG_TYPES: Set[Type] = Set(INT_TYPE, CHAR_TYPE)
  private val EQ_ARG_TYPES: Set[Type] =
    Set(INT_TYPE, BOOL_TYPE, CHAR_TYPE, STRING_TYPE, PAIR_TYPE, ARRAY_TYPE)

  def validateProgram(
      program: WaccProgram,
      file: String
  ): List[WaccError] = {
    implicit val fileImplicit: String = file
    implicit val fileLines: Array[String] =
      Source.fromFile(new File(file)).getLines().toArray
    val errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer.empty
    program match {
      case WaccProgram(funcs, stats) => {
        // generate function table
        implicit var printSymbols: mutable.Map[(Int, Int), Type] = mutable.Map()
        val funcTableMut: mutable.Map[Ident, FuncType] = mutable.Map.empty
        for (Func(ty, id, args, _) <- funcs) {
          if (funcTableMut contains id)
            errors += RedefinedFunctionError.mkError(id)
          funcTableMut += (id -> FuncType(
            ty,
            args.map { case Param(ty, _) => ty }
          ))
        }

        implicit val funcTable: Map[Ident, FuncType] = funcTableMut.toMap
        program.funcSymbols = Some(funcTable)
        for (f @ Func(ty, _, args, body) <- funcs) {
          val argsTable: TypeTable = TypeTable(
            args.zipWithIndex.map { case (Param(ty, id), index) =>
              id -> (ty, index + 1) // To account for lr being pushed when entering function scope
            }.toMap,
            None,
            args.length
          )
          val (typeTable, newErrors, _) = validateBlock(Some(argsTable), body, Some(ty))
          f.symbols = Some(typeTable)
          errors ++= newErrors
        }
        val (typeTable, newErrors, finalPrintSymbols) = validateBlock(None, stats, None)
        program.mainSymbols = Some(typeTable)
        program.printSymbols = finalPrintSymbols.toMap
        errors ++= newErrors
      }
    }
    errors.toList
  }

  // validate stats. This function may be called recursively to validate
  // nested blocks.
  def validateBlock(
      parentSymbols: Option[TypeTable],
      stats: List[Stat],
      returnType: Option[Type]
  )(implicit
      file: String,
      fileLines: Array[String],
      funcTable: Map[Ident, FuncType],
      printSymbols: mutable.Map[(Int, Int), Type]
  ): (TypeTable, List[WaccError], mutable.Map[(Int, Int), Type]) = {
    val errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer.empty
    implicit var localSymbols: TypeTable = TypeTable(Map.empty, parentSymbols, 0)
    for (stat <- stats) {
      // match on different types of statements
      stat match {
        case Skip() =>
        case Declare(ty, id, rhs) => {
          val (maybeRhs, rhsErrors) =
            typeOfRhs(rhs)
          errors ++= rhsErrors
          maybeRhs match {
            case Some(rhsType) =>
              if (!(rhsType coercesTo ty)) {
                errors += TypeError.mkError(
                  "rhs of declaration statement",
                  Set(ty),
                  rhsType
                )
              }
            case _ =>
          }
          if (localSymbols locallyContains id) {
            errors += RedefinedVariableError.mkError(id)
          } else {
            localSymbols += (id -> ty)
          }
        }
        case Assign(lhs, rhs) => {
          val (maybeLhs, lhsErrors) =
            typeOfLhs(lhs)
          val (maybeRhs, rhsErrors) =
            typeOfRhs(rhs)
          errors ++= lhsErrors
          errors ++= rhsErrors
          (maybeLhs, maybeRhs) match {
            case (Some(lhsType), Some(rhsType)) =>
              if (!(rhsType coercesTo lhsType)) {
                errors += TypeError.mkError(
                  "rhs of assignment statement",
                  Set(lhsType),
                  rhsType
                )
              }
            case _ =>
          }
        }
        case Read(lhs) => {
          val (maybeType, lhsErrors) =
            typeOfLhs(lhs)
          errors ++= lhsErrors
          maybeType match {
            case Some(IntType()) => printSymbols += lhs.pos -> INT_TYPE
            case Some(CharType()) => printSymbols += lhs.pos -> CHAR_TYPE
            case Some(ty) =>
              errors += TypeError.mkError(
                "argument of read statement",
                Set(INT_TYPE, CHAR_TYPE),
                ty
              )
            case None =>
          }
        }
        case Free(expr) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(PairType(_, _)) =>
            case Some(ArrayType(_))   =>
            case Some(ty) =>
              errors += TypeError.mkError(
                "argument of free statement",
                Set(PAIR_TYPE, ARRAY_TYPE),
                ty
              )
            case _ =>
          }
        }
        case returnExpr @ Return(expr) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(expr)
          errors ++= exprErrors
          (maybeExpr, returnType) match {
            case (Some(exprType), Some(ty)) =>
              if (!(exprType coercesTo ty)) {
                errors += TypeError.mkError(
                  "argument of return statement",
                  Set(ty),
                  exprType
                )
              }
            case (_, None) =>
              errors += MisplacedReturnError.mkError(returnExpr)
            case (None, Some(_)) =>
          }
        }
        case Exit(expr) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(IntType()) =>
            case Some(ty) =>
              errors += TypeError.mkError(
                "argument of exit statement",
                Set(INT_TYPE),
                ty
              )
            case _ =>
          }
        }
        case Print(expr) =>
          val (maybeType, maybeErrors) = typeOfExpr(expr)
          errors ++= maybeErrors
          maybeType match {
            case Some(exprType) => printSymbols += expr.pos -> exprType
            case _ =>
          }
        case Println(expr) =>
          val (maybeType, maybeErrors) = typeOfExpr(expr)
          errors ++= maybeErrors
          maybeType match {
            case Some(exprType) => printSymbols += expr.pos -> exprType
            case _ =>
          }
        case s@If(expr, thenStats, elseStats) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(ty) =>
              errors += TypeError.mkError(
                "condition of if statement",
                Set(BOOL_TYPE),
                ty
              )
            case _ =>
          }
          val (thenTypeTable, thenErrors, _) = validateBlock(Some(localSymbols), thenStats, returnType)
          errors ++= thenErrors
          s.thenTypeTable = Some(thenTypeTable)
          val (elseTypeTable, elseErrors, _) = validateBlock(Some(localSymbols), elseStats, returnType)
          errors ++= elseErrors
          s.elseTypeTable = Some(elseTypeTable)
        }
        case s@While(expr, doStats) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(ty) =>
              errors += TypeError.mkError(
                "condition of while statement",
                Set(BOOL_TYPE),
                ty
              )
            case _ =>
          }
          val (doTypeTable, doErrors, _) = validateBlock(Some(localSymbols), doStats, returnType)
          errors ++= doErrors
          s.doTypeTable = Some(doTypeTable)
        }
        case s@Scope(innerStats) =>
          val (statsTypeTable, statsErrors, _) = validateBlock(Some(localSymbols), innerStats, returnType)
          errors ++= statsErrors
          s.typeTable = Some(statsTypeTable)
      }
    }
    (localSymbols, errors.toList, printSymbols)
  }

  // validate arguments for a given binary operator, returning type ret if arguments type-check
  private def typeOfBinOp(
      argTypes: Set[Type],
      x: Expr,
      y: Expr,
      ret: Type,
      opName: String
  )(implicit
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String],
      printSymbols: mutable.Map[(Int, Int), Type]
  ): (Option[Type], List[WaccError]) = {
    val (maybeTypes, errors) = typeOfExpr2(x, y)
    maybeTypes match {
      case Some((xType, yType)) => {
        if (!argTypes.exists(xType coercesTo _))
          (
            None,
            errors :+ TypeError.mkError(
              s"first argument of $opName",
              argTypes,
              xType
            )
          )
        else if (!argTypes.exists(yType coercesTo _))
          (
            None,
            errors :+ TypeError.mkError(
              s"second argument of $opName",
              argTypes,
              yType
            )
          )
        else if (!((xType coercesTo yType) || (yType coercesTo xType)))
          (
            None,
            errors :+ TypeError.mkError(
              s"arguments of $opName",
              Set(xType),
              yType
            )
          )
        else (Some(ret), errors)
      }
      case _ => (None, errors)
    }
  }

  // validate argument for a given binary operator, returning type ret if argument type-checks
  private def typeOfUnOp(
      argType: Set[Type],
      x: Expr,
      ret: Type,
      opName: String
  )(implicit
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String],
      printSymbols: mutable.Map[(Int, Int), Type]
  ): (Option[Type], List[WaccError]) = {
    val (maybeXType, xErrors) = typeOfExpr(x)
    maybeXType match {
      case Some(xType) => {
        if (!argType.exists(xType coercesTo _))
          (
            None,
            xErrors :+ TypeError.mkError(s"argument of $opName", argType, xType)
          )
        else (Some(ret), xErrors)
      }
      case _ => (None, xErrors)
    }
  }

  def typeOfExpr(expr: Expr)(implicit
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String],
      printSymbols: mutable.Map[(Int, Int), Type]
  ): (Option[Type], List[WaccError]) = {
    expr match {
      case orExpr @ Or(x, y) =>
        typeOfBinOp(
          Set(BOOL_TYPE),
          x,
          y,
          BoolType()(orExpr.pos),
          "||"
        )
      case andExpr @ And(x, y) =>
        typeOfBinOp(
          Set(BOOL_TYPE),
          x,
          y,
          BoolType()(andExpr.pos),
          "&&"
        )
      case eqExpr @ Eq(x, y) =>
        typeOfBinOp(
          EQ_ARG_TYPES,
          x,
          y,
          BoolType()(eqExpr.pos),
          "=="
        )
      case neqExpr @ Neq(x, y) =>
        typeOfBinOp(
          EQ_ARG_TYPES,
          x,
          y,
          BoolType()(neqExpr.pos),
          "!="
        )
      case leqExpr @ Leq(x, y) =>
        typeOfBinOp(
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(leqExpr.pos),
          "<="
        )
      case ltExpr @ Lt(x, y) =>
        typeOfBinOp(
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(ltExpr.pos),
          "<"
        )
      case geqExpr @ Geq(x, y) =>
        typeOfBinOp(
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(geqExpr.pos),
          ">="
        )
      case gtExpr @ Gt(x, y) =>
        typeOfBinOp(
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(gtExpr.pos),
          ">"
        )
      case addExpr @ Add(x, y) =>
        typeOfBinOp(
          Set(INT_TYPE),
          x,
          y,
          IntType()(addExpr.pos),
          "+"
        )
      case subExpr @ Sub(x, y) =>
        typeOfBinOp(
          Set(INT_TYPE),
          x,
          y,
          IntType()(subExpr.pos),
          "-"
        )
      case mulExpr @ Mul(x, y) =>
        typeOfBinOp(
          Set(INT_TYPE),
          x,
          y,
          IntType()(mulExpr.pos),
          "*"
        )
      case divExpr @ Div(x, y) =>
        typeOfBinOp(
          Set(INT_TYPE),
          x,
          y,
          IntType()(divExpr.pos),
          "/"
        )
      case modExpr @ Mod(x, y) =>
        typeOfBinOp(
          Set(INT_TYPE),
          x,
          y,
          IntType()(modExpr.pos),
          "%"
        )
      case notExpr @ Not(x) =>
        typeOfUnOp(Set(BOOL_TYPE), x, BoolType()(notExpr.pos), "!")
      case negExpr @ Neg(x) =>
        typeOfUnOp(Set(INT_TYPE), x, IntType()(negExpr.pos), "-")
      case lenExpr @ Len(x) =>
        typeOfUnOp(
          Set(ARRAY_TYPE),
          x,
          IntType()(lenExpr.pos),
          "len"
        )
      case ordExpr @ Ord(x) =>
        typeOfUnOp(
          Set(CHAR_TYPE),
          x,
          IntType()(ordExpr.pos),
          "ord"
        )
      case chrExpr @ Chr(x) =>
        typeOfUnOp(
          Set(INT_TYPE),
          x,
          CharType()(chrExpr.pos),
          "chr"
        )
      case nullExpr @ Null() =>
        (
          Some(
            PairType(AnyType()(nullExpr.pos), AnyType()(nullExpr.pos))(
              nullExpr.pos
            )
          ),
          Nil
        )
      case Paren(expr) => typeOfExpr(expr)
      case identExpr: Ident =>
        (typeTable getType identExpr) match {
          // Need to find out what type it is because we need to construct a new object to change the pos
          case Some(ty) => (Some(ty.withPos(identExpr.pos)), Nil)
          case None =>
            (
              None,
              List(
                UndefinedVariableError.mkError(identExpr)
              )
            )
        }
      case intExpr: IntLiter   => (Some(IntType()(intExpr.pos)), Nil)
      case strExpr: StrLiter   => (Some(StringType()(strExpr.pos)), Nil)
      case boolExpr: BoolLiter => (Some(BoolType()(boolExpr.pos)), Nil)
      case charExpr: CharLiter => (Some(CharType()(charExpr.pos)), Nil)
      case arrayExpr @ ArrayLiter(Nil) => {
        printSymbols += (arrayExpr.pos -> ArrayType(ANY_TYPE)(arrayExpr.pos))
        (Some(ArrayType(ANY_TYPE)(arrayExpr.pos)), Nil)}
      case arrayExpr @ ArrayLiter(expr :: exprs) => {
        val (maybeTypes, errors) =
          typeOfExpr2(expr, ArrayLiter(exprs)(arrayExpr.pos))
        maybeTypes match {
          case Some((a, ArrayType(b))) => {
            printSymbols += (arrayExpr.pos -> maybeTypes.get._2)
            if (b coercesTo a) (Some(ArrayType(a)(arrayExpr.pos)), errors)
            else if (a coercesTo b) (Some(ArrayType(b)(arrayExpr.pos)), errors)
            else
              (
                None,
                errors :+ TypeError.mkError("elements of array", Set(a), b)
              )
          }
          case _ => (None, errors)
        }
      }
      case arrayElem @ ArrayElem(id, index: Expr) => {
        val (maybeIndexType, indexErrors) = typeOfExpr(index)
        maybeIndexType match {
          case Some(IntType()) => {
            val (maybeArrayType, arrayErrors) = typeOfExpr(id)
            val errors = indexErrors ++ arrayErrors
            maybeArrayType match {
              case Some(ArrayType(innerType)) =>
                printSymbols += arrayElem.pos -> innerType
                (Some(innerType.withPos(arrayElem.pos)), errors)
              case Some(ty) =>
                (
                  None,
                  errors :+ TypeError.mkError(
                    "array",
                    Set(ARRAY_TYPE),
                    ty.withPos(arrayElem.pos)
                  )
                )
              case None => (None, errors)
            }
          }
          case Some(ty) =>
            (
              None,
              indexErrors :+ TypeError.mkError(
                "array index",
                Set(INT_TYPE),
                ty.withPos(index.pos)
              )
            )
          case None => (None, indexErrors)
        }
      }
    }
  }

  def typeOfExpr2(x: Expr, y: Expr)(implicit
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String],
      printSymbols: mutable.Map[(Int, Int), Type]
  ): (Option[(Type, Type)], List[WaccError]) = {
    val (maybeXType, xErrors) = typeOfExpr(x)
    val (maybeYType, yErrors) = typeOfExpr(y)
    val errors = xErrors ++ yErrors
    (maybeXType, maybeYType) match {
      case (Some(xType), Some(yType)) => (Some((xType, yType)), errors)
      case _                          => (None, errors)
    }
  }

  // get type of rhs and checking for semantic errors within rhs
  def typeOfRhs(
      rhs: AssignRhs
  )(implicit
      funcTable: Map[Ident, FuncType],
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String],
      printSymbols: mutable.Map[(Int, Int), Type]
  ): (Option[Type], List[WaccError]) = {
    rhs match {
      case rhs @ NewPair(fst, snd) => {
        val (maybeTypes, errors) = typeOfExpr2(fst, snd)
        maybeTypes match {
          case Some((fstType, sndType)) =>
            printSymbols += fst.pos -> fstType
            printSymbols += snd.pos -> sndType
            (
              Some(
                PairType(fstType.toPairElemType, sndType.toPairElemType)(
                  rhs.pos
                )
              ),
              errors
            )
          case _ => (None, errors)
        }
      }
      case Fst(expr) => {
        if (expr == Null()(NO_POS))
          (
            None,
            List(NullExceptionError.mkError(s"argument of $rhs", expr))
          )
        else {
          val (maybeExprType, exprErrors) = typeOfExpr(expr)
          maybeExprType match {
            case Some(pt@PairType(fstType, _)) =>
              printSymbols += expr.pos -> pt
              (Some(fstType.toType), exprErrors)
            case Some(ty) =>
              (
                None,
                exprErrors :+ TypeError.mkError(
                  s"argument of $rhs",
                  Set(PAIR_TYPE),
                  ty
                )
              )
            case None => (None, exprErrors)
          }
        }
      }
      case Snd(expr) => {
        if (expr == Null()(NO_POS))
          (
            None,
            List(NullExceptionError.mkError(s"argument of $rhs", expr))
          )
        else {
          val (maybeExprType, exprErrors) = typeOfExpr(expr)
          maybeExprType match {
            case Some(pt@PairType(_, sndType)) =>
              printSymbols += expr.pos -> pt
              (Some(sndType.toType), exprErrors)
            case Some(ty) =>
              (
                None,
                exprErrors :+ TypeError.mkError(
                  s"argument of $rhs",
                  Set(PAIR_TYPE),
                  ty
                )
              )
            case None => (None, exprErrors)
          }
        }
      }
      case callExpr @ Call(id, args) => {
        val (maybeArgTypes, argErrorLists) =
          args.map(typeOfExpr(_)).unzip
        val argErrors = argErrorLists.flatten
        if (maybeArgTypes contains None) (None, argErrors)
        else {
          val argTypes = maybeArgTypes.map(_.get)
          (funcTable get id) match {
            case Some(FuncType(returnType, paramTypes)) => {
              if (argTypes.length != paramTypes.length)
                (
                  None,
                  argErrors :+ NumOfArgsError.mkError(
                    id,
                    paramTypes.length,
                    argTypes.length
                  )
                )
              else if (
                argTypes.lazyZip(paramTypes).map(_ coercesTo _).forall(identity)
              ) (Some(returnType.withPos(callExpr.pos)), argErrors)
              else {
                (
                  None,
                  argErrors ++ (argTypes zip paramTypes).collect {
                    case (argType, paramType)
                        if (!(argType coercesTo paramType)) =>
                      TypeError.mkError(
                        s"argument of $id",
                        Set(paramType),
                        argType
                      )
                  }
                )
              }
            }
            case None =>
              (
                None,
                argErrors :+ UndefinedFunctionError.mkError(id)
              )
          }
        }
      }
      // matching by a type normally doesn't work because of type erasure.
      // However, this is fine because this case is actually the default;
      // all subtypes of AssignRhs which are not also subtypes of Expr
      // Are already handled by the above cases, so expr is always an Expr.
      // The type annotation is only needed to make scala notice that this is
      // the case.
      case expr: Expr => typeOfExpr(expr)
    }
  }

  def typeOfLhs(
      lhs: AssignLhs
  )(implicit
      funcTable: Map[Ident, FuncType],
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String],
      printSymbols: mutable.Map[(Int, Int), Type]
  ): (Option[Type], List[WaccError]) = {
    // Every subtype of AssignLhs is also a subtype of AssignRhs. This method
    // exists anyway for easier extensibility if this were to change
    lhs match {
      case rhs: AssignRhs => typeOfRhs(rhs)
    }
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])

}

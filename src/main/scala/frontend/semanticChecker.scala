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
        val funcTableMut: mutable.Map[Ident, FuncType] = mutable.Map.empty
        val localPrintSymbols = mutable.Map.empty[(Int, Int), Type]
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
          val (typeTable, newErrors, funcsPrintSymbols) = validateBlock(Some(argsTable), body, Some(ty))
          localPrintSymbols ++= funcsPrintSymbols
          f.symbols = Some(typeTable)
          errors ++= newErrors
        }
        val (typeTable, newErrors, blockPrintSymbols) = validateBlock(None, stats, None)
        localPrintSymbols ++= blockPrintSymbols
        program.mainSymbols = Some(typeTable)
        program.printSymbols = localPrintSymbols.toMap
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
      funcTable: Map[Ident, FuncType]
  ): (TypeTable, List[WaccError], Map[(Int, Int), Type]) = {
    val errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer.empty
    implicit var localSymbols: TypeTable = TypeTable(Map.empty, parentSymbols, 0)
    val localPrintSymbols = mutable.Map.empty[(Int, Int), Type]
    for (stat <- stats) {
      // match on different types of statements
      stat match {
        case Skip() =>
        case Declare(ty, id, rhs) => {
          val (maybeRhs, rhsErrors, printSymbols) =
            typeOfRhs(rhs)
          localPrintSymbols ++= printSymbols
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
          val (maybeLhs, lhsErrors, lhsPrintSymbols) =
            typeOfLhs(lhs)
          val (maybeRhs, rhsErrors, rhsPrintSymbols) =
            typeOfRhs(rhs)
          localPrintSymbols ++= lhsPrintSymbols
          localPrintSymbols ++= rhsPrintSymbols
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
          val (maybeType, lhsErrors, lhsPrintSymbols) =
            typeOfLhs(lhs)
          localPrintSymbols ++= lhsPrintSymbols
          errors ++= lhsErrors
          maybeType match {
            case Some(IntType()) => localPrintSymbols += lhs.pos -> INT_TYPE
            case Some(CharType()) => localPrintSymbols += lhs.pos -> CHAR_TYPE
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
          val (maybeExpr, exprErrors, printSymbols) =
            typeOfExpr(expr)
          localPrintSymbols ++= printSymbols
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
          val (maybeExpr, exprErrors, printSymbols) =
            typeOfExpr(expr)
          localPrintSymbols ++= printSymbols
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
          val (maybeExpr, exprErrors, printSymbols) =
            typeOfExpr(expr)
          localPrintSymbols ++= printSymbols
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
          val (maybeType, maybeErrors, printSymbols) = typeOfExpr(expr)
          localPrintSymbols ++= printSymbols
          errors ++= maybeErrors
          maybeType match {
            case Some(exprType) => localPrintSymbols += expr.pos -> exprType
            case _ =>
          }
        case Println(expr) =>
          val (maybeType, maybeErrors, printSymbols) = typeOfExpr(expr)
          localPrintSymbols ++= printSymbols
          errors ++= maybeErrors
          maybeType match {
            case Some(exprType) => localPrintSymbols += expr.pos -> exprType
            case _ =>
          }
        case s@If(expr, thenStats, elseStats) => {
          val (maybeExpr, exprErrors, condPrintSymbols) =
            typeOfExpr(expr)
          localPrintSymbols ++= condPrintSymbols
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
          val (thenTypeTable, thenErrors, thenPrintSymbols) = validateBlock(Some(localSymbols), thenStats, returnType)
          localPrintSymbols ++= thenPrintSymbols
          errors ++= thenErrors
          s.thenTypeTable = Some(thenTypeTable)
          val (elseTypeTable, elseErrors, elsePrintSymbols) = validateBlock(Some(localSymbols), elseStats, returnType)
          localPrintSymbols ++= elsePrintSymbols
          errors ++= elseErrors
          s.elseTypeTable = Some(elseTypeTable)
        }
        case s@While(expr, doStats) => {
          val (maybeExpr, exprErrors, condPrintSymbols) =
            typeOfExpr(expr)
          localPrintSymbols ++= condPrintSymbols
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
          val (doTypeTable, doErrors, doPrintSymbols) = validateBlock(Some(localSymbols), doStats, returnType)
          localPrintSymbols ++= doPrintSymbols
          errors ++= doErrors
          s.doTypeTable = Some(doTypeTable)
        }
        case s@Scope(innerStats) =>
          val (statsTypeTable, statsErrors, printSymbols) = validateBlock(Some(localSymbols), innerStats, returnType)
          localPrintSymbols ++= printSymbols
          errors ++= statsErrors
          s.typeTable = Some(statsTypeTable)
      }
    }
    (localSymbols, errors.toList, localPrintSymbols.toMap)
  }

  // validate arguments for a given binary operator, returning type ret if arguments type-check
  private def typeOfBinOp(
      argTypes: Set[Type],
      x: Expr,
      y: Expr,
      ret: Type,
      opName: String)(implicit
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String]
  ): (Option[Type], List[WaccError], Map[(Int, Int), Type]) = {
    var localPrintSymbols = mutable.Map.empty[(Int, Int), Type]
    val (maybeTypes, errors, printSymbols) = typeOfExpr2(x, y)
    localPrintSymbols ++= printSymbols
    maybeTypes match {
      case Some((xType, yType)) => {
        if (!argTypes.exists(xType coercesTo _))
          (
            None,
            errors :+ TypeError.mkError(
              s"first argument of $opName",
              argTypes,
              xType
            ),
            localPrintSymbols.toMap
          )
        else if (!argTypes.exists(yType coercesTo _))
          (
            None,
            errors :+ TypeError.mkError(
              s"second argument of $opName",
              argTypes,
              yType
            ),
            localPrintSymbols.toMap
          )
        else if (!((xType coercesTo yType) || (yType coercesTo xType)))
          (
            None,
            errors :+ TypeError.mkError(
              s"arguments of $opName",
              Set(xType),
              yType
            ),
            localPrintSymbols.toMap
          )
        else (Some(ret), errors, localPrintSymbols.toMap)
      }
      case _ => (None, errors, localPrintSymbols.toMap)
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
      fileLines: Array[String]
  ): (Option[Type], List[WaccError], Map[(Int, Int), Type]) = {
    var localPrintSymbols = mutable.Map.empty[(Int, Int), Type]
    val (maybeXType, xErrors, printSymbols) = typeOfExpr(x)
    localPrintSymbols ++= printSymbols
    maybeXType match {
      case Some(xType) => {
        if (!argType.exists(xType coercesTo _))
          (
            None,
            xErrors :+ TypeError.mkError(s"argument of $opName", argType, xType),
            localPrintSymbols.toMap
          )
        else (Some(ret), xErrors, localPrintSymbols.toMap)
      }
      case _ => (None, xErrors, localPrintSymbols.toMap)
    }
  }

  def typeOfExpr(expr: Expr)(implicit
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String]
  ): (Option[Type], List[WaccError], Map[(Int, Int), Type]) = {
    var localPrintSymbols = mutable.Map.empty[(Int, Int), Type]
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
          Nil,
          localPrintSymbols.toMap
        )
      case Paren(expr) => typeOfExpr(expr)
      case identExpr: Ident =>
        (typeTable getType identExpr) match {
          // Need to find out what type it is because we need to construct a new object to change the pos
          case Some(ty) => (Some(ty.withPos(identExpr.pos)), Nil, localPrintSymbols.toMap)
          case None =>
            (
              None,
              List(
                UndefinedVariableError.mkError(identExpr)
              ),
              localPrintSymbols.toMap
            )
        }
      case intExpr: IntLiter   => (Some(IntType()(intExpr.pos)), Nil, localPrintSymbols.toMap)
      case strExpr: StrLiter   => (Some(StringType()(strExpr.pos)), Nil, localPrintSymbols.toMap)
      case boolExpr: BoolLiter => (Some(BoolType()(boolExpr.pos)), Nil, localPrintSymbols.toMap)
      case charExpr: CharLiter => (Some(CharType()(charExpr.pos)), Nil, localPrintSymbols.toMap)
      case arrayExpr @ ArrayLiter(Nil) => {
        localPrintSymbols += (arrayExpr.pos -> ArrayType(ANY_TYPE)(arrayExpr.pos))
        (Some(ArrayType(ANY_TYPE)(arrayExpr.pos)), Nil, localPrintSymbols.toMap)}
      case arrayExpr @ ArrayLiter(expr :: exprs) => {
        val (maybeTypes, errors, printSymbols) =
          typeOfExpr2(expr, ArrayLiter(exprs)(arrayExpr.pos))
        localPrintSymbols ++= printSymbols
        maybeTypes match {
          case Some((a, ArrayType(b))) => {
            if (b coercesTo a) {
              localPrintSymbols += (arrayExpr.pos -> ArrayType(a)(arrayExpr.pos))
              (Some(ArrayType(a)(arrayExpr.pos)), errors, localPrintSymbols.toMap)
            }
            else if (a coercesTo b) {
              localPrintSymbols += (arrayExpr.pos -> ArrayType(b)(arrayExpr.pos))
              (Some(ArrayType(b)(arrayExpr.pos)), errors, localPrintSymbols.toMap)
            }
            else
              (
                None,
                errors :+ TypeError.mkError("elements of array", Set(a), b),
                localPrintSymbols.toMap
              )
          }
          case _ => (None, errors, localPrintSymbols.toMap)
        }
      }
      case arrayElem @ ArrayElem(id, index: Expr) => {
        val (maybeIndexType, indexErrors, printSymbols) = typeOfExpr(index)
        localPrintSymbols ++= printSymbols
        maybeIndexType match {
          case Some(IntType()) => {
            val (maybeArrayType, arrayErrors, idPrintSymbols) = typeOfExpr(id)
            localPrintSymbols ++= idPrintSymbols
            val errors = indexErrors ++ arrayErrors
            maybeArrayType match {
              case Some(ArrayType(innerType)) =>
                localPrintSymbols += arrayElem.pos -> innerType
                (Some(innerType.withPos(arrayElem.pos)), errors, localPrintSymbols.toMap)
              case Some(ty) =>
                (
                  None,
                  errors :+ TypeError.mkError(
                    "array",
                    Set(ARRAY_TYPE),
                    ty.withPos(arrayElem.pos)
                  ),
                  localPrintSymbols.toMap
                )
              case None => (None, errors, localPrintSymbols.toMap)
            }
          }
          case Some(ty) =>
            (
              None,
              indexErrors :+ TypeError.mkError(
                "array index",
                Set(INT_TYPE),
                ty.withPos(index.pos)
              ),
              localPrintSymbols.toMap
            )
          case None => (None, indexErrors, localPrintSymbols.toMap)
        }
      }
    }
  }

  def typeOfExpr2(x: Expr, y: Expr)(implicit
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String]
  ): (Option[(Type, Type)], List[WaccError], Map[(Int, Int), Type]) = {
    var localPrintSymbols = mutable.Map.empty[(Int, Int), Type]
    val (maybeXType, xErrors, xPrintSymbols) = typeOfExpr(x)
    val (maybeYType, yErrors, yPrintSymbols) = typeOfExpr(y)
    val errors = xErrors ++ yErrors
    localPrintSymbols ++= xPrintSymbols
    localPrintSymbols ++= yPrintSymbols
    (maybeXType, maybeYType) match {
      case (Some(xType), Some(yType)) => (Some((xType, yType)), errors, localPrintSymbols.toMap)
      case _                          => (None, errors, localPrintSymbols.toMap)
    }
  }

  // get type of rhs and checking for semantic errors within rhs
  def typeOfRhs(
      rhs: AssignRhs)(implicit
      funcTable: Map[Ident, FuncType],
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String]
  ): (Option[Type], List[WaccError], Map[(Int, Int), Type]) = {
    var localPrintSymbols = mutable.Map.empty[(Int, Int), Type]
    rhs match {
      case rhs @ NewPair(fst, snd) => {
        val (maybeTypes, errors, printTable) = typeOfExpr2(fst, snd)
        localPrintSymbols ++= printTable
        maybeTypes match {
          case Some((fstType, sndType)) =>
            localPrintSymbols += fst.pos -> fstType
            localPrintSymbols += snd.pos -> sndType
            (
              Some(
                PairType(fstType.toPairElemType, sndType.toPairElemType)(
                  rhs.pos
                )
              ),
              errors,
              localPrintSymbols.toMap
            )
          case _ => (None, errors, localPrintSymbols.toMap)
        }
      }
      case Fst(expr) => {
        if (expr == Null()(NO_POS))
          (
            None,
            List(NullExceptionError.mkError(s"argument of $rhs", expr)),
            localPrintSymbols.toMap
          )
        else {
          val (maybeExprType, exprErrors, printSymbols) = typeOfExpr(expr)
          localPrintSymbols ++= printSymbols
          maybeExprType match {
            case Some(pt@PairType(fstType, _)) =>
              localPrintSymbols += expr.pos -> pt
              (Some(fstType.toType), exprErrors, localPrintSymbols.toMap)
            case Some(ty) =>
              (
                None,
                exprErrors :+ TypeError.mkError(
                  s"argument of $rhs",
                  Set(PAIR_TYPE),
                  ty
                ),
                localPrintSymbols.toMap
              )
            case None => (None, exprErrors, localPrintSymbols.toMap)
          }
        }
      }
      case Snd(expr) => {
        if (expr == Null()(NO_POS))
          (
            None,
            List(NullExceptionError.mkError(s"argument of $rhs", expr)),
            localPrintSymbols.toMap
          )
        else {
          val (maybeExprType, exprErrors, printSymbols) = typeOfExpr(expr)
          maybeExprType match {
            case Some(pt@PairType(_, sndType)) =>
              localPrintSymbols += expr.pos -> pt
              (Some(sndType.toType), exprErrors, localPrintSymbols.toMap)
            case Some(ty) =>
              (
                None,
                exprErrors :+ TypeError.mkError(
                  s"argument of $rhs",
                  Set(PAIR_TYPE),
                  ty
                ),
                localPrintSymbols.toMap
              )
            case None => (None, exprErrors, localPrintSymbols.toMap)
          }
        }
      }
      case callExpr @ Call(id, args) => {
        val (maybeArgTypes, argErrorLists, printSymbolsList) =
          args.map(typeOfExpr(_)).unzip3
        val argErrors = argErrorLists.flatten
        localPrintSymbols ++= printSymbolsList.flatten
        if (maybeArgTypes contains None) (None, argErrors, localPrintSymbols.toMap)
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
                  ),
                  localPrintSymbols.toMap
                )
              else if (
                argTypes.lazyZip(paramTypes).map(_ coercesTo _).forall(identity)
              ) (Some(returnType.withPos(callExpr.pos)), argErrors, localPrintSymbols.toMap)
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
                  },
                  localPrintSymbols.toMap
                )
              }
            }
            case None =>
              (
                None,
                argErrors :+ UndefinedFunctionError.mkError(id),
                localPrintSymbols.toMap
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
      lhs: AssignLhs)(implicit
      funcTable: Map[Ident, FuncType],
      typeTable: TypeTable,
      file: String,
      fileLines: Array[String]
  ): (Option[Type], List[WaccError], Map[(Int, Int), Type]) = {
    // Every subtype of AssignLhs is also a subtype of AssignRhs. This method
    // exists anyway for easier extensibility if this were to change
    lhs match {
      case rhs: AssignRhs => typeOfRhs(rhs)
    }
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])

}

package frontend

import frontend.Errors._
import frontend.ast._

import scala.collection.mutable

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

  def validateProgram(program: WaccProgram, file: String): List[WaccError] = {
    implicit val fileImplicit: String = file
    val errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer.empty
    program match {
      case WaccProgram(funcs, stats) => {
        // generate function table
        val funcTableMut: mutable.Map[Ident, FuncType] = mutable.Map.empty
        for (f @ Func(ty, id, args, _) <- funcs) {
          if (funcTableMut contains id)
            errors += WaccError(f.pos, file, RedefinedFunctionError(id))
          funcTableMut += (id -> FuncType(
            ty,
            args.map { case Param(ty, _) => ty }
          ))
        }

        val funcTable: Map[Ident, FuncType] = funcTableMut.toMap
        // validate functions
        for (Func(ty, _, args, body) <- funcs) {
          val argsTable: Map[Ident, Type] = args.map { case Param(ty, id) =>
            id -> ty
          }.toMap
          errors ++= validateBlock(funcTable, argsTable, body, Some(ty))
        }
        errors ++= validateBlock(funcTable, Map.empty[Ident, Type], stats, None)
      }
    }
    errors.toList
  }

  // validate stats. This function may be called recursively to validate
  // nested blocks.
  def validateBlock(
      funcTable: Map[Ident, FuncType],
      parentSymbols: Map[Ident, Type],
      stats: List[Stat],
      returnType: Option[Type]
  )(implicit file: String): List[WaccError] = {
    val errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer.empty
    val localSymbols: mutable.Map[Ident, Type] = mutable.Map.empty[Ident, Type]
    for (stat <- stats) {
      // match on different types of statements
      stat match {
        case Skip() =>
        case Declare(ty, id, rhs) => {
          val (maybeRhs, rhsErrors) =
            typeOfRhs(funcTable, parentSymbols ++ localSymbols.toMap, rhs)
          errors ++= rhsErrors
          maybeRhs match {
            case Some(rhsType) =>
              if (!(rhsType coercesTo ty)) {
                errors += WaccError(
                  rhs.pos,
                  file,
                  TypeError("rhs of declaration statement", Set(ty), rhsType)
                )
              }
            case _ =>
          }
          if (localSymbols contains id) {
            errors += WaccError(rhs.pos, file, RedefinedVariableError(id))
          } else {
            localSymbols += (id -> ty)
          }
        }
        case Assign(lhs, rhs) => {
          val (maybeLhs, lhsErrors) =
            typeOfLhs(funcTable, parentSymbols ++ localSymbols.toMap, lhs)
          val (maybeRhs, rhsErrors) =
            typeOfRhs(funcTable, parentSymbols ++ localSymbols.toMap, rhs)
          errors ++= lhsErrors
          errors ++= rhsErrors
          (maybeLhs, maybeRhs) match {
            case (Some(lhsType), Some(rhsType)) =>
              if (!(rhsType coercesTo lhsType)) {
                errors += WaccError(
                  rhs.pos,
                  file,
                  TypeError(
                    "rhs of assignment statement",
                    Set(lhsType),
                    rhsType
                  )
                )
              }
            case _ =>
          }
        }
        case Read(lhs) => {
          val (maybeType, lhsErrors) =
            typeOfLhs(funcTable, parentSymbols ++ localSymbols.toMap, lhs)
          errors ++= lhsErrors
          maybeType match {
            case Some(IntType()) | Some(CharType()) =>
            case Some(ty) =>
              errors += WaccError(
                lhs.pos,
                file,
                TypeError(
                  "argument of read statement",
                  Set(INT_TYPE, CHAR_TYPE),
                  ty
                )
              )
            case None =>
          }
        }
        case Free(expr) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(parentSymbols ++ localSymbols.toMap, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(PairType(_, _)) =>
            case Some(ArrayType(_))   =>
            case Some(ty) =>
              errors += WaccError(
                expr.pos,
                file,
                TypeError(
                  "argument of free statement",
                  Set(PAIR_TYPE, ARRAY_TYPE),
                  ty
                )
              )
            case _ =>
          }
        }
        case returnExpr @ Return(expr) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(parentSymbols ++ localSymbols.toMap, expr)
          errors ++= exprErrors
          (maybeExpr, returnType) match {
            case (Some(exprType), Some(ty)) =>
              if (!(exprType coercesTo ty)) {
                errors += WaccError(
                  expr.pos,
                  file,
                  TypeError("argument of return statement", Set(ty), exprType)
                )
              }
            case (_, None) =>
              errors += WaccError(returnExpr.pos, file, MisplacedReturnError())
            case (None, Some(_)) =>
          }
        }
        case Exit(expr) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(parentSymbols ++ localSymbols.toMap, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(IntType()) =>
            case Some(ty) =>
              errors += WaccError(
                expr.pos,
                file,
                TypeError("argument of exit statement", Set(INT_TYPE), ty)
              )
            case _ =>
          }
        }
        case Print(expr) =>
          errors ++= typeOfExpr(parentSymbols ++ localSymbols.toMap, expr)._2
        case Println(expr) =>
          errors ++= typeOfExpr(parentSymbols ++ localSymbols.toMap, expr)._2
        case If(expr, thenStats, elseStats) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(parentSymbols ++ localSymbols.toMap, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(ty) =>
              errors += WaccError(
                expr.pos,
                file,
                TypeError("condition of if statement", Set(BOOL_TYPE), ty)
              )
            case _ =>
          }
          errors ++= validateBlock(
            funcTable,
            parentSymbols ++ localSymbols.toMap,
            thenStats,
            returnType
          )
          errors ++= validateBlock(
            funcTable,
            parentSymbols ++ localSymbols.toMap,
            elseStats,
            returnType
          )
        }
        case While(expr, doStats) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(parentSymbols ++ localSymbols.toMap, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(ty) =>
              errors += WaccError(
                expr.pos,
                file,
                TypeError("condition of while statement", Set(BOOL_TYPE), ty)
              )

            case _ =>
          }
          errors ++= validateBlock(
            funcTable,
            parentSymbols ++ localSymbols.toMap,
            doStats,
            returnType
          )
        }
        case Scope(innerStats) =>
          errors ++= validateBlock(
            funcTable,
            parentSymbols ++ localSymbols.toMap,
            innerStats,
            returnType
          )
      }
    }
    errors.toList
  }

  // validate arguments for a given binary operator, returning type ret if arguments type-check
  private def typeOfBinOp(
      symbolTable: Map[Ident, Type],
      argTypes: Set[Type],
      x: Expr,
      y: Expr,
      ret: Type,
      opName: String
  )(implicit file: String): (Option[Type], List[WaccError]) = {
    val (maybeTypes, errors) = typeOfExpr2(symbolTable, x, y)
    maybeTypes match {
      case Some((xType, yType)) => {
        if (!argTypes.exists(xType coercesTo _))
          (
            None,
            errors :+ WaccError(
              x.pos,
              file,
              TypeError(s"first argument of $opName", argTypes, xType)
            )
          )
        else if (!argTypes.exists(yType coercesTo _))
          (
            None,
            errors :+ WaccError(
              y.pos,
              file,
              TypeError(s"second argument of $opName", argTypes, yType)
            )
          )
        else if (!((xType coercesTo yType) || (yType coercesTo xType)))
          (
            None,
            errors :+ WaccError(
              x.pos,
              file,
              TypeError(s"arguments of $opName", Set(xType), yType)
            )
          )
        else (Some(ret), errors)
      }
      case _ => (None, errors)
    }
  }

  // validate argument for a given binary operator, returning type ret if argument type-checks
  private def typeOfUnOp(
      symbolTable: Map[Ident, Type],
      argType: Set[Type],
      x: Expr,
      ret: Type,
      opName: String
  )(implicit file: String): (Option[Type], List[WaccError]) = {
    val (maybeXType, xErrors) = typeOfExpr(symbolTable, x)
    maybeXType match {
      case Some(xType) => {
        if (!argType.exists(xType coercesTo _))
          (
            None,
            xErrors :+ WaccError(
              x.pos,
              file,
              TypeError(s"argument of $opName", argType, xType)
            )
          )
        else (Some(ret), xErrors)
      }
      case _ => (None, xErrors)
    }
  }

  // return expected type of a given expression
  def typeOfExpr(symbolTable: Map[Ident, Type], expr: Expr)(implicit
      file: String
  ): (Option[Type], List[WaccError]) = {
    expr match {
      case orExpr @ Or(x, y) =>
        typeOfBinOp(
          symbolTable,
          Set(BOOL_TYPE),
          x,
          y,
          BoolType()(orExpr.pos),
          "||"
        )
      case andExpr @ And(x, y) =>
        typeOfBinOp(
          symbolTable,
          Set(BOOL_TYPE),
          x,
          y,
          BoolType()(andExpr.pos),
          "&&"
        )
      case eqExpr @ Eq(x, y) =>
        typeOfBinOp(
          symbolTable,
          EQ_ARG_TYPES,
          x,
          y,
          BoolType()(eqExpr.pos),
          "=="
        )
      case neqExpr @ Neq(x, y) =>
        typeOfBinOp(
          symbolTable,
          EQ_ARG_TYPES,
          x,
          y,
          BoolType()(neqExpr.pos),
          "!="
        )
      case leqExpr @ Leq(x, y) =>
        typeOfBinOp(
          symbolTable,
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(leqExpr.pos),
          "<="
        )
      case ltExpr @ Lt(x, y) =>
        typeOfBinOp(
          symbolTable,
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(ltExpr.pos),
          "<"
        )
      case geqExpr @ Geq(x, y) =>
        typeOfBinOp(
          symbolTable,
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(geqExpr.pos),
          ">="
        )
      case gtExpr @ Gt(x, y) =>
        typeOfBinOp(
          symbolTable,
          COMP_ARG_TYPES,
          x,
          y,
          BoolType()(gtExpr.pos),
          ">"
        )
      case addExpr @ Add(x, y) =>
        typeOfBinOp(
          symbolTable,
          Set(INT_TYPE),
          x,
          y,
          IntType()(addExpr.pos),
          "+"
        )
      case subExpr @ Sub(x, y) =>
        typeOfBinOp(
          symbolTable,
          Set(INT_TYPE),
          x,
          y,
          IntType()(subExpr.pos),
          "-"
        )
      case mulExpr @ Mul(x, y) =>
        typeOfBinOp(
          symbolTable,
          Set(INT_TYPE),
          x,
          y,
          IntType()(mulExpr.pos),
          "*"
        )
      case divExpr @ Div(x, y) =>
        typeOfBinOp(
          symbolTable,
          Set(INT_TYPE),
          x,
          y,
          IntType()(divExpr.pos),
          "/"
        )
      case modExpr @ Mod(x, y) =>
        typeOfBinOp(
          symbolTable,
          Set(INT_TYPE),
          x,
          y,
          IntType()(modExpr.pos),
          "%"
        )
      case notExpr @ Not(x) =>
        typeOfUnOp(symbolTable, Set(BOOL_TYPE), x, BoolType()(notExpr.pos), "!")
      case negExpr @ Neg(x) =>
        typeOfUnOp(symbolTable, Set(INT_TYPE), x, IntType()(negExpr.pos), "-")
      case lenExpr @ Len(x) =>
        typeOfUnOp(
          symbolTable,
          Set(ARRAY_TYPE),
          x,
          IntType()(lenExpr.pos),
          "len"
        )
      case ordExpr @ Ord(x) =>
        typeOfUnOp(
          symbolTable,
          Set(CHAR_TYPE),
          x,
          IntType()(ordExpr.pos),
          "ord"
        )
      case chrExpr @ Chr(x) =>
        typeOfUnOp(
          symbolTable,
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
      case Paren(expr) => typeOfExpr(symbolTable, expr)
      case identExpr: Ident =>
        (symbolTable get identExpr) match {
          case Some(ty) => (Some(ty), Nil)
          case None =>
            (
              None,
              List(
                WaccError(
                  identExpr.pos,
                  file,
                  UndefinedVariableError(identExpr)
                )
              )
            )
        }
      case intExpr: IntLiter   => (Some(IntType()(intExpr.pos)), Nil)
      case strExpr: StrLiter   => (Some(StringType()(strExpr.pos)), Nil)
      case boolExpr: BoolLiter => (Some(BoolType()(boolExpr.pos)), Nil)
      case charExpr: CharLiter => (Some(CharType()(charExpr.pos)), Nil)
      case arrayExpr @ ArrayLiter(Nil) =>
        (Some(ArrayType(ANY_TYPE)(arrayExpr.pos)), Nil)
      case arrayExpr @ ArrayLiter(expr :: exprs) => {
        val (maybeTypes, errors) =
          typeOfExpr2(symbolTable, expr, ArrayLiter(exprs)(arrayExpr.pos))
        maybeTypes match {
          case Some((a, ArrayType(b))) => {
            if (b coercesTo a) (Some(ArrayType(a)(arrayExpr.pos)), errors)
            else if (a coercesTo b) (Some(ArrayType(b)(arrayExpr.pos)), errors)
            else
              (
                None,
                errors :+ WaccError(
                  arrayExpr.pos,
                  file,
                  TypeError("elements of array", Set(a), b)
                )
              )
          }
          case _ => (None, errors)
        }
      }
      case arrayElem @ ArrayElem(id, index: Expr) => {
        val (maybeIndexType, indexErrors) = typeOfExpr(symbolTable, index)
        maybeIndexType match {
          case Some(IntType()) => {
            val (maybeArrayType, arrayErrors) = typeOfExpr(symbolTable, id)
            val errors = indexErrors ++ arrayErrors
            maybeArrayType match {
              case Some(ArrayType(innerType)) => (Some(innerType), errors)
              case Some(ty) =>
                (
                  None,
                  errors :+ WaccError(
                    arrayElem.pos,
                    file,
                    TypeError("array", Set(ARRAY_TYPE), ty)
                  )
                )
              case None => (None, errors)
            }
          }
          case Some(ty) =>
            (
              None,
              indexErrors :+ WaccError(
                arrayElem.pos,
                file,
                TypeError("array index", Set(INT_TYPE), ty)
              )
            )
          case None => (None, indexErrors)
        }
      }
    }
  }

  // determine types of expressions x and y
  def typeOfExpr2(symbolTable: Map[Ident, Type], x: Expr, y: Expr)(implicit
      file: String
  ): (Option[(Type, Type)], List[WaccError]) = {
    val (maybeXType, xErrors) = typeOfExpr(symbolTable, x)
    val (maybeYType, yErrors) = typeOfExpr(symbolTable, y)
    val errors = xErrors ++ yErrors
    (maybeXType, maybeYType) match {
      case (Some(xType), Some(yType)) => (Some((xType, yType)), errors)
      case _                          => (None, errors)
    }
  }

  // get type of rhs and checking for semantic errors within rhs
  def typeOfRhs(
      funcTable: Map[Ident, FuncType],
      symbolTable: Map[Ident, Type],
      rhs: AssignRhs
  )(implicit file: String): (Option[Type], List[WaccError]) = {
    rhs match {
      case rhs @ NewPair(fst, snd) => {
        val (maybeTypes, errors) = typeOfExpr2(symbolTable, fst, snd)
        maybeTypes match {
          case Some((fstType, sndType)) =>
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
            List(
              WaccError(expr.pos, file, NullExceptionError(s"argument of $rhs"))
            )
          )
        else {
          val (maybeExprType, exprErrors) = typeOfExpr(symbolTable, expr)
          maybeExprType match {
            case Some(PairType(fstType, _)) =>
              (Some(fstType.toType), exprErrors)
            case Some(ty) =>
              (
                None,
                exprErrors :+ WaccError(
                  expr.pos,
                  file,
                  TypeError(s"argument of $rhs", Set(PAIR_TYPE), ty)
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
            List(
              WaccError(expr.pos, file, NullExceptionError(s"argument of $rhs"))
            )
          )
        else {
          val (maybeExprType, exprErrors) = typeOfExpr(symbolTable, expr)
          maybeExprType match {
            case Some(PairType(_, sndType)) =>
              (Some(sndType.toType), exprErrors)
            case Some(ty) =>
              (
                None,
                exprErrors :+ WaccError(
                  expr.pos,
                  file,
                  TypeError(s"argument of $rhs", Set(PAIR_TYPE), ty)
                )
              )
            case None => (None, exprErrors)
          }
        }
      }
      case callExpr @ Call(id, args) => {
        val (maybeArgTypes, argErrorLists) =
          args.map(typeOfExpr(symbolTable, _)).unzip
        val argErrors = argErrorLists.flatten
        if (maybeArgTypes contains None) (None, argErrors)
        else {
          val argTypes = maybeArgTypes.map(_.get)
          (funcTable get id) match {
            case Some(FuncType(returnType, paramTypes)) => {
              if (argTypes.length != paramTypes.length)
                (
                  None,
                  argErrors :+ WaccError(
                    callExpr.pos,
                    file,
                    NumOfArgsError(
                      s"arguments of ${id.id}",
                      paramTypes.length,
                      argTypes.length
                    )
                  )
                )
              else if (
                argTypes.lazyZip(paramTypes).map(_ coercesTo _).forall(identity)
              ) (Some(returnType), argErrors)
              else {
                (
                  None,
                  argErrors ++ (argTypes zip paramTypes).collect {
                    case (argType, paramType)
                        if (!(argType coercesTo paramType)) =>
                      WaccError(
                        argType.pos,
                        file,
                        TypeError(s"argument of $id", Set(paramType), argType)
                      )
                  }
                )
              }
            }
            case None =>
              (
                None,
                argErrors :+ WaccError(
                  callExpr.pos,
                  file,
                  UndefinedFunctionError(id)
                )
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
      case expr: Expr => typeOfExpr(symbolTable, expr)
    }
  }

  def typeOfLhs(
      funcTable: Map[Ident, FuncType],
      symbolTable: Map[Ident, Type],
      lhs: AssignLhs
  )(implicit file: String): (Option[Type], List[WaccError]) = {
    // Every subtype of AssignLhs is also a subtype of AssignRhs. This method
    // exists anyway for easier extensibility if this were to change
    lhs match {
      case rhs: AssignRhs => typeOfRhs(funcTable, symbolTable, rhs)
    }
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])

}

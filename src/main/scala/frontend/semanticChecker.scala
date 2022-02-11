package frontend

import frontend.Errors._
import frontend.ast._

import scala.collection.mutable
import scala.io.Source
import java.io.File

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

  private def lineInfo(pos: (Int, Int))(implicit fileLines: Array[String]): LineInfo = pos match {
    case (line, col) => {
      LineInfo(
        fileLines(line - 1),
        if (line > 1) Seq(fileLines(line - 2)) else Nil,
        if (line < fileLines.length) Seq(fileLines(line)) else Nil,
        col
      )
    }
  }

  def validateProgram(program: WaccProgram, file: String): List[WaccError] = {
    implicit val fileImplicit: String = file
    implicit val fileLines: Array[String] = Source.fromFile(new File(file)).getLines().toArray
    val errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer.empty
    program match {
      case WaccProgram(funcs, stats) => {
        // generate function table
        val funcTableMut: mutable.Map[Ident, FuncType] = mutable.Map.empty
        for (f @ Func(ty, id, args, _) <- funcs) {
          if (funcTableMut contains id)
            errors += WaccError(f.pos, file, RedefinedFunctionError(id, lineInfo(f.pos)))
          funcTableMut += (id -> FuncType(
            ty,
            args.map { case Param(ty, _) => ty }
          ))
        }

        implicit val funcTable: Map[Ident, FuncType] = funcTableMut.toMap
        for (Func(ty, _, args, body) <- funcs) {
          val argsTable: Map[Ident, Type] = args.map { case Param(ty, id) =>
            id -> ty
          }.toMap
          errors ++= validateBlock(argsTable, body, Some(ty))
        }
        errors ++= validateBlock(Map.empty[Ident, Type], stats, None)
      }
    }
    errors.toList
  }

  def validateBlock(
      parentSymbols: Map[Ident, Type],
      stats: List[Stat],
      returnType: Option[Type]
  )(
    implicit file: String,
    fileLines: Array[String],
    funcTable: Map[Ident, FuncType]
  ): List[WaccError] = {
    val errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer.empty
    var localSymbols: Map[Ident, Type] = Map.empty[Ident, Type]
    implicit var childSymbols = parentSymbols ++ localSymbols
    for (stat <- stats) {
      stat match {
        case Skip() =>
        case Declare(ty, id, rhs) => {
          val (maybeRhs, rhsErrors) =
            typeOfRhs(rhs)
          errors ++= rhsErrors
          maybeRhs match {
            case Some(rhsType) =>
              if (!(rhsType coercesTo ty)) {
                errors += WaccError(
                  rhs.pos,
                  file,
                  TypeError("rhs of declaration statement", Set(ty), rhsType, lineInfo(rhs.pos))
                )
              }
            case _ =>
          }
          if (localSymbols contains id) {
            errors += WaccError(id.pos, file, RedefinedVariableError(id, lineInfo(id.pos)))
          } else {
            localSymbols += (id -> ty)
            childSymbols = parentSymbols ++ localSymbols
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
                errors += WaccError(
                  rhs.pos,
                  file,
                  TypeError(
                    "rhs of assignment statement",
                    Set(lhsType),
                    rhsType,
                    lineInfo(rhs.pos)
                  )
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
            case Some(IntType()) | Some(CharType()) =>
            case Some(ty) =>
              errors += WaccError(
                lhs.pos,
                file,
                TypeError(
                  "argument of read statement",
                  Set(INT_TYPE, CHAR_TYPE),
                  ty, lineInfo(lhs.pos)
                )
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
              errors += WaccError(
                expr.pos,
                file,
                TypeError(
                  "argument of free statement",
                  Set(PAIR_TYPE, ARRAY_TYPE),
                  ty, lineInfo(expr.pos)
                )
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
                errors += WaccError(
                  expr.pos,
                  file,
                  TypeError("argument of return statement", Set(ty), exprType, lineInfo(expr.pos))
                )
              }
            case (_, None) =>
              errors += WaccError(returnExpr.pos, file, MisplacedReturnError(lineInfo(returnExpr.pos)))
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
              errors += WaccError(
                expr.pos,
                file,
                TypeError("argument of exit statement", Set(INT_TYPE), ty, lineInfo(expr.pos))
              ) 
            case _ =>
          }
        }
        case Print(expr) =>
          errors ++= typeOfExpr(expr)._2
        case Println(expr) =>
          errors ++= typeOfExpr(expr)._2
        case If(expr, thenStats, elseStats) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(ty) =>
              errors += WaccError(
                expr.pos,
                file,
                TypeError("condition of if statement", Set(BOOL_TYPE), ty, lineInfo(expr.pos))
              )
            case _ =>
          }
          errors ++= validateBlock(
            parentSymbols ++ localSymbols.toMap,
            thenStats,
            returnType
          )
          errors ++= validateBlock(
            parentSymbols ++ localSymbols.toMap,
            elseStats,
            returnType
          )
        }
        case While(expr, doStats) => {
          val (maybeExpr, exprErrors) =
            typeOfExpr(expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(ty) =>
              errors += WaccError(
                expr.pos,
                file,
                TypeError("condition of while statement", Set(BOOL_TYPE), ty, lineInfo(expr.pos))
              )

            case _ =>
          }
          errors ++= validateBlock(
            parentSymbols ++ localSymbols.toMap,
            doStats,
            returnType
          )
        }
        case Scope(innerStats) =>
          errors ++= validateBlock(
            parentSymbols ++ localSymbols.toMap,
            innerStats,
            returnType
          )
      }
    }
    errors.toList
  }

  private def typeOfBinOp(
    argTypes: Set[Type],
    x: Expr,
    y: Expr,
    ret: Type,
    opName: String
  )(
    implicit symbolTable: Map[Ident, Type],
    file: String,
    fileLines: Array[String]
  ): (Option[Type], List[WaccError]) = {
    val (maybeTypes, errors) = typeOfExpr2(x, y)
    maybeTypes match {
      case Some((xType, yType)) => {
        if (!argTypes.exists(xType coercesTo _))
          (
            None,
            errors :+ WaccError(
              x.pos,
              file,
              TypeError(s"first argument of $opName", argTypes, xType, lineInfo(x.pos))
            )
          )
        else if (!argTypes.exists(yType coercesTo _))
          (
            None,
            errors :+ WaccError(
              y.pos,
              file,
              TypeError(s"second argument of $opName", argTypes, yType, lineInfo(y.pos))
            )
          )
        else if (!((xType coercesTo yType) || (yType coercesTo xType)))
          (
            None,
            errors :+ WaccError(
              x.pos,
              file,
              TypeError(s"arguments of $opName", Set(xType), yType, lineInfo(x.pos))
            )
          )
        else (Some(ret), errors)
      }
      case _ => (None, errors)
    }
  }

  private def typeOfUnOp(
    argType: Set[Type],
    x: Expr,
    ret: Type,
    opName: String
  )(implicit
    symbolTable: Map[Ident, Type],
    file: String,
    fileLines: Array[String]
  ): (Option[Type], List[WaccError]) = {
    val (maybeXType, xErrors) = typeOfExpr(x)
    maybeXType match {
      case Some(xType) => {
        if (!argType.exists(xType coercesTo _))
          (
            None,
            xErrors :+ WaccError(
              x.pos,
              file,
              TypeError(s"argument of $opName", argType, xType, lineInfo(x.pos))
            )
          )
        else (Some(ret), xErrors)
      }
      case _ => (None, xErrors)
    }
  }

  def typeOfExpr(expr: Expr)(implicit
    symbolTable: Map[Ident, Type], 
    file: String,
    fileLines: Array[String]
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
        (symbolTable get identExpr) match {
          case Some(ty) => (Some(ty), Nil)
          case None =>
            (
              None,
              List(
                WaccError(
                  identExpr.pos,
                  file,
                  UndefinedVariableError(identExpr, lineInfo(identExpr.pos))
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
          typeOfExpr2(expr, ArrayLiter(exprs)(arrayExpr.pos))
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
                  TypeError("elements of array", Set(a), b, lineInfo(arrayExpr.pos))
                )
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
              case Some(ArrayType(innerType)) => (Some(innerType), errors)
              case Some(ty) =>
                (
                  None,
                  errors :+ WaccError(
                    arrayElem.pos,
                    file,
                    TypeError("array", Set(ARRAY_TYPE), ty, lineInfo(arrayElem.pos))
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
                TypeError("array index", Set(INT_TYPE), ty, lineInfo(arrayElem.pos))
              )
            )
          case None => (None, indexErrors)
        }
      }
    }
  }

  def typeOfExpr2(x: Expr, y: Expr)(implicit
    symbolTable: Map[Ident, Type], 
    file: String,
    fileLines: Array[String]
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
    symbolTable: Map[Ident, Type],
    file: String,
    fileLines: Array[String]
  ): (Option[Type], List[WaccError]) = {
    rhs match {
      case rhs @ NewPair(fst, snd) => {
        val (maybeTypes, errors) = typeOfExpr2(fst, snd)
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
              WaccError(expr.pos, file, NullExceptionError(s"argument of $rhs", lineInfo(expr.pos)))
            )
          )
        else {
          val (maybeExprType, exprErrors) = typeOfExpr(expr)
          maybeExprType match {
            case Some(PairType(fstType, _)) =>
              (Some(fstType.toType), exprErrors)
            case Some(ty) =>
              (
                None,
                exprErrors :+ WaccError(
                  expr.pos,
                  file,
                  TypeError(s"argument of $rhs", Set(PAIR_TYPE), ty, lineInfo(expr.pos))
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
              WaccError(expr.pos, file, NullExceptionError(s"argument of $rhs", lineInfo(expr.pos)))
            )
          )
        else {
          val (maybeExprType, exprErrors) = typeOfExpr(expr)
          maybeExprType match {
            case Some(PairType(_, sndType)) =>
              (Some(sndType.toType), exprErrors)
            case Some(ty) =>
              (
                None,
                exprErrors :+ WaccError(
                  expr.pos,
                  file,
                  TypeError(s"argument of $rhs", Set(PAIR_TYPE), ty, lineInfo(expr.pos))
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
                  argErrors :+ WaccError(
                    callExpr.pos,
                    file,
                    NumOfArgsError(
                      s"arguments of ${id.id}",
                      paramTypes.length,
                      argTypes.length, lineInfo(callExpr.pos)
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
                        TypeError(s"argument of $id", Set(paramType), argType, lineInfo(argType.pos))
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
                  UndefinedFunctionError(id, lineInfo(callExpr.pos))
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
      case expr: Expr => typeOfExpr(expr)
    }
  }

  def typeOfLhs(
    lhs: AssignLhs
  )(implicit
    funcTable: Map[Ident, FuncType],
    symbolTable: Map[Ident, Type],
    file: String,
    fileLines: Array[String]
  ): (Option[Type], List[WaccError]) = {
    // Every subtype of AssignLhs is also a subtype of AssignRhs. This method
    // exists anyway for easier extensibility if this were to change
    lhs match {
      case rhs: AssignRhs => typeOfRhs(rhs)
    }
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])

}

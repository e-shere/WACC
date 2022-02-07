package frontend

import ast._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object semanticChecker {

  // used as the pos of a synthetic NodeWithPosition whose pos carries no meaning
  private val NO_POS = (-1, -1)

  private val ANY_TYPE = AnyType()(NO_POS)
  private val INT_TYPE = IntType()(NO_POS)
  private val BOOL_TYPE = BoolType()(NO_POS)
  private val CHAR_TYPE = CharType()(NO_POS)
  private val STRING_TYPE = StringType()(NO_POS)
  private val PAIR_TYPE = PairType(ANY_TYPE, ANY_TYPE)(NO_POS)
  private val ARRAY_TYPE = ArrayType(ANY_TYPE)(NO_POS)
  private val COMP_ARG_TYPES: Set[Type] = Set(INT_TYPE, CHAR_TYPE)
  private val EQ_ARG_TYPES: Set[Type] = Set(INT_TYPE, BOOL_TYPE, CHAR_TYPE, STRING_TYPE, PAIR_TYPE, ARRAY_TYPE)

  // TODO: should be returning some kind of optional try error?
  def validateProgram(program: WaccProgram): Unit = {
    program match {
      case WaccProgram(funcs, stats) => {
        // generate function table
        val funcTable: Map[Ident, FuncType] = funcs.map {
          case Func(ty, id, args, _) => {
            id->FuncType(ty, args.map{case Param(ty, _) => ty})
          }
        }.toMap
        for (Func(ty, _, args, body) <- funcs) {
          val argsTable: Map[Ident, Type] = args.map {
            case Param(ty, id) => id->ty
          }.toMap
          validateBlock(funcTable, argsTable, body, Some(ty))
        }
        validateBlock(funcTable, Map.empty[Ident, Type], stats, None)
      }
    }
  }

  def validateBlock(funcTable: Map[Ident, FuncType],
                    parentSymbols: Map[Ident, Type],
                    stats: List[Stat],
                    returnType: Option[Type]): List[SemanticError] = {
    val errors: mutable.ListBuffer[SemanticError] = mutable.ListBuffer.empty
    val localSymbols: mutable.Map[Ident, Type] = mutable.Map.empty[Ident, Type]
    for (stat <- stats) {
      stat match {
        case Skip() =>
        case Declare(ty, id, rhs) => {
          val (maybeRhs, rhsErrors) = validateRhs(funcTable, localSymbols.toMap ++ parentSymbols, rhs)
          errors ++= rhsErrors
          maybeRhs match {
            case Some(rhsType) => if (rhsType != ty) {
              errors += SemanticError("rhs of assignment doesn't match type")
            }
            case _ =>
          }
          if (localSymbols contains id) {
            errors += SemanticError("redeclaring identifier within same scope")
          } else {
            localSymbols += (id->ty)
          }
        }
        case Assign(lhs, rhs) => {
          val (maybeLhs, lhsErrors) = validateLhs(funcTable, localSymbols.toMap ++ parentSymbols, lhs)
          val (maybeRhs, rhsErrors) = validateRhs(funcTable, localSymbols.toMap ++ parentSymbols, rhs)
          errors ++= lhsErrors
          errors ++= rhsErrors
          (maybeLhs, maybeRhs) match {
            case (Some(lhsType), Some(rhsType)) => if (lhsType != rhsType) {
              errors += SemanticError("rhs of assignment doesn't match type")
            }
            case _ =>
          }
        }
        case Read(lhs) => errors ++= validateLhs(funcTable, localSymbols.toMap ++ parentSymbols, lhs)._2
        case Free(expr) => {
          val (maybeExpr, exprErrors) = validateExpr(localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(PairType(_, _)) =>
            case Some(ArrayType(_)) =>
            case Some(_) => errors += SemanticError("Only a pair or array can be freed")
            case _ =>
          }
        }
        case Return(expr) => {
          val (maybeExpr, exprErrors) = validateExpr(localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          (maybeExpr, returnType) match {
            case (Some(exprType), Some(ty)) => if (exprType != ty) {
              errors += SemanticError("return type must match return type of function")
            }
            case (_, None) => errors += SemanticError("can't return outside a function")
            case (None, Some(_)) =>
          }
        }
        case Exit(expr) => {
          val (maybeExpr, exprErrors) = validateExpr(localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(IntType()) =>
            case Some(_) => errors += SemanticError("Exit status must be an integer")
            case _ =>
          }
        }
        case Print(expr) => errors ++= validateExpr(localSymbols.toMap ++ parentSymbols, expr)._2
        case Println(expr) => errors ++= validateExpr(localSymbols.toMap ++ parentSymbols, expr)._2
        case If(expr, thenStats, elseStats) => {
          val (maybeExpr, exprErrors) = validateExpr(localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(_) => errors += SemanticError("If condition must be a bool")
            case _ =>
          }
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, thenStats, returnType)
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, elseStats, returnType)
        }
        case While(expr, doStats) => {
          val (maybeExpr, exprErrors) = validateExpr(localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) =>
            case Some(_) => errors += SemanticError("While condition must be a bool")

            case _ =>
          }
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, doStats, returnType)
        }
        case Scope(innerStats) =>
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, innerStats, returnType)
      }
    }
    errors.toList
  }

  private def validateBinaryOperator(symbolTable: Map[Ident, Type], argType: Set[Type], x: Expr, y: Expr, ret: Type, opName: String): (Option[Type], List[SemanticError]) = {
    val (maybeXType, xErrors) = validateExpr(symbolTable, x)
    val (maybeYType, yErrors) = validateExpr(symbolTable, y)
    (maybeXType, maybeYType) match {
      case (Some(xType), Some(yType)) => {
        if (!(argType contains xType)) (None, xErrors ++ yErrors :+ SemanticError(s"The first argument to $opName should be one of ${argType.map(_.toTypeName).mkString(", ")}"))
        else if (!(argType contains yType)) (None, xErrors ++ yErrors :+ SemanticError(s"The second argument to $opName should be one of ${argType.map(_.toTypeName).mkString(", ")}"))
        else if (xType != yType) (None, xErrors ++ yErrors :+ SemanticError(s"The two arguments to $opName must have the same type"))
        else (Some(ret), xErrors ++ yErrors)
      }
      case _ => (None, xErrors ++ yErrors)
    }
  }

  private def validateUnaryOperator(symbolTable: Map[Ident, Type], argType: Set[Type], x: Expr, ret: Type, opName: String): (Option[Type], List[SemanticError]) = {
    val (maybeXType, xErrors) = validateExpr(symbolTable, x)
    maybeXType match {
      case Some(xType) => {
        if (!(argType contains xType)) (None, xErrors :+ SemanticError(s"The argument to $opName should be one of ${argType.map(_.toTypeName).mkString(", ")}"))
        else (Some(ret), xErrors)
      }
      case _ => (None, xErrors)
    }
  }


  def validateExpr(symbolTable: Map[Ident, Type], expr: Expr): (Option[Type], List[SemanticError]) = {
    expr match {
      case orStat@Or(x, y) => validateBinaryOperator(symbolTable, Set(BOOL_TYPE), x, y, BoolType()(orStat.pos), "||")
      case andStat@And(x, y) => validateBinaryOperator(symbolTable, Set(BOOL_TYPE), x, y, BoolType()(andStat.pos), "&&")
      case eqStat@Eq(x, y) => validateBinaryOperator(symbolTable, EQ_ARG_TYPES, x, y, BoolType()(eqStat.pos), "==")
      case neqStat@Neq(x, y) => validateBinaryOperator(symbolTable, EQ_ARG_TYPES, x, y, BoolType()(neqStat.pos), "!=")
      case leqStat@Leq(x, y) => validateBinaryOperator(symbolTable, COMP_ARG_TYPES, x, y, BoolType()(leqStat.pos), "<=")
      case ltStat@Lt(x, y) => validateBinaryOperator(symbolTable, COMP_ARG_TYPES, x, y, BoolType()(ltStat.pos), "<")
      case geqStat@Geq(x, y) => validateBinaryOperator(symbolTable, COMP_ARG_TYPES, x, y, BoolType()(geqStat.pos), ">=")
      case gtStat@Gt(x, y) => validateBinaryOperator(symbolTable, COMP_ARG_TYPES, x, y, BoolType()(gtStat.pos), ">")
      case addStat@Add(x, y) => validateBinaryOperator(symbolTable, Set(INT_TYPE), x, y, IntType()(addStat.pos), "+")
      case subStat@Sub(x, y) => validateBinaryOperator(symbolTable, Set(INT_TYPE), x, y, IntType()(subStat.pos), "-")
      case mulStat@Mul(x, y) => validateBinaryOperator(symbolTable, Set(INT_TYPE), x, y, IntType()(mulStat.pos), "*")
      case divStat@Div(x, y) => validateBinaryOperator(symbolTable, Set(INT_TYPE), x, y, IntType()(divStat.pos), "/")
      case modStat@Mod(x, y) => validateBinaryOperator(symbolTable, Set(INT_TYPE), x, y, IntType()(modStat.pos), "%")
      case notStat@Not(x) => validateUnaryOperator(symbolTable, Set(BOOL_TYPE), x, BoolType()(notStat.pos), "!")
      case negStat@Neg(x) => validateUnaryOperator(symbolTable, Set(INT_TYPE), x, IntType()(negStat.pos), "-")
      case lenStat@Len(x) => validateUnaryOperator(symbolTable, Set(ARRAY_TYPE), x, IntType()(lenStat.pos), "len")
      case ordStat@Ord(x) => validateUnaryOperator(symbolTable, Set(CHAR_TYPE), x, IntType()(ordStat.pos), "ord")
      case chrStat@Chr(x) => validateUnaryOperator(symbolTable, Set(INT_TYPE), x, CharType()(chrStat.pos), "chr")
      case nullExpr@Null() => (Some(PairType(AnyType()(nullExpr.pos), AnyType()(nullExpr.pos))(nullExpr.pos)), Nil)
      case Paren(expr) => validateExpr(symbolTable, expr)
      case identExpr: Ident => (symbolTable get identExpr) match {
        case Some(ty) => (Some(ty), Nil)
        case None => (None, List(SemanticError("undefined identifier")))
      }
      case intExpr: IntLiter => (Some(IntType()(intExpr.pos)), Nil)
      case strExpr: StrLiter => (Some(StringType()(strExpr.pos)), Nil)
      case boolExpr: BoolLiter => (Some(BoolType()(boolExpr.pos)), Nil)
      case charExpr: CharLiter => (Some(CharType()(charExpr.pos)), Nil)
      case arrayExpr@ArrayLiter(Nil) => (Some(ArrayType(AnyType()(arrayExpr.pos))(arrayExpr.pos)), Nil)
      case arrayExpr@ArrayLiter(expr :: exprs) => {
        val (maybeHeadType, headErrors) = validateExpr(symbolTable, expr)
        val (maybeTailType, tailErrors) = validateExpr(symbolTable, ArrayLiter(exprs)(arrayExpr.pos))
        (maybeHeadType, maybeTailType) match {
          case (Some(a), Some(b)) => {
            if (a == b) (Some(a), headErrors ++ tailErrors)
            else (None, headErrors ++ tailErrors :+ SemanticError("All elements of an array must have the same type"))
          } 
          case (_, _) => (None, headErrors ++ tailErrors)
        }
      }
      case ArrayElem(id, index: Expr) => {
        val (maybeIndexType, indexErrors) = validateExpr(symbolTable, index)
        maybeIndexType match {
          case Some(IntType()) => {
            val (maybeInnerType, innerErrors) = validateExpr(symbolTable, id)
            (maybeInnerType, innerErrors ++ indexErrors)
          }
          case Some(_) => (None, indexErrors :+ SemanticError("Array index must be an int"))
          case None => (None, indexErrors)
        }
      }
    }
  }


    // get type of rhs and checking for semantic errors within rhs
  def validateRhs(funcTable: Map[Ident, FuncType],
                  symbolTable: Map[Ident, Type], rhs: AssignRhs): (Option[Type], List[SemanticError]) = {
    (None, Nil)
  }

  // check if is in local symbols
  def validateLhs(funcTable: Map[Ident, FuncType],
                  symbolTable: Map[Ident, Type], lhs: AssignLhs): (Option[Type], List[SemanticError]) = {
    (None, Nil)
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])
  case class SemanticError(msg: String)

}

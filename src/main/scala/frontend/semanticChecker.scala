package frontend

import ast._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object semanticChecker {

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

  //
  private def validateBinaryOperators[A <: Type](symbolTable: Map[Ident, Type], x: Expr, y: Expr, pos: (Int, Int), opInfo: (String, String)): (Option[Type], List[SemanticError]) = {
    var maybeTy: Option[Type] = None
    val errors: mutable.ListBuffer[SemanticError] = mutable.ListBuffer.empty

    val (maybeXType, xErrors) = validateExpr(symbolTable, x)
    val (maybeYType, yErrors) = validateExpr(symbolTable, y)
    errors ++= xErrors
    errors ++= yErrors
    (maybeXType, maybeYType) match {
      case (Some(BoolType()), Some(BoolType())) => { maybeTy = Some(BoolType()(pos))}
      case (Some(_), Some(BoolType())) => errors += SemanticError(s"The first argument to ${opInfo._1} should be a ${opInfo._2}")
      case (Some(BoolType()), Some(_)) => errors += SemanticError(s"The second argument to ${opInfo._1} should be a ${opInfo._2}")
      case (Some(_), Some(_)) => errors += SemanticError(s"Both arguments to ${opInfo._1} must be a ${opInfo._2}")
      case (_, _)  =>
    }

    (maybeTy, errors.toList)
  }

  def validateExpr(symbolTable: Map[Ident, Type], expr: Expr): (Option[Type], List[SemanticError]) = {
    expr match {
      case orStat@Or(x, y) => validateBinaryOperators(symbolTable, x, y, orStat.pos, ("||", "bool"))
      case andStat@And(x, y) => validateBinaryOperators(symbolTable, x, y, andStat.pos, ("&&", "bool"))
      case eqStat@Eq(x, y) => {
        var maybeTy: Option[Type] = None
        val errors: mutable.ListBuffer[SemanticError] = mutable.ListBuffer.empty

        val (maybeXType, xErrors) = validateExpr(symbolTable, x)
        val (maybeYType, yErrors) = validateExpr(symbolTable, y)
        errors ++= xErrors
        errors ++= yErrors
        (maybeXType, maybeYType) match {
            // TODO: dependent on overloading pair equality
          case (Some(a), Some(b)) => if (a == b) {
            maybeTy = Some(BoolType()(eqStat.pos))
          } else {
            errors += SemanticError("Can't compare two different types")
          }
          case (_, _) =>
        }
        (maybeTy, errors.toList)
      }
//      case neqStat@Neq(x, y) =>
//      case leqStat@Leq(x, y) =>
//      case ltStat@Lt(x, y) =>
//      case geqStat@Geq(x, y) =>
//      case gtStat@Gt(x, y) =>
      case addStat@Add(x, y) => validateBinaryOperators(symbolTable, x, y, addStat.pos, ("+", "int"))
      case subStat@Sub(x, y) => validateBinaryOperators(symbolTable, x, y, subStat.pos, ("-", "int"))
      case mulStat@Mul(x, y) => validateBinaryOperators(symbolTable, x, y, mulStat.pos, ("*", "int"))
      case divStat@Div(x, y) => validateBinaryOperators(symbolTable, x, y, divStat.pos, ("/", "int"))
      case modStat@Mod(x, y) => validateBinaryOperators(symbolTable, x, y, modStat.pos, ("%", "int"))
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

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
        for (Func(_, _, args, body) <- funcs) {
          val argsTable: Map[Ident, Type] = args.map {
            case Param(ty, id) => id->ty
          }.toMap
          validateBlock(funcTable, argsTable, body)

        }
        validateBlock(funcTable, Map.empty[Ident, Type], stats)
      }
    }
  }

  def validateBlock(funcTable: Map[Ident, FuncType],
                    parentSymbols: Map[Ident, Type],
                    stats: List[Stat]): List[SemanticError] = {
    val errors: mutable.ListBuffer[SemanticError] = mutable.ListBuffer.empty
    val localSymbols: mutable.Map[Ident, Type] = mutable.Map.empty[Ident, Type]
    for (stat <- stats) {
      stat match {
        case Skip() => {}
        case Declare(ty, id, rhs) => {
          val (maybeRhs, rhsErrors) = validateRhs(funcTable, localSymbols.toMap ++ parentSymbols, rhs)
          errors ++= rhsErrors
          maybeRhs match {
            case Some(rhsType) => if (rhsType != ty) {
              errors += SemanticError("rhs of assignment doesn't match type")
            }
            case _ => {}
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
            case _ => {}
          }
        }
        case Read(lhs) => {
          errors ++= validateLhs(funcTable, localSymbols.toMap ++ parentSymbols, lhs)._2
        }
        case Free(expr) => {
          val (maybeExpr, exprErrors) = validateExpr(funcTable, localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(PairType(_, _)) => {}
            case Some(ArrayType(_)) => {}
            case Some(_) => errors += SemanticError("Only a pair or array can be freed")
            case _ => {}
          }
        }
        case Return(expr) => {
          // todo
        }
        case Exit(expr) => {
          val (maybeExpr, exprErrors) = validateExpr(funcTable, localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(IntType()) => {
              // todo check on OS what range the number is
            }
            case Some(_) => errors += SemanticError("Exit status must be an integer")
            case _ => {}
          }
        }
        case Print(expr) => {
          errors ++= validateExpr(funcTable, localSymbols.toMap ++ parentSymbols, expr)._2
        }
        case Println(expr) => {
          errors ++= validateExpr(funcTable, localSymbols.toMap ++ parentSymbols, expr)._2
        }
        case If(expr, thenStats, elseStats) => {
          val (maybeExpr, exprErrors) = validateExpr(funcTable, localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) => {}
            case Some(_) => {
              errors += SemanticError("If condition must be a bool")
            }
            case _ => {}
          }
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, thenStats)
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, elseStats)
        }
        case While(expr, doStats) => {
          val (maybeExpr, exprErrors) = validateExpr(funcTable, localSymbols.toMap ++ parentSymbols, expr)
          errors ++= exprErrors
          maybeExpr match {
            case Some(BoolType()) => {}
            case Some(_) => {
              errors += SemanticError("While condition must be a bool")
            }
            case _ => {}
          }
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, doStats)
        }
        case Scope(innerStats) => {
          errors ++= validateBlock(funcTable, localSymbols.toMap ++ parentSymbols, innerStats)
        }
      }
    }
    errors.toList

  }

  def validateExpr(funcTable: Map[Ident, FuncType],
                   symbolTable: Map[Ident, Type], expr: Expr): (Option[Type], List[SemanticError]) = {
    (None, Nil)
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

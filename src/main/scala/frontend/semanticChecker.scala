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
        for (Func(ty, id, args, body) <- funcs) {
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
//          //        validateLhs(funcTable, localSymbols ++ parentSymbols, lhs)++
//          //          validateRhs(funcTable, localSymbols ++ parentSymbols, rhs)
//
//
//          //TODO: CHECK
//          validateBlock(funcTable, parentSymbols, localSymbols, tailStats)
//        }
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
        //      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)

      }
    }
    errors.toList

  }

  // get type of rhs and checking for semantic errors within rhs
  def validateRhs(funcTable: Map[Ident, FuncType],
                  symbolTable: Map[Ident, Type], rhs: AssignRhs): (Option[Type], List[SemanticError]) = {
    (None, Nil)
  }

  def validateLhs(funcTable: Map[Ident, FuncType],
                  symbolTable: Map[Ident, Type], lhs: AssignLhs): (Option[Type], List[SemanticError]) = {
    (None, Nil)
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])
  case class SemanticError(msg: String)

}

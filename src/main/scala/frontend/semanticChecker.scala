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
          validateBlock(funcTable, argsTable, Map.empty[Ident,Type], body)

        }
        validateBlock(funcTable, Map.empty[Ident, Type], Map.empty[Ident,Type], stats)
      }
    }
  }

  def validateBlock(funcTable: Map[Ident, FuncType],
                    parentSymbols: Map[Ident, Type],
                    localSymbols : Map[Ident, Type],
                    stats: List[Stat]): List[SemanticError] = {
    stats match {
      case Nil => { Nil }
      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, localSymbols, tailStats)
      case Declare(ty, id, rhs)::tailStats => {
        validateRhs(funcTable, localSymbols ++ parentSymbols, rhs, ty)++(
        if (localSymbols.contains(id)) {
          SemanticError("")::validateBlock(funcTable, parentSymbols, localSymbols, tailStats)
        } else {
          validateBlock(funcTable, parentSymbols, localSymbols+(id->ty), tailStats)
        })
      }
//      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
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

  // get type of rhs and checking for semantic errors within rhs
  def validateRhs(value: Map[Ident, FuncType],
                  value1: Map[Ident, Type], rhs: AssignRhs, idealType: Type): List[SemanticError] = {
    Nil
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])
  case class SemanticError(msg: String)

}

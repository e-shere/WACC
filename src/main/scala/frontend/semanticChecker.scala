package frontend

import ast._

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
                    stats: List[Stat]): Unit = {
    val localSymbols: Map[Ident, Type] = Map.empty;
    stats match {
      case Nil => {}
      case Skip()::tailStats => validateBlock(funcTable, parentSymbols, tailStats)
      case Declare(ty, id, rhs)::tailStats => {
        getTypeRhs(funcTable, localSymbols ++ parentSymbols, rhs) match {
          case Success(ty) => {

          }
          case Success(_) => {}
          case Failure(e) => {}
        }

        validateBlock(funcTable, parentSymbols, tailStats)
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
  def getTypeRhs(value: Map[Ident, FuncType],
                  value1: Map[Ident, Type], rhs: AssignRhs): Try[Type] = {
    Success(IntType()(0,0))
  }

  case class FuncType(returnType: Type, paramTypes: List[Type])

}

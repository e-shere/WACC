package backend

import frontend.ast._
import frontend.symbols._
import intermediateRep._

object ir {
  
  def representProgram(program: WaccProgram, typeTree: TypeTree): Map[String, (List[IRNode], SymbolTable)] = program match {
    case WaccProgram(funcs, stats) => {
      (funcs :+ Func(AnyType()(NO_POS), Ident("main")(NO_POS), Nil, stats)(NO_POS))
        .map { 
          case f@Func(_, Ident(id), _, _ ) => id -> representFunc(f, typeTree)
        }.toMap
    }
  }

  def representFunc(func: Func, typeTree: TypeTree): (List[IRNode], SymbolTable) = func match {
    case f@Func(_, _, args, body) => {
      var nodes: List[IRNode] = Nil
      var symbols: SymbolTable = Map.empty
      nodes :+= StartFunc()
      args.foreach {
        case Param(ty, Ident(arg)) => {
          nodes :+= LoadArg(arg)
          symbols += ((f.pos, arg) -> SymbolEntry(ty, None, None))
        }
      }
      val (bodyNodes, bodySymbols) = representBlock(body, symbols)
      nodes ++= bodyNodes
      symbols = bodySymbols
      nodes :+= EndFunc()
      (nodes, symbols)
    }
  }

  def representBlock(stats: List[Stat], symbols: SymbolTable): (List[IRNode], SymbolTable) = stats match {
    case Nil => (Nil, symbols)
    case stat :: tail => {
      var nodes: List[IRNode] = Nil

      val (tailNodes, tailSymbols) = representBlock(tail, symbols)
      (nodes ++ tailNodes, tailSymbols)
    }
  }
}

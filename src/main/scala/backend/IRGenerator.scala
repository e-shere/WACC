package backend

import frontend.ast._
import backend.intermediateRep._
import java.io._

object IRGenerator {
  type IRFunc = List[IRNode]

  def generateIR(ast: WaccProgram): List[IRFunc] = {
    List(DIRECTIVE("text\n"), DIRECTIVE("global main")) +:
    ast.funcs.map(f => generateFunc(f.id.id, f.body)) :+ generateFunc("main", ast.stats)
  }

  def generateFunc(name: String, body: List[Stat]): IRFunc = {
    LABEL(name) +:
    START_FUNC() +:
    generateBlock(body) :+
    RETURN(0) :+
    EXIT_FUNC() :+
    DIRECTIVE("ltorg")
  }

  def generateBlock(stats: List[Stat]): List[IRNode] = {
    stats match {
        // TODO: generate assembly needed to evaluate expr, then insert something referring to that
        // into the return IR node- perhaps an intermediate value with register in the symbol table
      case Nil => List.empty
      case stat :: tail => generateStat(stat) ++: generateBlock(tail)
    }
  }

  def generateStat(stat: Stat): Option[IRNode] = {
    stat match {
      case Skip() => None
      case Declare(ty, id, rhs) => ???
      case Assign(lhs, rhs) => ???
      case Read(lhs) => ???
      case Free(expr) => ???
      case Return(_) => Some(RETURN(0))
      case Exit(_) => Some(RETURN(0))
      case Print(expr) => ???
      case Println(expr) => ???
      case If(expr, thenStats, elseStats) => ???
      case While(expr, doStats) => ???
    }
  }

  def generateCode(ast: WaccProgram, outputPath: String): Unit = {
    val rep = generateIR(ast)

    val pw = new PrintWriter(new File(outputPath + ".s"))
    rep.foreach(f => pw.write(f.mkString("\n") + "\n"))
    pw.close()

  }

}

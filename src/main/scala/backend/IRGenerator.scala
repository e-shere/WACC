package backend

import frontend.ast._
import backend.intermediateRep._
import java.io._

object IRGenerator {
  type IRFunc = List[IRNode]

  def generateIR(ast: WaccProgram): List[IRFunc] = {
    ast.funcs.map(f => generateFunc(f.id.id, f.body)) :+ generateFunc("main", ast.stats)
  }

  def generateFunc(name: String, body: List[Stat]): IRFunc = {
    LABEL(name) +:
    START_FUNC() +:
    generateBlock(body) :+
    RETURN(0) :+
    EXIT_FUNC()
  }

  def generateBlock(stats: List[Stat]): List[IRNode] = {
    stats match {
        // TODO: generate assembly needed to evaluate expr, then insert something referring to that
        // into the return IR node- perhaps an intermediate value with register in the symbol table
      case Nil => List.empty
      case Skip() :: tail => generateBlock(tail)
      case stat :: tail => generateStat(stat) +: generateBlock(tail)
    }
  }

  def generateStat(stat: Stat): IRNode = {
    stat match {
      case Return(_) => RETURN(0)
    }
  }

  def generateCode(ast: WaccProgram, outputPath: String): Unit = {
    val rep = generateIR(ast)

    val pw = new PrintWriter(new File(outputPath + ".s"))
    pw.write(".text\n\n.global main\n")
    rep.foreach(f => pw.write(f.mkString("\n") + "\n"))
    pw.close()

  }

}

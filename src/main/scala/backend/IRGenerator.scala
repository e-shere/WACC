package backend

import frontend.ast._
import backend.intermediateRep._
import java.io._

object IRGenerator {

  def generateIR(ast: WaccProgram): List[FUNCTION] = {
    ast.funcs.map(f => FUNCTION(f.id.id, generateBody(f.body))) :+ FUNCTION("main", generateBody(ast.stats) :+ RETURN())
  }

  def generateBody(stats: List[Stat]): List[IRNode] = {
    stats match {
        // TODO: generate assembly needed to evaluate expr, then insert something referring to that
        // into the return IR node- perhaps an intermediate value with register in the symbol table
      case List(Return(_)) => List(RETURN())
      case _ :: t => generateBody(t)
      case Nil => List.empty // WILL NEVER REACH HERE!
    }
  }

  def generateCode(ast: WaccProgram, outputPath: String): Unit = {
    val rep = generateIR(ast)

    val pw = new PrintWriter(new File(outputPath + ".s"))
    pw.write(".text\n\n.global main\n")

    pw.write(rep.mkString("\n") + "\n")
    pw.close

  }

}

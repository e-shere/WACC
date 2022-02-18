package backend

import frontend.ast._
import backend.intermediateRep._
import java.io._

object IRGenerator {

  def generateIR(ast: WaccProgram): List[function] = {
    ast.funcs.map(f => function(f.id.id, generateBody(f.body))) :+ function("main", generateBody(ast.stats))
  }

  def generateBody(stats: List[Stat]): List[IRNode] = {
    Nil
  }

  def generateCode(ast: WaccProgram, outputPath: String): Unit = {
    val rep = generateIR(ast)

    val pw = new PrintWriter(new File(outputPath + ".s"))
    pw.write(".text\n\n.global main\n")

    pw.write(rep.mkString("\n") + "\n")
    pw.close

  }

}

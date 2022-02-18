package backend

import frontend.ast._
import backend.intermediateRep._
import java.io._

object IRGenerator {

  def generateIR(ast: WaccProgram): List[IRNode] = {
    Nil
  }

  def generateCode(ast: WaccProgram, outputPath: String): Unit = {
    val rep = generateIR(ast)

    val pw = new PrintWriter(new File(outputPath + ".s"))
    pw.write(".text\n\n.global main\nmain:")
    pw.write(rep.mkString("\n"))
    pw.close

  }

}

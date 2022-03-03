package backend

import backend.generator.genProgram
import frontend.semanticChecker
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, matchPattern}
import parsley.{Failure, Success}

import java.io.{File, PrintWriter}

class MatchOutputTests extends AnyFlatSpec {
  import frontend.parser._

  def makeIR(waccPath: String, assemblyPath: String): Unit = {
    val maybeAst = parse(new File(waccPath))
    maybeAst should matchPattern { case Success(_) => }
    val ast = maybeAst.get
    semanticChecker.validateProgram(ast, waccPath) should matchPattern { case Nil => }
    val pw = new PrintWriter(new File(assemblyPath))
    pw.write(genProgram(ast).mkString("\n") + "\n")
    pw.close()
  }

  behavior of "one line wacc programs"

  "begin skip end program" should "only skip" in {
//    val waccPath =  "src/examples/valid/basic/skip/skip.wacc"
//    val assemblyPath = waccPath.split("\\.").head + ".s"
//    makeIR(waccPath, assemblyPath)
//    scala.io.Source.fromFile(assemblyPath).mkString shouldBe ".text\n\n.global main\nmain:\nPUSH {lr}\nLDR r0, =0\nPOP {pc}\n.ltorg\n"
  }

}

//package backend
//
//import frontend.semanticChecker
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, matchPattern}
//import parsley.{Failure, Success}
//
//import java.io.File
//
//class MatchOutputTests extends AnyFlatSpec {
//  import frontend.parser._
//
//  def makeIR(waccPath: String, assemblyPath: String): Unit = {
//    val maybeAst = parse(new File(waccPath))
//    maybeAst should matchPattern { case Success(_) => }
//    val ast = maybeAst.get
//    semanticChecker.validateProgram(ast, waccPath) should matchPattern { case Nil => }
//    backend.IRGenerator.generateCode(ast, assemblyPath)
//  }
//
//  behavior of "one line wacc programs"
//
//  "begin skip end program" should "only skip" in {
//    val waccPath =  "src/examples/valid/basic/skip/skip.wacc"
//    val assemblyPath = waccPath.split("\\.").head
//    makeIR(waccPath, assemblyPath)
//    scala.io.Source.fromFile(assemblyPath+ ".s").mkString shouldBe ".text\n\n.global main\nmain:\nPUSH {lr}\nLDR r0, =0\nPOP {pc}\n.ltorg\n"
//  }
//
//}

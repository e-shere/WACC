package backend

import backend.generator.genProgram
import frontend.parser._
import frontend.semanticChecker

import java.io.{File, PrintWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Success

import scala.language.postfixOps
import sys.process._

class AssemblyCompileTests extends AnyFlatSpec {
  behavior of "all valid programs"

  def getListOfFilesRecursively(dir: String): Array[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val result: List[String] = d.listFiles
        .filter(_.isDirectory)
        .toList
        .flatMap(x => getListOfFilesRecursively(x.toString))
      d.listFiles.filter(_.isFile).map(_.toString()).concat(result)
    } else {
      Array[String]()
    }
  }

  def allCompile(srcPath: String) = {
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      // parse wacc program
      val maybeAst = parse(new File(path))
      maybeAst should matchPattern { case Success(_) => }
      val ast = maybeAst.get
      semanticChecker.validateProgram(ast, path) should matchPattern { case Nil => }
      // generate assembly file
      val assemblyPath = path.split("\\.").head + ".s"
      val pw = new PrintWriter(new File(assemblyPath))
      pw.write(genProgram(ast).mkString("\n") + "\n")
      pw.close()
      // compile assembly
      val compileResult = s"arm-linux-gnueabi-gcc -o ${path.split("\\.").head} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s ${assemblyPath}"!
    }
  }


  "All valid advanced programs" should "compile" in pending /* {
    allCompile("src/examples/valid/advanced")
  } */

  "All valid array programs" should "compile" in pending /* {
    allCompile("src/examples/valid/array")
  } */

  "All valid basic programs" should "compile" in  {
    allCompile("src/examples/valid/basic")
  }

  "All valid expressions programs" should "compile" in pending /* {
    allCompile("src/examples/valid/expressions")
  } */

  "All valid function programs" should "compile" in pending /* {
    allCompile("src/examples/valid/function")
  } */

  "All valid if programs" should "compile" in pending /* {
    allCompile("src/examples/valid/if")
  } */

  "All valid IO programs" should "compile" in pending /* {
    allCompile("src/examples/valid/IO")
  } */

  "All valid pairs programs" should "compile" in pending /* {
    allCompile("src/examples/valid/pairs")
  } */

  "All valid runtimeErr programs" should "compile" in pending /* {
    allCompile("src/examples/valid/runtimeErr")
  } */

  "All valid scope programs" should "compile" in pending /* {
    allCompile("src/examples/valid/scope")
  } */

  "All valid sequence programs" should "compile" in pending /* {
    allCompile("src/examples/valid/sequence")
  } */

  "All valid variables programs" should "compile" in pending /* {
    allCompile("src/examples/valid/variables")
  } */

  "All valid while programs" should "compile" in pending /* {
    allCompile("src/examples/valid/while")
  } */

}

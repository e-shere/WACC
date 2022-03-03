package backend

import backend.generator.genProgram
import frontend.parser._
import frontend.semanticChecker
import java.io.{File, PrintWriter}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Success

import scala.language.postfixOps
import scala.reflect.io.File
import sys.process._

class AssemblyCompileTests extends AnyFlatSpec {
  behavior of "all valid programs"

  def getListOfFilesRecursively(dir: String): Array[String] = {
    val d = new java.io.File(dir)
    if (d.exists && d.isDirectory) {
      val result: List[String] = d.listFiles
        .filter(_.isDirectory)
        .toList
        .flatMap(x => getListOfFilesRecursively(x.toString))
      d.listFiles.filter(_.isFile).filter(_.toString.endsWith(".wacc")).map(_.toString()).concat(result)
    } else {
      Array[String]()
    }
  }

  def getOutput(path: String): Array[String] = {
    scala.io.Source.fromFile(path).mkString
      .split('\n')
      .dropWhile(x => !x.startsWith("# Output"))
      .takeWhile(x => x.startsWith("#"))
      .drop(1)
      .map(x => x.substring(2))
  }

  def matchLine(expectedLine: String, actualLine: String): Boolean = {
    actualLine == expectedLine
//    (expectedLine, actualLine) match {
//      case ('#' +: restX, restY) => matchLine(restX.)
//    }
  }

  def matchOutput(expected: String, actual: String): Boolean = {
    var matches = true
    val expectedArr = getOutput(expected)
    if (expectedArr(0).equals("#empty#")) return actual.isEmpty
    val actualArr = actual.split('\n')
    if (!(expectedArr.length == actualArr.length)) return false
    for ((expectedLine, actualLine) <- expectedArr zip actualArr) {
      matches = matchLine(expectedLine, actualLine)
    }
    matches
  }

  def allCompile(srcPath: String) = {
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      // parse wacc program
      val maybeAst = parse(new java.io.File(path))
      maybeAst should matchPattern { case Success(_) => }
      val ast = maybeAst.get
      semanticChecker.validateProgram(ast, path) should matchPattern { case Nil => }
      // generate assembly file
      val assemblyPath = path.split("\\.").head + ".s"
      val pw = new PrintWriter(new java.io.File(assemblyPath))
      pw.write(genProgram(ast).mkString("\n") + "\n")
      pw.close()
      // compile assembly
      val cmd = s"arm-linux-gnueabi-gcc -o ${path.split("\\.").head} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s ${assemblyPath}"
      cmd.! shouldBe 0
      val out = s"qemu-arm -L /usr/arm-linux-gnueabi/ ${path.split("\\.").head}"
      matchOutput(path, out.!!) shouldBe true
    }
  }


  "All valid advanced programs" should "compile" in {
    allCompile("src/examples/valid/advanced")
  }

  "All valid array programs" should "compile" in {
    allCompile("src/examples/valid/array")
  }

  "All valid basic programs" should "compile" in pending /*  {
    allCompile("src/examples/valid/basic")
  } */

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

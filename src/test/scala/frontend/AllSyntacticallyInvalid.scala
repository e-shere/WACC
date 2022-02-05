package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.io.File

class AllSyntacticallyInvalid extends AnyFlatSpec {
  import parser._
  import scala.io.Source
  import parsley.{Success, Failure}

  behavior of "all syntactically invalid programs"

  def getListOfFilesRecursively(dir: String): Array[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val result : List[String] = d.listFiles.filter(_.isDirectory)
        .toList.flatMap(x => getListOfFilesRecursively(x.toString()))
      d.listFiles.filter(_.isFile).map(_.toString()).concat(result)
    } else {
      Array[String]()
    }
  }

  "All invalid array programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/array"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

  "All invalid basic programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/basic"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

  "All invalid expressions programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/expressions"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

  // TODO: Something is succeeding here?
  "All invalid function programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/function"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

  "All invalid if programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/if"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

  "All invalid pairs programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/pairs"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }
  "All invalid print programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/print"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

    "All invalid sequence programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/sequence"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

  // TODO: I think it's an int overflow error isn't caught
  "All invalid variables programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/variables"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

  "All invalid while programs" should "return failure" in {
    val srcPath = "src/examples/invalid/syntaxErr/while"
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      parse(source) should matchPattern { case Failure(_) => }
    }
  }

}
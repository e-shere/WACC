package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.io.File

class AllSemanticallyInvalid extends AnyFlatSpec {
  import parser._
  import parsley.Success

  import scala.io.Source

  behavior of "all semantically invalid programs"

  def getListOfFilesRecursively(dir: String): Array[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val result: List[String] = d.listFiles
        .filter(_.isDirectory)
        .toList
        .flatMap(x => getListOfFilesRecursively(x.toString()))
      d.listFiles.filter(_.isFile).map(_.toString()).concat(result)
    } else {
      Array[String]()
    }
  }

  def allSemanticallyFail(srcPath: String): Unit = {
    val allValidProgramPaths = getListOfFilesRecursively(srcPath)
    for (path <- allValidProgramPaths) {
      val source = Source.fromFile(path).mkString
      val maybeAst = parse(new File(path))
      maybeAst should matchPattern { case Success(_) => }
      semanticChecker.validateProgram(
        maybeAst.get,
        path
      ) should not matchPattern { case Nil =>
      }
    }
  }

  "All invalid array programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/exit")
  }

  "All invalid expressions programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/expressions")
  }

  // TODO: Something is succeeding here?
  "All invalid function programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/function")
  }

  "All invalid if programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/if")
  }

  "All invalid IO programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/IO")
  }

  "All invalid multiple programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/multiple")
  }

  "All invalid pairs programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/pairs")
  }

  "All invalid print programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/print")
  }

  "All invalid read programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/read")
  }

  "All invalid scope programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/scope")
  }

  // TODO: I think it's an int overflow error isn't caught
  "All invalid variables programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/variables")
  }

  "All invalid while programs" should "return failure" in {
    allSemanticallyFail("src/examples/invalid/semanticErr/while")
  }

}

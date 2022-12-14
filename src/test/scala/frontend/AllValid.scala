package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.io.File

class AllValid extends AnyFlatSpec {
    import parser._
    import scala.io.Source
    import parsley.{Success, Failure}

    behavior of "all valid programs"

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

    def allSucceed(srcPath: String) = {
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            //parse(source) should matchPattern { case Success(_) => }
            val maybeAst = parse(new File(path))// should matchPattern { case Success(_) => }
            maybeAst should matchPattern { case Success(_) => }
            semanticChecker.validateProgram(maybeAst.get, path) should matchPattern {
                    case Nil =>
            }
        }
    }

    // TODO: ISSUE WITH PAIRS
    "All valid advanced programs" should "return success" in {
        allSucceed("src/examples/valid/advanced")
    }

    // TODO: DOESN'T RECOGNISE []
    "All valid array programs" should "return success" in {
        allSucceed("src/examples/valid/array")
    }

    "All valid basic programs" should "return success" in {
        allSucceed("src/examples/valid/basic")
    }

    "All valid expressions programs" should "return success" in {
        allSucceed("src/examples/valid/expressions")
    }

    // TODO: DOESN'T RECOGNISE '('
    "All valid function programs" should "return success" in {
        allSucceed("src/examples/valid/function")
    }

    "All valid if programs" should "return success" in {
        allSucceed("src/examples/valid/if")
    }

    // TODO: DOESN'T RECOGNISE \n
    "All valid IO programs" should "return success" in {
        allSucceed("src/examples/valid/IO")
    }

    "All valid pairs programs" should "return success" in {
        allSucceed("src/examples/valid/pairs")
    }

    // fails because doesn't recognise array []
    "All valid runtimeErr programs" should "return success" in {
        allSucceed("src/examples/valid/runtimeErr")
    }

    // fails because doesn't recognise array []
    "All valid scope programs" should "return success" in {
        allSucceed("src/examples/valid/scope")
    }

    // fails because of pairs
    "All valid sequence programs" should "return success" in {
        allSucceed("src/examples/valid/sequence")
    }

    // fails because of pairs
    "All valid variables programs" should "return success" in {
        allSucceed("src/examples/valid/variables")
    }

    "All valid while programs" should "return success" in {
        allSucceed("src/examples/valid/while")
    }




}

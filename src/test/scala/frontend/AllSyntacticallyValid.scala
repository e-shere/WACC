package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.io.File

class AllSyntacticallyValid extends AnyFlatSpec {
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

    // TODO: ISSUE WITH PAIRS
    "All valid advanced programs" should "return success" in {
        val srcPath = "src/examples/valid/advanced"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    // TODO: DOESN'T RECOGNISE []
    "All valid array programs" should "return success" in {
        val srcPath = "src/examples/valid/array"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    "All valid basic programs" should "return success" in {
        val srcPath = "src/examples/valid/basic"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    "All valid expressions programs" should "return success" in {
        val srcPath = "src/examples/valid/expressions"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    // TODO: DOESN'T RECOGNISE '('
        "All valid function programs" should "return success" in {
            val srcPath = "src/examples/valid/function"
            val allValidProgramPaths = getListOfFilesRecursively(srcPath)
            for (path <- allValidProgramPaths) {
                val source = Source.fromFile(path).mkString
                parse(source) should matchPattern { case Success(_) => }
            }
        }

    "All valid if programs" should "return success" in {
        val srcPath = "src/examples/valid/if"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    // TODO: DOESN'T RECOGNISE \n
    "All valid IO programs" should "return success" in {
        val srcPath = "src/examples/valid/IO"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    "All valid pairs programs" should "return success" in {
        val srcPath = "src/examples/valid/pairs"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    // fails because doesn't recognise array []
    "All valid runtimeErr programs" should "return success" in {
        val srcPath = "src/examples/valid/runtimeErr"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    // fails because doesn't recognise array []
    "All valid scope programs" should "return success" in {
        val srcPath = "src/examples/valid/scope"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    // fails because of pairs
    "All valid sequence programs" should "return success" in {
        val srcPath = "src/examples/valid/sequence"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    // fails because of pairs
    "All valid variables programs" should "return success" in {
        val srcPath = "src/examples/valid/variables"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }

    "All valid while programs" should "return success" in {
        val srcPath = "src/examples/valid/while"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }




}
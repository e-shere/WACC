package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.io.File

class AllValid extends AnyFlatSpec {
    import parser._
    import ast._
    import parsley.Result
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

    "All basic valid programs" should "return success" in {
        val srcPath = "src/examples/valid/basic/skip"
        val allValidProgramPaths = getListOfFilesRecursively(srcPath)
        info(allValidProgramPaths.mkString)
        for (path <- allValidProgramPaths) {
            val source = Source.fromFile(path).mkString
            parse(source) should matchPattern { case Success(_) => }
        }
    }


    // class SuccessMatcher extends BeMatcher[Int] {
    //     def apply(result:  Result[ErrorBuilder[String], WaccProgram]) =
    //         MatchResult(
    //             left % 2 == 1,
    //             left.toString + " was even",
    //             left.toString + " was odd"
    //         )
    // }
}
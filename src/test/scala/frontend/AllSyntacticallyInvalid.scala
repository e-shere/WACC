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

	def allFail(srcPath: String) = {
		val allValidProgramPaths = getListOfFilesRecursively(srcPath)
		for (path <- allValidProgramPaths) {
			val source = Source.fromFile(path).mkString
			parse(source) should matchPattern { case Failure(_) => }
		}
	}

	"All invalid array programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/array")
	}

	"All invalid basic programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/basic")
	}

	"All invalid expressions programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/expressions")
	}

	// TODO: Something is succeeding here?
	"All invalid function programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/function")
	}

	"All invalid if programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/if")
	}

	"All invalid pairs programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/pairs")
	}
	"All invalid print programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/print")
	}

		"All invalid sequence programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/sequence")
	}

	// TODO: I think it's an int overflow error isn't caught
	"All invalid variables programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/variables")
	}

	"All invalid while programs" should "return failure" in {
			allFail("src/examples/invalid/syntaxErr/while")
	}

}

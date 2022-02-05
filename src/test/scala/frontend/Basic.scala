package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Basic extends AnyFlatSpec {
    import parser._
    import ast._
    import parsley.Result
    import scala.io.Source
    import parsley.{Success, Failure}

    behavior of "one line wacc programs"

    "begin skip end program" should "only skip" in {
        val source = Source.fromFile("src/examples/valid/basic/skip/skip.wacc").mkString
        parse(source).toString() shouldBe "Success(WaccProgram(List(),List(Skip((8,7)))))"
    }
}
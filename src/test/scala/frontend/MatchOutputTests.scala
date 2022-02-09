package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.io.File

class MatchOutputTests extends AnyFlatSpec {
    import parser._
    import ast._
    import parsley.Result
    import scala.io.Source
    import parsley.{Success, Failure}

    behavior of "one line wacc programs"

    "begin skip end program" should "only skip" in {
      parse(new File("src/examples/valid/basic/skip/skip.wacc")).toString() shouldBe "Success(WaccProgram(List(),List(Skip()(8,7)))(8,7))"
    }

}

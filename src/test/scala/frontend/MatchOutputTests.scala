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

    "minus test program" should "use minus, not unary/binary neg" in {
      println(parse(new File("src/examples/outputTests/minus.wacc")).toString())
      parse(new File("src/examples/outputTests/minus.wacc")).toString() shouldBe "Success(WaccProgram(List(),List(Declare(IntType()(2,3),Ident(x)(2,7),Neg(IntLiter(-2147483648)(2,12))(2,11))(2,3)))(2,3))"
    }

}
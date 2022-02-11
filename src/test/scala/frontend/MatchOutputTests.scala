package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import java.io.File

class MatchOutputTests extends AnyFlatSpec {
    import parser._

    behavior of "one line wacc programs"

    "begin skip end program" should "only skip" in {
      parse(new File("src/examples/valid/basic/skip/skip.wacc")).toString() shouldBe "Success(WaccProgram(List(),List(Skip()(8,7)))(8,7))"
    }

  "exitBasic program" should "exit with code 7" in {
    parse(new File("src/examples/valid/basic/exit/exitBasic.wacc")).toString() shouldBe "Success(WaccProgram(List(),List(Exit(IntLiter(7)(12,8))(12,3)))(12,3))"
  }

    "exit-1 program" should "exit with code -1" in {
      parse(new File("src/examples/valid/basic/exit/exit-1.wacc")).toString() shouldBe "Success(WaccProgram(List(),List(Exit(IntLiter(-1)(12,8))(12,3)))(12,3))"
    }

  "commentInLine program" should "only skip" in {
    parse(new File("src/examples/valid/basic/skip/commentInLine.wacc")).toString() shouldBe "Success(WaccProgram(List(),List(Skip()(9,3)))(9,3))"
  }

  "ifBasic program" should "skip in both branches" in {
    parse(new File("src/examples/valid/if/ifBasic.wacc")).toString() shouldBe "Success(WaccProgram(List(),List(If(BoolLiter(true)(9,6),List(Skip()(11,5)),List(Skip()(13,5)))(9,3)))(9,3))"
  }

  "functionDeclaration program" should "declare the 0 function" in {
    parse(new File("src/examples/valid/function/simple_functions/functionDeclaration.wacc")).toString() shouldBe
      "Success(WaccProgram(List(f),List(Skip()(12,3)))(9,3))"
  }

  "booleanExprs program" should "use and, or and not" in {
    parse(new File("src/examples/outputTests/boolExprs.wacc")).toString() shouldBe
      "Success(WaccProgram(List(),List(Declare(bool,a,BoolLiter(true)(12,12))(12,3), " +
        "Declare(bool,b,BoolLiter(false)(13,12))(13,3), Println(Or(a,b)(14,13))(14,3), " +
        "Println(And(a,b)(15,13))(15,3), Println(Not(a)(16,11))(16,3)))(12,3))"
  }

}

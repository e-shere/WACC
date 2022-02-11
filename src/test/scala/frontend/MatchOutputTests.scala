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
      "Success(WaccProgram(List(Func(int,Ident(f)(9,7),List(),List(Return(IntLiter(0)(10,12))(10,5)))(9,3)),List(Skip()(12,3)))(9,3))"
  }

  "booleanExprs program" should "use and, or and not" in {
    parse(new File("src/examples/outputTests/boolExprs.wacc")).toString() shouldBe "Success(WaccProgram(List()," +
      "List(Declare(bool,Ident(a)(12,8),BoolLiter(true)(12,12))(12,3), Declare(bool,Ident(b)(13,8),BoolLiter(false)(13,12))(13,3), " +
      "Println(Or(Ident(a)(14,11),Ident(b)(14,16))(14,13))(14,3), Println(And(Ident(a)(15,11),Ident(b)(15,16))(15,13))(15,3), " +
      "Println(Not(Ident(a)(16,12))(16,11))(16,3)))(12,3))"
  }

}

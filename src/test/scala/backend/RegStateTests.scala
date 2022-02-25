//package backend
//
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers._
//
//class RegStateTests extends AnyFlatSpec {
//  import backend.generator._
//
//  behavior of "state of registers"
//
//  "initial register state" should "have no used registers" in {
//    NEW_REG.reg shouldBe REG_START
//  }
//
//  "initial register state" should "have no placeholder values" in {
//    NEW_REG.placeholder shouldBe PLACEHOLDER_END
//  }
//
//  "writing to a register" should "increment register number" in {
//    NEW_REG.write._1.reg shouldBe NEW_REG.reg + 1
//  }
//
//  "first register written to" should "be r4" in {
//    NEW_REG.write._2 shouldBe "r4"
//  }
//
//  "an empty register state" should "not use stack" in {
//    assert(NEW_REG.isReg)
//    assert(!NEW_REG.isStack)
//  }
//
//  "writing to a not full register state" should "not use stack" in {
//    assert(NEW_REG.write._1.isReg)
//    assert(!NEW_REG.write._1.isStack)
//  }
//
//
//
//}

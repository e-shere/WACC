package backend

import backend.asm.AsmReg
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable

class StateTests extends AnyFlatSpec {
  import backend.state._

  behavior of "state of registers"

  "initial register state" should "have no used registers" in {
    NEW_REG.reg shouldBe REG_START
  }

  "first register" should "be 4" in {
    val reg = NEW_REG
    reg.write._1 shouldBe AsmReg(4)
    reg.write._3.read._1 shouldBe AsmReg(4)
    reg.reg shouldBe REG_START
  }

  "an empty register state" should "use registers" in {
    assert(NEW_REG.isReg)
    assert(!NEW_REG.isStack)
  }

  "a full register state" should "use stack" in {
    assert(State(AsmReg(10), Set(), Map.empty).isStack)
    assert(!State(AsmReg(10), Set(), Map.empty).isReg)
  }

  "top register" should "used in full stack" in {
    val reg = State(AsmReg(9), Set(), Map.empty)
    assert(reg.isReg)
    reg.write._1 shouldBe AsmReg(9)
    assert(reg.isReg)
  }

}

package backend

import backend.asm.AsmDefReg
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
    reg.write._1 shouldBe AsmDefReg(4)
    reg.write._3.read._1 shouldBe AsmDefReg(4)
    reg.reg shouldBe REG_START
  }

  "an empty register state" should "use registers" in {
    assert(NEW_REG.isReg)
    assert(!NEW_REG.isStack)
  }

  "a full register state" should "use stack" in {
    assert(State(AsmDefReg(10), Set()).isStack)
    assert(!State(AsmDefReg(10), Set()).isReg)
  }

  "top register" should "used in full stack" in {
    val reg = State(AsmDefReg(9), Set())
    assert(reg.isReg)
    reg.write._1 shouldBe AsmDefReg(9)
    assert(reg.isReg)
  }

}

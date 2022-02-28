package backend
import backend.state._

object auxState {

  val includedFuncs: Set[PredefinedFunc] = Set.empty

  def getPredefFuncs(): Step = {
    includedFuncs.foldLeft(Step.identity)(
      (prev, f) => prev <++> f.toStep
    )
  }


  // TODO: enum or case classes of each type of predefined functions
  sealed trait PredefinedFunc {
    def toStep: Step
  }

  case class print_ln() {
    def toStep: Step = ???
  }

  case class print_int() {
    def toStep: Step = ???
  }

  case class print_string() {
    def toStep: Step = ???
  }

  case class print_bool() {
    def toStep: Step = ???
  }

  case class print_ref() {
    def toStep: Step = ???
  }

  case class throw_overflow() {
    def toStep: Step = ???
  }

  case class throw_runtime() {
    def toStep: Step = ???
  }

  case class check_div_zero() {
    def toStep: Step = ???
  }

  case class free_pair() {
    def toStep: Step = ???
  }

  case class check_null_pointer() {
    def toStep: Step = ???
  }

  case class check_array_bound() {
    def toStep: Step = ???
  }

  case class read_char() {
    def toStep: Step = ???
  }

  case class read_int() {
    def toStep: Step = ???
  }

}

package frontend

object ast {
  case class WaccProgram(funcs: List[Func], stats: List[StatUnit])
  // todo: represent program arguments as something optional
  case class Func(t: Type, id: Ident, args: ParamList, body: List[StatUnit])
  case class ParamList(params: List[Param])
  case class Param(t: Type,id: Ident)

  sealed trait StatUnit
  case class Skip() extends StatUnit
  case class Declare(t: Type, id: Ident, rhs: AssignRhs) extends StatUnit
  case class Assign(lhs: AssignLhs, rhs: AssignRhs) extends StatUnit
  case class Read(lhs: AssignLhs) extends StatUnit
  case class Free(exp: Expr) extends StatUnit
  case class Return(exp: Expr) extends StatUnit
  case class Exit(exp: Expr) extends StatUnit
  case class Print(exp: Expr) extends StatUnit
  case class Println(exp: Expr) extends StatUnit
  case class If(exp: Expr, ifTrue: StatUnit, ifFalse: StatUnit) extends StatUnit
  case class While(exp: Expr, body: StatUnit) extends StatUnit
  case class SingleStat(stat: StatUnit) extends StatUnit
  case class Next(stat: StatUnit, nextStat: StatUnit) extends StatUnit

  sealed trait AssignLhs
  // todo: work out how to make exps non-empty
  case class ArrayElem(id: Ident, exps: List[Expr]) extends AssignLhs
  case class PairName(e: PairElem) extends AssignLhs

  //todo: use regex??
  case class Ident(id: String) extends AssignLhs with Expr

  sealed trait AssignRhs
  case class ExpAssignment(exp: Expr) extends AssignRhs
  case class ArrLiterAssignment(arrLiter: ArrLiter) extends AssignRhs
  case class Pair(fst: Expr, snd: Expr) extends AssignRhs
  case class PairElem(e: PairElem) extends AssignRhs
  case class Call(id: Ident, args: List[Expr])

  sealed trait ArrayElem

  sealed trait Type
  sealed trait PairElem
  sealed trait Expr
}

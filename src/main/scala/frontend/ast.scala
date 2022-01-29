package frontend

object ast {

  // Top level
  case class WaccProgram(funcs: List[Func], stats: List[Stat])
  // todo: represent program arguments as something optional
  case class Func(t: Type, id: Ident, args: List[Param], body: List[Stat])
  case class Param(t: Type,id: Ident)


  // Statements
  sealed trait Stat
  case object Skip extends Stat
  case class Declare(t: Type, id: Ident, rhs: AssignRhs) extends Stat
  case class Assign(lhs: AssignLhs, rhs: AssignRhs) extends Stat
  case class Read(lhs: AssignLhs) extends Stat
  case class Free(expr: Expr) extends Stat
  case class Return(expr: Expr) extends Stat
  case class Exit(expr: Expr) extends Stat
  case class Print(expr: Expr) extends Stat
  case class Println(expr: Expr) extends Stat
  case class If(expr: Expr, thenStats: List[Stat], elseStats: List[Stat]) extends Stat
  case class While(expr: Expr, doStats: List[Stat]) extends Stat
  case class Scope(stats: List[Stat]) extends Stat

  // Assignments
  sealed trait AssignLhs

  sealed trait AssignRhs

  case class NewPair(fst: Expr, snd: Expr) extends AssignRhs

  case class Call(id: Ident, args: List[Expr])

  sealed trait PairElem extends AssignLhs with AssignRhs
  case class Fst(expr: Expr) extends PairElem
  case class Snd(expr: Expr) extends PairElem

  // Types
  sealed trait Type
  
  sealed trait BaseType extends Type with PairElemType
  case object IntType extends BaseType
  case object BoolType extends BaseType
  case object CharType extends BaseType
  case object StringType extends BaseType

  case class ArrayType(ty: Type) extends Type with PairElemType

  case class PairType(ty1: PairElemType, ty2: PairElemType) extends Type

  sealed trait PairElemType
  case object NestedPairType extends PairElemType

  // Exprs
  sealed trait Expr

  // Binary operators

  // Precedence 6
  case class Or(x: Expr5, y: Expr) extends Expr

  // Precedence 5
  sealed trait Expr5 extends Expr
  case class And(x: Expr4, y: Expr5) extends Expr5

  // Precedence 4
  sealed trait Expr4 extends Expr5
  case class Eq(x: Expr3, y: Expr4) extends Expr4
  case class Neq(x: Expr3, y: Expr4) extends Expr4

  // Precedence 3
  sealed trait Expr3 extends Expr4
  case class Leq(x: Expr2, y: Expr3) extends Expr3
  case class Lt(x: Expr2, y: Expr3) extends Expr3
  case class Geq(x: Expr2, y: Expr3) extends Expr3
  case class Gt(x: Expr2, y: Expr3) extends Expr3

  // Precedence 2
  sealed trait Expr2 extends Expr3
  case class Add(x: Expr1, y: Expr2) extends Expr2
  case class Sub(x: Expr1, y: Expr2) extends Expr2

  // Precedence 1
  sealed trait Expr1 extends Expr2
  case class Mul(x: Expr0, y: Expr1) extends Expr1
  case class Div(x: Expr0, y: Expr1) extends Expr1
  case class Mod(x: Expr0, y: Expr1) extends Expr1

  // Other expression types
  sealed trait Expr0 extends Expr1

  // Literals

  // Primitives
  case class IntLiter(x: Int) extends Expr6
  case class BoolLiter(b: Boolean) extends Expr6
  case class CharLiter(c: Char) extends Expr6
  case class StrLiter(s: String) extends Expr6

  // Pairs
  sealed trait PairLiter extends Expr6
  case object Null extends PairLiter

  // Identifiers
  case class Ident(id: String) extends AssignLhs with Expr6

  // Array accesses
  case class ArrayElem(id: Ident, indexes: List[Expr])

  // Unary operators
  case class Not(x: Expr6) extends Expr6
  case class Neg(x: Expr6) extends Expr6
  case class Len(x: Expr6) extends Expr6
  case class Ord(x: Expr6) extends Expr6
  case class Chr(x: Expr6) extends Expr6

  // Parentheses
  case class Paren(expr: Expr6) extends Expr

  //todo: use regex??


}

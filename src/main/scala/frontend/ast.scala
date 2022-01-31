package frontend

import parsley.Parsley, Parsley._

object ast {

  trait NodeWithPosition {
    val pos: (Int, Int)
  }

  // Top level
  case class WaccProgram(funcs: List[Func], stats: List[Stat])(val pos: (Int, Int)) extends NodeWithPosition
  // todo: represent program arguments as something optional
  case class Func(t: Type, id: Ident, args: List[Param], body: List[Stat])(val pos: (Int, Int)) extends NodeWithPosition
  case class Param(t: Type,id: Ident) extends NodeWithPosition


  // Statements
  sealed trait Stat extends NodeWithPosition
  case class Skip(val pos: (Int, Int)) extends Stat
  case class Declare(t: Type, id: Ident, rhs: AssignRhs)(val pos: (Int, Int)) extends Stat
  case class Assign(lhs: AssignLhs, rhs: AssignRhs)(val pos: (Int, Int)) extends Stat
  case class Read(lhs: AssignLhs)(val pos: (Int, Int)) extends Stat
  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat
  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat
  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat
  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat
  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat
  case class If(expr: Expr, thenStats: List[Stat], elseStats: List[Stat])(val pos: (Int, Int)) extends Stat
  case class While(expr: Expr, doStats: List[Stat])(val pos: (Int, Int)) extends Stat
  case class Scope(stats: List[Stat])(val pos: (Int, Int)) extends Stat

  // Assignments
  sealed trait AssignLhs extends NodeWithPosition

  sealed trait AssignRhs extends NodeWithPosition

  case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int)) extends AssignRhs

  case class Call(id: Ident, args: List[Expr])(val pos: (Int, Int)) extends AssignRhs

  sealed trait PairElem extends AssignLhs with AssignRhs
  case class Fst(expr: Expr)(val pos: (Int, Int)) extends PairElem
  case class Snd(expr: Expr)(val pos: (Int, Int)) extends PairElem

  // Types
  sealed trait Type extends NodeWithPosition

  sealed trait BaseType extends Type with PairElemType
  case class IntType(val pos: (Int, Int)) extends BaseType
  case class BoolType(val pos: (Int, Int)) extends BaseType
  case class CharType(val pos: (Int, Int)) extends BaseType
  case class StringType(val pos: (Int, Int)) extends BaseType

  case class ArrayType(ty: Type)(val pos: (Int, Int)) extends Type with PairElemType

  case class PairType(ty1: PairElemType, ty2: PairElemType)(val pos: (Int, Int)) extends Type

  sealed trait PairElemType extends NodeWithPosition
  case class NestedPairType(val pos: (Int, Int)) extends PairElemType

  // Exprs
  sealed trait Expr extends NodeWithPosition

  // Binary operators

  // Precedence 6
  sealed trait Expr6 extends Expr
  case class Or(x: Expr6, y: Expr5)(val pos: (Int, Int)) extends Expr6

  // Precedence 5
  sealed trait Expr5 extends Expr6
  case class And(x: Expr5, y: Expr4)(val pos: (Int, Int)) extends Expr5

  // Precedence 4
  sealed trait Expr4 extends Expr5
  case class Eq(x: Expr4, y: Expr3)(val pos: (Int, Int)) extends Expr4
  case class Neq(x: Expr4, y: Expr3)(val pos: (Int, Int)) extends Expr4

  // Precedence 3
  sealed trait Expr3 extends Expr4
  case class Leq(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3
  case class Lt(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3
  case class Geq(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3
  case class Gt(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3

  // Precedence 2
  sealed trait Expr2 extends Expr3
  case class Add(x: Expr2, y: Expr1)(val pos: (Int, Int)) extends Expr2
  case class Sub(x: Expr2, y: Expr1)(val pos: (Int, Int)) extends Expr2

  // Precedence 1
  sealed trait Expr1 extends Expr2
  case class Mul(x: Expr1, y: Expr0)(val pos: (Int, Int)) extends Expr1
  case class Div(x: Expr1, y: Expr0)(val pos: (Int, Int)) extends Expr1
  case class Mod(x: Expr1, y: Expr0)(val pos: (Int, Int)) extends Expr1

  // Other expression types
  sealed trait Expr0 extends Expr1

  // Literals

  // Primitives
  case class IntLiter(x: Int)(val pos: (Int, Int)) extends Expr0
  case class BoolLiter(b: Boolean)(val pos: (Int, Int)) extends Expr0
  case class CharLiter(c: Char)(val pos: (Int, Int)) extends Expr0
  case class StrLiter(s: String)(val pos: (Int, Int)) extends Expr0

  // Pairs
  sealed trait PairLiter extends Expr0
  case class Null(val pos: (Int, Int)) extends PairLiter

  // Identifiers
  case class Ident(id: String)(val pos: (Int, Int)) extends AssignLhs with Expr0

  // Array accesses
  case class ArrayElem(id: Ident, indexes: List[Expr])(val pos: (Int, Int))

  // Unary operators
  case class Not(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Neg(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Len(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Ord(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Chr(x: Expr0)(val pos: (Int, Int)) extends Expr0

  // Parentheses
  case class Paren(expr: Expr0)(val pos: (Int, Int)) extends Expr

  //todo: use regex??

  trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
  }

  trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
        def apply(x: T1)(pos: (Int, Int)): R
        val parser = pos.map(p => apply(_)(p))
  }
  
  trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
        def apply(x: T1, y: T2)(pos: (Int, Int)): R
        val parser = pos.map(p => apply(_, _)(p))
  }

  object Or extends ParserBuilderPos2[Expr6, Expr5, Expr6]
  
  object And extends ParserBuilderPos2[Expr5, Expr4, Expr5]

  object Eq extends ParserBuilderPos2[Expr4, Expr3, Expr4]
  object Neq extends ParserBuilderPos2[Expr4, Expr3, Expr4]

  object Leq extends ParserBuilderPos2[Expr3, Expr2, Expr3]
  object Lt extends ParserBuilderPos2[Expr3, Expr2, Expr3]
  object Geq extends ParserBuilderPos2[Expr3, Expr2, Expr3]
  object Gt extends ParserBuilderPos2[Expr3, Expr2, Expr3]

  object Add extends ParserBuilderPos2[Expr2, Expr1, Expr2]
  object Sub extends ParserBuilderPos2[Expr2, Expr1, Expr2]
  
  object Mul extends ParserBuilderPos2[Expr1, Expr0, Expr1]
  object Div extends ParserBuilderPos2[Expr1, Expr0, Expr1]
  object Mod extends ParserBuilderPos2[Expr1, Expr0, Expr1]


  // object IntType extends ParserBuilder[IntType] {
  //   val parser = pos.map(IntType)
  // }
}

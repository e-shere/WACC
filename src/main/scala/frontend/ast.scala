package frontend

import parsley.Parsley, Parsley._
import parsley.implicits.zipped.Zipped2
import parsley.implicits.zipped.Zipped3
import parsley.implicits.zipped.Zipped4


object ast {

  trait NodeWithPosition {
    val pos: (Int, Int)
  }

  // Top level
  case class WaccProgram(funcs: List[Func], stats: List[Stat])(val pos: (Int, Int)) extends NodeWithPosition
  // todo: represent program arguments as something optional
  case class Func(ty: Type, id: Ident, args: List[Param], body: List[Stat])(val pos: (Int, Int)) extends NodeWithPosition
  case class Param(ty: Type, id: Ident)(val pos: (Int, Int)) extends NodeWithPosition


  // Statements
  sealed trait Stat extends NodeWithPosition
  case class Skip(val pos: (Int, Int)) extends Stat
  case class Declare(ty: Type, id: Ident, rhs: AssignRhs)(val pos: (Int, Int)) extends Stat
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
  sealed trait Expr extends AssignRhs

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
  case class ArrayElem(id: Ident, indexes: List[Expr])(val pos: (Int, Int)) extends AssignLhs

  // Unary operators
  case class Not(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Neg(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Len(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Ord(x: Expr0)(val pos: (Int, Int)) extends Expr0
  case class Chr(x: Expr0)(val pos: (Int, Int)) extends Expr0

  // Parentheses
  case class Paren(expr: Expr)(val pos: (Int, Int)) extends Expr0

  //todo: use regex??

  // Begin builders/companion objects

  trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
  }

  trait ParserBuilderPos0[R] extends ParserBuilder[R] {
    def apply(pos: (Int, Int)): R
    val parser: Parsley[R] = pos.map(p => apply(p))
  }

  trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1)(pos: (Int, Int)): R
    val parser: Parsley[T1 => R] = pos.map(p => apply(_)(p))
  }
  
  trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2)(pos: (Int, Int)): R
    val parser: Parsley[(T1, T2) => R] = pos.map(p => apply(_, _)(p))
  }

  object Func {
    def apply(ty: Parsley[Type], id: Parsley[Ident], args: Parsley[List[Param]], body: Parsley[List[Stat]]): Parsley[Func] = 
      pos <**> (ty, id, args, body).zipped(Func(_, _, _, _) _)
  }

  object Param {
    def apply(ty: Parsley[Type], id: Parsley[Ident]): Parsley[Param] = pos <**> (ty, id).zipped(Param(_,_) _)
  }

  object Skip extends ParserBuilderPos0[Skip]

  object Declare {
    def apply(ty: Parsley[Type], id: Parsley[Ident], rhs: Parsley[AssignRhs]): Parsley[Declare] =
      pos <**> (ty, id, rhs).zipped(Declare(_, _, _) _)
  }

  object Assign {
    def apply(lhs: Parsley[AssignLhs], rhs: Parsley[AssignRhs]): Parsley[Assign] = pos <**> (lhs, rhs).zipped(Assign(_, _) _)
  }

  object Read {
    def apply(lhs: Parsley[AssignLhs]): Parsley[Read] = pos <**> lhs.map(Read(_) _)
  }
  
  object Free {
    def apply(expr: Parsley[Expr]): Parsley[Free] = pos <**> expr.map(Free(_) _)
  }

  object Return {
    def apply(expr: Parsley[Expr]): Parsley[Return] = pos <**> expr.map(Return(_) _)
  }

  object Exit {
    def apply(expr: Parsley[Expr]): Parsley[Exit] = pos <**> expr.map(Exit(_) _)
  }

  object Print {
    def apply(expr: Parsley[Expr]): Parsley[Print] = pos <**> expr.map(Print(_) _)
  }

  object Println {
    def apply(expr: Parsley[Expr]): Parsley[Println] = pos <**> expr.map(Println(_) _)
  }

  object If {
    def apply(expr: Parsley[Expr], thenStats: Parsley[List[Stat]], elseStats: Parsley[List[Stat]]): Parsley[If] =
      pos <**> (expr, thenStats, elseStats).zipped(If(_, _, _) _)
  }

  object While {
    def apply(expr: Parsley[Expr], doStats: Parsley[List[Stat]]): Parsley[While] =
      pos <**> (expr, doStats).zipped(While(_, _) _)
  }

  object Scope {
    def apply(stats: Parsley[List[Stat]]): Parsley[Scope] =
      pos <**> (stats).map(Scope(_) _)
  }

  // Parser builder for assignments
  object NewPair {
    def apply(fst: Parsley[Expr], snd: Parsley[Expr]): Parsley[NewPair] = pos <**> (fst, snd).zipped(NewPair(_,_) _)
  }

  object Call {
    def apply(id: Parsley[Ident], args: Parsley[List[Expr]]): Parsley[Call] = pos <**> (id, args).zipped(Call(_,_) _)
  }

  object Fst {
    def apply(expr: Parsley[Expr]): Parsley[Fst] = pos <**> expr.map(Fst(_) _)
  }

  object Snd {
    def apply(expr: Parsley[Expr]): Parsley[Snd] = pos <**> expr.map(Snd(_) _)
  }

  // Parser builder for types
  object IntType extends ParserBuilder[IntType]
  object BoolType extends ParserBuilder[BoolType]
  object CharType extends ParserBuilder[CharType]
  object StringType extends ParserBuilder[StringType]

  object ArrayType {
    def apply(ty: Parsley[Type]): Parsley[ArrayType] = pos <**> ty.map(ArrayType(_))
  }

  object PairType {
    def apply(ty1: Parsley[PairElemType], ty2: Parsley[PairElemType]): Parsley[PairType] = pos <**> (ty1, ty2).zipped(PairType(_,_) _)
  }

  object NestedPairType extends ParserBuilder[NestedPairType]

  // Expressions with precedence 6
  object Or extends ParserBuilderPos2[Expr6, Expr5, Expr6]
  
  // Expressions with precedence 5
  object And extends ParserBuilderPos2[Expr5, Expr4, Expr5]

  // Expressions with precedence 4
  object Eq extends ParserBuilderPos2[Expr4, Expr3, Expr4]
  object Neq extends ParserBuilderPos2[Expr4, Expr3, Expr4]

  // Expressions with precedence 3
  object Leq extends ParserBuilderPos2[Expr3, Expr2, Expr3]
  object Lt extends ParserBuilderPos2[Expr3, Expr2, Expr3]
  object Geq extends ParserBuilderPos2[Expr3, Expr2, Expr3]
  object Gt extends ParserBuilderPos2[Expr3, Expr2, Expr3]

  // Expressions with precedence 2
  object Add extends ParserBuilderPos2[Expr2, Expr1, Expr2]
  object Sub extends ParserBuilderPos2[Expr2, Expr1, Expr2]

  // Expressions with precedence 1
  object Mul extends ParserBuilderPos2[Expr1, Expr0, Expr1]
  object Div extends ParserBuilderPos2[Expr1, Expr0, Expr1]
  object Mod extends ParserBuilderPos2[Expr1, Expr0, Expr1]
  
  object IntLiter {
    def apply(x: Parsley[Int]): Parsley[IntLiter] = pos <**> x.map(IntLiter(_) _)
  }

  object BoolLiter {
    def apply(b: Parsley[Boolean]): Parsley[BoolLiter] = pos <**> b.map(BoolLiter(_) _)
  }

  object CharLiter {
    def apply(c: Parsley[Char]): Parsley[CharLiter] = pos <**> c.map(CharLiter(_) _)
  }

  object StrLiter {
    def apply(s: Parsley[String]): Parsley[StrLiter] = pos <**> s.map(StrLiter(_) _)
  }

  object Null extends ParserBuilderPos0[Null]

  object Ident {
    def apply(id: Parsley[String]): Parsley[Ident] = pos <**> id.map(Ident(_) _)
  }

  object ArrayElem {
    def apply(id: Parsley[Ident], indexes: Parsley[List[Expr]]): Parsley[ArrayElem] = pos <**> (id, indexes).zipped(ArrayElem(_, _) _)
  }
  
  // Unary operators
  object Not extends ParserBuilderPos1[Expr0, Expr0]
  object Neg extends ParserBuilderPos1[Expr0, Expr0]
  object Len extends ParserBuilderPos1[Expr0, Expr0]
  object Ord extends ParserBuilderPos1[Expr0, Expr0]
  object Chr extends ParserBuilderPos1[Expr0, Expr0]

  object Paren {
    def apply(expr: Parsley[Expr]): Parsley[Paren] = pos <**> expr.map(Paren(_) _)
  }

}

package frontend

import frontend.semanticChecker.FuncType
import frontend.symbols.TypeTable
import parsley.Parsley
import parsley.Parsley._
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

import scala.runtime.ScalaRunTime

object ast {

  // used as the pos of a synthetic NodeWithPosition whose pos carries no meaning
  val NO_POS = (-1, -1)

  // Statements
  sealed trait Stat extends NodeWithPosition

  // Assignments
  sealed trait AssignLhs extends NodeWithPosition

  sealed trait AssignRhs extends NodeWithPosition

  sealed trait PairElem extends AssignLhs with AssignRhs

  // The union of Type and PairElemType
  sealed trait TypeOrPairElemType extends NodeWithPosition {
    def toTypeName: String
    override def toString(): String = toTypeName
    def coercesTo(that: TypeOrPairElemType): Boolean =
      (this, that) match {
        case (AnyType(), _) => true
        case (_, AnyType()) => true
        case (PairType(x1, y1), PairType(x2, y2)) =>
          (x1 coercesTo x2) && (y1 coercesTo y2)
        case (ArrayType(x), ArrayType(y))          => x coercesTo y
        case (ArrayType(CharType()), StringType()) => true
        case _                                     => this == that
      }
  }

  // The intersection of Type and PairElemType
  sealed trait TypeAndPairElemType extends Type with PairElemType {
    def toPairElemType: PairElemType = this
    def toType: Type = this
  }

  sealed trait Type extends TypeOrPairElemType {
    def toPairElemType: PairElemType
    def withPos(pos: (Int, Int)): Type
  }

  sealed trait BaseType extends TypeAndPairElemType

  sealed trait PairElemType extends TypeOrPairElemType {
    def toType: Type
  }

  // Exprs
  sealed trait Expr extends AssignRhs

  // Precedence 6
  sealed trait Expr6 extends Expr

  // Precedence 5
  sealed trait Expr5 extends Expr6

  // Precedence 4
  sealed trait Expr4 extends Expr5

  // Precedence 3
  sealed trait Expr3 extends Expr4

  // Precedence 2
  sealed trait Expr2 extends Expr3

  // Precedence 1
  sealed trait Expr1 extends Expr2

  // Other expression types
  sealed trait Expr0 extends Expr1

  // Pairs
  sealed trait PairLiter extends Expr0

  sealed trait ArrayIdent extends AssignLhs with Expr0

  trait NodeWithPosition extends Product {
    val pos: (Int, Int)

    // This causes all positions to be shown in the printed AST
    // Otherwise, only the first argument set is shown
    override def toString(): String = (
      ScalaRunTime._toString(
        this
      ) // The usual string representation of a case class
        + pos.toString // Comment out this line to disable position printing
    )
  }

  trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
  }

  trait ParserBuilderPos0[R] extends ParserBuilder[R] {
    val parser: Parsley[R] = pos.map(p => apply()(p))

    def apply()(pos: (Int, Int)): R
  }

  trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
    val parser: Parsley[T1 => R] = pos.map(p => apply(_)(p))

    def apply(x: T1)(pos: (Int, Int)): R
  }

  trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    val parser: Parsley[(T1, T2) => R] = pos.map(p => apply(_, _)(p))

    def apply(x: T1, y: T2)(pos: (Int, Int)): R
  }

  // Types

  trait ParserBuilderCurriedFlippedPos2[T1, T2, R]
      extends ParserBuilder[T2 => T1 => R] {
    val parser: Parsley[T2 => T1 => R] = pos.map(p => y => apply(_, y)(p))

    def apply(x: T1, y: T2)(pos: (Int, Int)): R
  }

  // Top level
  case class WaccProgram(funcs: List[Func], stats: List[Stat])(
      val pos: (Int, Int)) extends NodeWithPosition {
    var mainSymbols: Option[TypeTable] = None
    var funcSymbols: Option[Map[Ident, FuncType]] = None
    var printSymbols: Map[(Int, Int), Type] = Map[(Int, Int), Type]()
  }

  case class Func(ty: Type, id: Ident, args: List[Param], body: List[Stat])(
      val pos: (Int, Int)
  ) extends NodeWithPosition {
    override def toString(): String = id.toString()
    var symbols: Option[TypeTable] = None
  }

  case class Param(ty: Type, id: Ident)(val pos: (Int, Int))
      extends NodeWithPosition {
    override def toString(): String = id.toString()
  }

  case class Skip()(val pos: (Int, Int)) extends Stat

  case class Declare(ty: Type, id: Ident, rhs: AssignRhs)(val pos: (Int, Int))
      extends Stat

  case class Assign(lhs: AssignLhs, rhs: AssignRhs)(val pos: (Int, Int))
      extends Stat

  case class Read(lhs: AssignLhs)(val pos: (Int, Int)) extends Stat

  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class If(expr: Expr, thenStats: List[Stat], elseStats: List[Stat])(
      val pos: (Int, Int)
  ) extends Stat {
    var thenTypeTable: Option[TypeTable] = None
    var elseTypeTable: Option[TypeTable] = None
  }

  // Binary operators

  case class While(expr: Expr, doStats: List[Stat])(val pos: (Int, Int))
      extends Stat {
    var doTypeTable: Option[TypeTable] = None
  }

  case class Scope(stats: List[Stat])(val pos: (Int, Int)) extends Stat {
    var typeTable: Option[TypeTable] = None
  }


  case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int))
      extends AssignRhs {
    override def toString(): String = "pair"
  }

  case class Call(id: Ident, args: List[Expr])(val pos: (Int, Int))
      extends AssignRhs

  case class Fst(expr: Expr)(val pos: (Int, Int)) extends PairElem

  case class Snd(expr: Expr)(val pos: (Int, Int)) extends PairElem

  case class AnyType()(val pos: (Int, Int)) extends TypeAndPairElemType {
    def toTypeName: String = "any"
    def withPos(pos: (Int, Int)): AnyType = AnyType()(pos)
  }

  case class IntType()(val pos: (Int, Int)) extends BaseType {
    def toTypeName: String = "int"
    def withPos(pos: (Int, Int)): IntType = IntType()(pos)
  }

  case class BoolType()(val pos: (Int, Int)) extends BaseType {
    def toTypeName: String = "bool"
    def withPos(pos: (Int, Int)): BoolType = BoolType()(pos)
  }

  case class CharType()(val pos: (Int, Int)) extends BaseType {
    def toTypeName: String = "char"
    def withPos(pos: (Int, Int)): CharType = CharType()(pos)
  }

  case class StringType()(val pos: (Int, Int)) extends BaseType {
    def toTypeName: String = "string"
    def withPos(pos: (Int, Int)): StringType = StringType()(pos)
  }

  case class ArrayType(ty: Type)(val pos: (Int, Int))
      extends TypeAndPairElemType {
    def toTypeName: String = ty.toTypeName + "[]"
    def withPos(pos: (Int, Int)): ArrayType = ArrayType(ty)(pos)
  }

  case class PairType(ty1: PairElemType, ty2: PairElemType)(val pos: (Int, Int))
      extends Type {
    def toTypeName: String =
      "pair(" + ty1.toTypeName + ", " + ty2.toTypeName + ")"
    def toPairElemType: PairElemType = NestedPairType()(pos)
    def withPos(pos: (Int, Int)): PairType = PairType(ty1, ty2)(pos)
  }

  case class NestedPairType()(val pos: (Int, Int)) extends PairElemType {
    def toTypeName: String = "pair"
    def toType: Type = PairType(AnyType()(NO_POS), AnyType()(NO_POS))(pos)
  }

  case class Or(x: Expr6, y: Expr5)(val pos: (Int, Int)) extends Expr6

  case class And(x: Expr5, y: Expr4)(val pos: (Int, Int)) extends Expr5

  case class Eq(x: Expr4, y: Expr3)(val pos: (Int, Int)) extends Expr4

  case class Neq(x: Expr4, y: Expr3)(val pos: (Int, Int)) extends Expr4

  case class Leq(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3

  case class Lt(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3

  // Literals

  case class Geq(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3

  case class Gt(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr3

  case class Add(x: Expr2, y: Expr1)(val pos: (Int, Int)) extends Expr2

  case class Sub(x: Expr2, y: Expr1)(val pos: (Int, Int)) extends Expr2

  case class Mul(x: Expr1, y: Expr0)(val pos: (Int, Int)) extends Expr1

  case class Div(x: Expr1, y: Expr0)(val pos: (Int, Int)) extends Expr1

  case class Mod(x: Expr1, y: Expr0)(val pos: (Int, Int)) extends Expr1

  // Primitives
  case class IntLiter(x: Int)(val pos: (Int, Int)) extends Expr0

  case class BoolLiter(b: Boolean)(val pos: (Int, Int)) extends Expr0

  case class CharLiter(c: Char)(val pos: (Int, Int)) extends Expr0

  case class StrLiter(s: String)(val pos: (Int, Int)) extends Expr0

  case class Null()(val pos: (Int, Int)) extends PairLiter

  // Arrays
  case class ArrayLiter(xs: List[Expr])(val pos: (Int, Int)) extends Expr0

  // Identifiers
  case class Ident(id: String)(val pos: (Int, Int)) extends ArrayIdent {
    override def toString(): String = id.toString
  }

  // Array accesses
  case class ArrayElem(id: ArrayIdent, index: Expr)(val pos: (Int, Int))
      extends ArrayIdent

  // Unary operators
  case class Not(x: Expr0)(val pos: (Int, Int)) extends Expr0
  // Begin builders/companion objects

  case class Neg(x: Expr0)(val pos: (Int, Int)) extends Expr0

  case class Len(x: Expr0)(val pos: (Int, Int)) extends Expr0

  case class Ord(x: Expr0)(val pos: (Int, Int)) extends Expr0

  case class Chr(x: Expr0)(val pos: (Int, Int)) extends Expr0

  // Parentheses
  case class Paren(expr: Expr)(val pos: (Int, Int)) extends Expr0

  object WaccProgram {
    def apply(
        funcs: Parsley[List[Func]],
        stats: Parsley[List[Stat]]
    ): Parsley[WaccProgram] =
      pos <**> (funcs, stats).zipped(WaccProgram(_, _) _)
  }

  object Func {
    def apply(
        ty: Parsley[Type],
        id: Parsley[Ident],
        args: Parsley[List[Param]],
        body: Parsley[List[Stat]]
    ): Parsley[Func] =
      pos <**> (ty, id, args, body).zipped(Func(_, _, _, _) _)
  }

  object Param {
    def apply(ty: Parsley[Type], id: Parsley[Ident]): Parsley[Param] =
      pos <**> (ty, id).zipped(Param(_, _) _)
  }

  object Skip extends ParserBuilderPos0[Skip]

  object Declare {
    def apply(
        ty: Parsley[Type],
        id: Parsley[Ident],
        rhs: Parsley[AssignRhs]
    ): Parsley[Declare] =
      pos <**> (ty, id, rhs).zipped(Declare(_, _, _) _)
  }

  object Assign {
    def apply(
        lhs: Parsley[AssignLhs],
        rhs: Parsley[AssignRhs]
    ): Parsley[Assign] = pos <**> (lhs, rhs).zipped(Assign(_, _) _)
  }

  object Read {
    def apply(lhs: Parsley[AssignLhs]): Parsley[Read] =
      pos <**> lhs.map(Read(_) _)
  }

  object Free {
    def apply(expr: Parsley[Expr]): Parsley[Free] = pos <**> expr.map(Free(_) _)
  }

  object Return {
    def apply(expr: Parsley[Expr]): Parsley[Return] =
      pos <**> expr.map(Return(_) _)
  }

  object Exit {
    def apply(expr: Parsley[Expr]): Parsley[Exit] = pos <**> expr.map(Exit(_) _)
  }

  object Print {
    def apply(expr: Parsley[Expr]): Parsley[Print] =
      pos <**> expr.map(Print(_) _)
  }

  object Println {
    def apply(expr: Parsley[Expr]): Parsley[Println] =
      pos <**> expr.map(Println(_) _)
  }

  object If {
    def apply(
        expr: Parsley[Expr],
        thenStats: Parsley[List[Stat]],
        elseStats: Parsley[List[Stat]]
    ): Parsley[If] =
      pos <**> (expr, thenStats, elseStats).zipped(If(_, _, _) _)
  }

  object While {
    def apply(
        expr: Parsley[Expr],
        doStats: Parsley[List[Stat]]
    ): Parsley[While] =
      pos <**> (expr, doStats).zipped(While(_, _) _)
  }

  object Scope {
    def apply(stats: Parsley[List[Stat]]): Parsley[Scope] =
      pos <**> (stats).map(Scope(_) _)
  }

  // Parser builder for assignments
  object NewPair {
    def apply(fst: Parsley[Expr], snd: Parsley[Expr]): Parsley[NewPair] =
      pos <**> (fst, snd).zipped(NewPair(_, _) _)
  }

  object Call {
    def apply(id: Parsley[Ident], args: Parsley[List[Expr]]): Parsley[Call] =
      pos <**> (id, args).zipped(Call(_, _) _)
  }

  object Fst {
    def apply(expr: Parsley[Expr]): Parsley[Fst] = pos <**> expr.map(Fst(_) _)
  }

  object Snd {
    def apply(expr: Parsley[Expr]): Parsley[Snd] = pos <**> expr.map(Snd(_) _)
  }

  // Parser builder for types
  object IntType extends ParserBuilderPos0[IntType]
  object BoolType extends ParserBuilderPos0[BoolType]
  object CharType extends ParserBuilderPos0[CharType]
  object StringType extends ParserBuilderPos0[StringType]

  object ArrayType extends ParserBuilderPos1[Type, ArrayType]

  object PairType {
    def apply(
        ty1: Parsley[PairElemType],
        ty2: Parsley[PairElemType]
    ): Parsley[PairType] = pos <**> (ty1, ty2).zipped(PairType(_, _) _)
  }

  object NestedPairType extends ParserBuilderPos0[NestedPairType]

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
    def apply(x: Parsley[Int]): Parsley[IntLiter] =
      pos <**> x.map(IntLiter(_) _)
  }

  object BoolLiter {
    def apply(b: Parsley[Boolean]): Parsley[BoolLiter] =
      pos <**> b.map(BoolLiter(_) _)
  }

  object CharLiter {
    def apply(c: Parsley[Char]): Parsley[CharLiter] =
      pos <**> c.map(CharLiter(_) _)
  }

  object StrLiter {
    def apply(s: Parsley[String]): Parsley[StrLiter] =
      pos <**> s.map(StrLiter(_) _)
  }

  object Null extends ParserBuilderPos0[Null]

  object ArrayLiter {
    def apply(xs: Parsley[List[Expr]]): Parsley[ArrayLiter] =
      pos <**> xs.map(ArrayLiter(_) _)
  }

  object Ident {
    def apply(id: Parsley[String]): Parsley[Ident] = pos <**> id.map(Ident(_) _)
  }

  object ArrayElem
      extends ParserBuilderCurriedFlippedPos2[ArrayIdent, Expr, ArrayElem]

  // Unary operators
  object Not extends ParserBuilderPos1[Expr0, Expr0]
  object Neg extends ParserBuilderPos1[Expr0, Expr0]
  object Len extends ParserBuilderPos1[Expr0, Expr0]
  object Ord extends ParserBuilderPos1[Expr0, Expr0]
  object Chr extends ParserBuilderPos1[Expr0, Expr0]

  object Paren {
    def apply(expr: Parsley[Expr]): Parsley[Paren] =
      pos <**> expr.map(Paren(_) _)
  }

}

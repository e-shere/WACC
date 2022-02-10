package frontend

import lexer._
import implicits.implicitToken
import ast._
import parsley.Parsley
import parsley.Parsley._
import parsley.expr.{Atoms, InfixL, Prefix, SOps, precedence}
import parsley.implicits.character.charLift
import parsley.errors.combinator.ErrorMethods

import scala.language.implicitConversions
import parsley.combinator.{many, sepBy, sepBy1, some}
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.ErrorMethods
import parsley.Result
import parsley.debug._
import parsley.io.ParseFromIO

import java.io.File
import parsley.expr.chain

object parser {

    def parse[Err: ErrorBuilder](input: File): Result[Err, WaccProgram] = `<program>`.parseFromFile(input).get

    private lazy val `<program>` = fully("begin" *> WaccProgram(many(`<func>`), sepBy1(`<stat>`, ";".label("new statement")) <* "end"))

    private lazy val `<func>` = attempt(Func(`<type>`, `<ident>`, "(" *> sepBy(`<param>`, ",") <* ")", "is" *> sepBy1(`<stat>`, ";".label("new statement")).filterOut(functions_return) <* "end"))

    private lazy val `<param>` = Param(`<type>`, `<ident>`)

    private lazy val `<stat>`: Parsley[Stat] =
           (Skip <# "skip"
        <|> Declare(`<type>`, `<ident>`, "=" *> `<assign-rhs>`)
        <|> Assign(`<assign-lhs>`, "=" *> `<assign-rhs>`)
        <|> Read("read" *> `<assign-lhs>`)
        <|> Free("free" *> `<expr>`)
        <|> Return("return" *> `<expr>`)
        <|> Exit("exit" *> `<expr>`)
        <|> Print("print" *> `<expr>`)
        <|> Println("println" *> `<expr>`)
        <|> If("if" *> `<expr>`, "then" *> sepBy1(`<stat>`, ";".label("new statement")), "else" *> sepBy1(`<stat>`,";".label("new statement")) <* "fi")
        <|> While("while" *> `<expr>`, "do" *> sepBy1(`<stat>`, ";".label("new statement")) <* "done")
        <|> Scope("begin" *> sepBy1(`<stat>`,";".label("new statement")) <* "end")
    ).label("statement").explain("Examples of statements are new variables, " +
             "print instructions and the start of while or if expressions.")

    private lazy val `<assign-lhs>` = `<array-ident>` <|> `<pair-elem>`

    private lazy val `<assign-rhs>` = 
           (`<expr>`
        <|> `<array-liter>`
        <|> NewPair("newpair" *> "(" *> `<expr>` <* ",", `<expr>` <* ")")
        <|> `<pair-elem>`
        <|> Call("call" *> `<ident>`, "(" *> sepBy(`<expr>`, ",") <* ")").label("call to a function"))

    private lazy val `<pair-elem>` = Fst("fst" *> `<expr>`) <|> Snd("snd" *> `<expr>`)

    private lazy val `<type>`: Parsley[Type] = chain.postfix(`<base-type>` <|> `<pair-type>`,
      ArrayType <# ("[" <* "]")).label("type")
      .explain("A type can be an array, pair, integer, boolean, character or string.")

  private lazy val `<base-type>` = (IntType <# "int") <|>
                                      (BoolType <# "bool") <|>
                                      (CharType <# "char") <|>
                                     (StringType <# "string")

    private lazy val `<pair-type>`: Parsley[PairType] = PairType("pair" *> "(" *> `<pair-elem-type>` <* ",", `<pair-elem-type>` <* ")")

    private lazy val `<expr>`: Parsley[Expr] =
        precedence(SOps(InfixL)(Or  <# "||".label("operator")) +:
                SOps(InfixL)(And <# "&&".label("operator")) +:
                SOps(InfixL)(Eq  <# "==".label("operator"), Neq <# "!=".label("operator")) +:
                SOps(InfixL)(Lt  <# "<".label("operator"),  Leq <# "<=".label("operator"),
                             Gt  <# ">".label("operator"),  Geq <# ">=".label("operator")) +:
                SOps(InfixL)(Add <# "+".label("operator"),  Sub <# "-".label("operator")) +:
                SOps(InfixL)(Mul <# "*".label("operator"),  Div <# "/", Mod <# "%".label("operator")) +:
                SOps(Prefix)(Neg <# NEG,  Not <# "!".label("operator"), Len <# "len", Ord <# "ord", Chr <# "chr") +:
                `<expr0>`)


    private lazy val `<expr0>` = Atoms(
        IntLiter(INT),
        BoolLiter(BOOL),
        CharLiter(CHAR),
        StrLiter(STRING),
        Null <# "null",
        `<array-ident>`,
        Paren("(" *> `<expr>` <* ")")
    )

    private lazy val `<array-liter>` = ArrayLiter("[" *> sepBy(`<expr>`, ",") <* "]")

    private lazy val `<ident>` = Ident(ID)

  // ???: parsley[ident => array_elem]
    private lazy val `<array-ident>` = chain.postfix(`<ident>`, ArrayElem <# "[" <*> `<expr>` <* "]")

//    private lazy val `<array-elem>` = ArrayElem(`<ident>`, some("[" *> `<expr>` <* "]"))

    private lazy val `<pair-elem-type>`: Parsley[PairElemType] = attempt(chain.postfix1(`<base-type>` <|> `<pair-type>`, ArrayType <# ("[" <* "]"))) <|> `<base-type>` <|> (NestedPairType <# "pair")


    // defined for all which fail!
    private val functions_return = new PartialFunction[List[Stat], String] {
        def apply(x: List[Stat]) = "All functions must end in a return statement"
        def isDefinedAt(x: List[Stat]): Boolean = {
          x.last match {
              case Return(_) => false
              case Exit(_) => false
              case While(_, doStats) => isDefinedAt(doStats)
              case If(_, thenStats, elseStats) => isDefinedAt(thenStats) || isDefinedAt(elseStats)
              case Scope(stats) => isDefinedAt(stats)
              case _ => true
          }
        }
    }
}
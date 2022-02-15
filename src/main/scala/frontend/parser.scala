package frontend

import frontend.Errors.{WaccError, WaccErrorBuilder}
import frontend.ast._
import frontend.lexer._
import frontend.lexer.implicits.implicitToken
import parsley.Parsley._
import parsley.combinator.{many, sepBy, sepBy1}
import parsley.errors.combinator.ErrorMethods
import parsley.expr._
import parsley.io.ParseFromIO
import parsley.{Parsley, Result}

import java.io.File
import scala.language.implicitConversions

object parser {
  implicit val eb: WaccErrorBuilder = new WaccErrorBuilder

  def parse(input: File): Result[WaccError, WaccProgram] =
    `<program>`.parseFromFile(input).get

  private lazy val `<program>` = fully(
    "begin" *> WaccProgram(
      many(`<func>`),
      sepBy1(`<stat>`, ";") <* "end"
    )
  )

  private lazy val `<func>` = attempt(
    Func(
      `<type>`,
      `<ident>`,
      "(" *> sepBy(`<param>`, ",") <* ")",
      "is" *> sepBy1(`<stat>`, ";")
        .filterOut(functions_return) <* "end"
    )
  )

  private lazy val `<param>` = Param(`<type>`, `<ident>`)

  private lazy val `<stat>` : Parsley[Stat] =
    (Skip <# "skip"
      <|> Declare(`<type>`, `<ident>`, "=" *> `<assign-rhs>`)
      <|> Assign(`<assign-lhs>`, "=" *> `<assign-rhs>`)
      <|> Read("read" *> `<assign-lhs>`)
      <|> Free("free" *> `<expr>`)
      <|> Return("return" *> `<expr>`)
      <|> Exit("exit" *> `<expr>`)
      <|> Print("print" *> `<expr>`)
      <|> Println("println" *> `<expr>`)
      <|> If(
        "if" *> `<expr>`,
        "then" *> sepBy1(`<stat>`, ";"),
        "else" *> sepBy1(`<stat>`, ";") <* "fi"
      )
      <|> While(
        "while" *> `<expr>`,
        "do" *> sepBy1(`<stat>`, ";") <* "done"
      )
      <|> Scope(
        "begin" *> sepBy1(`<stat>`, ";") <* "end"
      ))
      .label("statement")
      .explain(
        "Examples of statements are new variables, " +
          "print instructions and the start of while or if expressions"
      )

  private lazy val `<assign-lhs>` = `<array-ident>` <|> `<pair-elem>`

  private lazy val `<assign-rhs>` =
    (`<expr>`
      <|> `<array-liter>`
      <|> NewPair("newpair" *> "(" *> `<expr>` <* ",", `<expr>` <* ")")
      <|> `<pair-elem>`
      <|> Call("call" *> `<ident>`, "(" *> sepBy(`<expr>`, ",") <* ")")
        .label("call to a function"))

  private lazy val `<pair-elem>` =
    Fst("fst" *> `<expr>`) <|> Snd("snd" *> `<expr>`)

  private lazy val `<type>` : Parsley[Type] = chain
    .postfix(`<base-type>` <|> `<pair-type>`, ArrayType <# ("[" <* "]"))
    .label("type")
    .explain(
      "A type ca be a array, pair, integer, boolean, character or string."
    )

  private lazy val `<base-type>` = (IntType <# "int") <|>
    (BoolType <# "bool") <|>
    (CharType <# "char") <|>
    (StringType <# "string")

  private lazy val `<pair-type>` : Parsley[PairType] = PairType(
    "pair" *> "(" *> `<pair-elem-type>` <* ",",
    `<pair-elem-type>` <* ")"
  )

  // format: off
  private lazy val `<expr>` : Parsley[Expr] =
    precedence(
      SOps(InfixL)(Or <# "||".label("operator")) +:
        SOps(InfixL)(And <# "&&".label("operator")) +:
        SOps(InfixL)(
          Eq <# "==".label("operator"),
          Neq <# "!=".label("operator")
        ) +:
        SOps(InfixL)(
          Lt <# "<".label("operator"),
          Leq <# "<=".label("operator"),
          Gt <# ">".label("operator"),
          Geq <# ">=".label("operator")
        ) +:
        SOps(InfixL)(
          Add <# "+".label("operator"),
          Sub <# "-".label("operator")
        ) +:
        SOps(InfixL)(
          Mul <# "*".label("operator"),
          Div <# "/".label("operator"),
          Mod <# "%".label("operator")
        ) +:
        SOps(Prefix)(
          Neg <# NEG,
          Not <# "!".label("operator"),
          Len <# "len".label("operator"),
          Ord <# "ord".label("operator"),
          Chr <# "chr".label("operator")
        ) +:
        `<expr0>`
    ).label("expression").explain("Examples of expressions are integers, booleans, characters, strings and applications of operators")
  // format: on

  private lazy val `<expr0>` = Atoms(
    IntLiter(INT),
    BoolLiter(BOOL),
    CharLiter(CHAR),
    StrLiter(STRING),
    Null <# "null",
    `<array-ident>`,
    Paren("(" *> `<expr>` <* ")")
  )

  private lazy val `<array-liter>` = ArrayLiter(
    ("[" *> sepBy(`<expr>`, ",") <* "]").label("array literal")
  )

  private lazy val `<ident>` = Ident(ID)

  private lazy val `<array-ident>` =
    chain.postfix(`<ident>`, ArrayElem <# "[" <*> `<expr>` <* "]")

  private lazy val `<pair-elem-type>` : Parsley[PairElemType] = attempt(
    chain.postfix1(`<base-type>` <|> `<pair-type>`, ArrayType <# ("[" <* "]"))
  ) <|> `<base-type>` <|> (NestedPairType <# "pair")

  // defined for all which fail!
  private val functions_return = new PartialFunction[List[Stat], String] {
    def apply(x: List[Stat]) = "All functions must end in a return statement"
    def isDefinedAt(x: List[Stat]): Boolean = {
      x.last match {
        case Return(_)         => false
        case Exit(_)           => false
        case While(_, doStats) => isDefinedAt(doStats)
        case If(_, thenStats, elseStats) =>
          isDefinedAt(thenStats) || isDefinedAt(elseStats)
        case Scope(stats) => isDefinedAt(stats)
        case _            => true
      }
    }
  }
}

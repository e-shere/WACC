package frontend

import lexer._
import implicits.implicitToken
import ast._
import parsley.Parsley, parsley.Parsley._
import parsley.expr.{InfixL, Prefix, SOps, precedence, Atoms}
import parsley.implicits.character.{charLift}
import scala.language.implicitConversions
import parsley.combinator.{sepBy, sepBy1, many, some}
import parsley.errors.ErrorBuilder
import parsley.Result
import parsley.debug._

object parser {

    def parse[Err: ErrorBuilder](input: String): Result[Err, WaccProgram] = `<program>`.parse(input)

    private lazy val `<program>` = fully(WaccProgram("begin" *> many(`<func>`), sepBy1(`<stat>`, ";") <* "end")).debug("WaccProgram")

    private lazy val `<func>` = attempt(Func(`<type>`, `<ident>`, "(" *> sepBy(`<param>`, ",") <* ")", "is" *> sepBy1(`<stat>`, ";") <* "end").debug("Func"))

    private lazy val `<param>` = Param(`<type>`, `<ident>`).debug("Param")

    private lazy val `<stat>`: Parsley[Stat] =
           (Skip <# "skip".debug("Skip")
        <|> Declare(`<type>`, `<ident>`, "=" *> `<assign-rhs>`).debug("Declare")
        <|> Assign(`<assign-lhs>`, "=" *> `<assign-rhs>`).debug("Assign")
        <|> Read("read" *> `<assign-lhs>`).debug("Read")
        <|> Free("free" *> `<expr>`).debug("Free")
        <|> Return("return" *> `<expr>`).debug("Return")
        <|> Exit("exit" *> `<expr>`).debug("Exit")
        <|> Print("print" *> `<expr>`).debug("Print")
        <|> Println("println" *> `<expr>`).debug("Println")
        <|> If("if" *> `<expr>`, "then" *> sepBy1(`<stat>`, ";"), "else" *> sepBy1(`<stat>`,";") <* "fi").debug("If")
        <|> While("while" *> `<expr>`, "do" *> sepBy1(`<stat>`, ";") <* "done").debug("While")
        <|> Scope("begin" *> sepBy1(`<stat>`,";") <* "end").debug("Scope")
    ).debug("Stat")

    private lazy val `<assign-lhs>` = `<ident>` <|> `<array-elem>` <|> `<pair-elem>`

    private lazy val `<assign-rhs>` = 
           (`<expr>`
        <|> ArrayElem(`<ident>`, "[" *> some(`<expr>`) <* "]")
        <|> NewPair("newpair" *> "(" *> `<expr>` <* ",", `<expr>` <* ")")
        <|> `<pair-elem>`
        <|> Call("call" *> `<ident>`, "(" *> sepBy(`<expr>`, ",") <* ")"))

    private lazy val `<pair-elem>` = Fst("fst" *> `<expr>`) <|> Snd("snd" *> `<expr>`)

    private lazy val `<type>`: Parsley[Type] = `<base-type>` <|> /*`<array-type>` <|>*/ `<pair-type>`

    private lazy val `<base-type>` = (IntType <# "int") <|>
                                      (BoolType <# "bool") <|>
                                      (CharType <# "char") <|>
                                     (StringType <# "string")


    private lazy val `<array-type>` = ArrayType(`<type>` <* "[" <* "]")

    private lazy val `<pair-type>` = PairType("pair" *> "(" *> `<pair-elem-type>` <* ",", `<pair-elem-type>` <* ")")

    private lazy val `<expr>`: Parsley[Expr] =
        precedence(SOps(InfixL)(Or  <# "||") +:
                SOps(InfixL)(And <# "&&") +:
                SOps(InfixL)(Eq  <# "==", Neq <# "!=") +:
                SOps(InfixL)(Lt  <# "<",  Leq <# "<=",
                             Gt  <# ">",  Geq <# ">=") +:
                SOps(InfixL)(Add <# "+",  Sub <# "-") +:
                SOps(InfixL)(Mul <# "*",  Div <# "/", Mod <# "%") +:
                SOps(Prefix)(Neg <# "-",  Not <# "!", Len <# "len", Ord <# "ord", Chr <# "chr") +:
                `<expr0>`)


    private lazy val `<expr0>` = Atoms(
        IntLiter(INT),
        BoolLiter(BOOL),
        CharLiter(CHAR),
        StrLiter(STRING),
        Null <# "null",
        `<ident>`,
        `<array-elem>`,
        Paren("(" *> `<expr>` <* ")")
    )

    private lazy val `<ident>` = Ident(ID)

    private lazy val `<array-elem>` = ArrayElem(`<ident>`, some("[" *> `<expr>` <* "]"))

    private lazy val `<pair-elem-type>` = `<base-type>` <|> `<array-type>` <|> (NestedPairType <# "pair")


}
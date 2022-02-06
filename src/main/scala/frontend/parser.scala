package frontend

import lexer._
import implicits.implicitToken
import ast._
import parsley.Parsley
import parsley.Parsley._
import parsley.expr.{Atoms, InfixL, Prefix, SOps, precedence}
import parsley.implicits.character.charLift

import scala.language.implicitConversions
import parsley.combinator.{many, sepBy, sepBy1, some}
import parsley.errors.ErrorBuilder
import parsley.Result
import parsley.debug._
import parsley.io.ParseFromIO

import java.io.File
import parsley.expr.chain

object parser {

    def parse[Err: ErrorBuilder](input: File): Result[Err, WaccProgram] = `<program>`.parseFromFile(input).get

    private lazy val `<program>` = fully(WaccProgram("begin" *> many(`<func>`), sepBy1(`<stat>`, ";") <* "end"))

    private lazy val `<func>` = attempt(Func(`<type>`, `<ident>`, "(" *> sepBy(`<param>`, ",") <* ")", "is" *> sepBy1(`<stat>`, ";") <* "end"))

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
        <|> If("if" *> `<expr>`, "then" *> sepBy1(`<stat>`, ";"), "else" *> sepBy1(`<stat>`,";") <* "fi")
        <|> While("while" *> `<expr>`, "do" *> sepBy1(`<stat>`, ";") <* "done")
        <|> Scope("begin" *> sepBy1(`<stat>`,";") <* "end")
    )

    private lazy val `<assign-lhs>` = `<ident>` <|> `<array-elem>` <|> `<pair-elem>`

    private lazy val `<assign-rhs>` = 
           (`<expr>`
        <|> `<array-liter>`
        <|> NewPair("newpair" *> "(" *> `<expr>` <* ",", `<expr>` <* ")")
        <|> `<pair-elem>`
        <|> Call("call" *> `<ident>`, "(" *> sepBy(`<expr>`, ",") <* ")"))

    private lazy val `<pair-elem>` = Fst("fst" *> `<expr>`) <|> Snd("snd" *> `<expr>`)

    private lazy val `<type>`: Parsley[Type] = chain.postfix(`<base-type>` <|> `<pair-type>`, ArrayType <# ("[" <* "]"))

    private lazy val `<base-type>` = (IntType <# "int") <|>
                                      (BoolType <# "bool") <|>
                                      (CharType <# "char") <|>
                                     (StringType <# "string")



    private lazy val `<pair-type>`: Parsley[PairType] = PairType("pair" *> "(" *> `<pair-elem-type>` <* ",", `<pair-elem-type>` <* ")")

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
        attempt(`<ident>`),
        `<array-elem>`,
        Paren("(" *> `<expr>` <* ")")
    )

    private lazy val `<array-liter>` = ArrayLiter("[" *> sepBy1(`<expr>`, ",") <* "]")

    private lazy val `<ident>` = Ident(ID)

    private lazy val `<array-elem>` = ArrayElem(`<ident>`, some("[" *> `<expr>` <* "]"))

    private lazy val `<pair-elem-type>`: Parsley[PairElemType] = attempt(chain.postfix1(`<base-type>` <|> `<pair-type>`, ArrayType <# ("[" <* "]"))) <|> `<base-type>` <|> (NestedPairType <# "pair")


}
package frontend

import lexer._
import implicits.implicitToken
import ast._
import parsley.Parsley, parsley.Parsley._
import parsley.expr.{InfixL, Prefix, SOps, precedence}
import parsley.implicits.character.{charLift}
import scala.language.implicitConversions
import parsley.expr.Atoms
import parsley.combinator.{sepBy, sepBy1, many, some}

object parser {

    private lazy val `<program>` = "begin" *> WaccProgram(many(`<func>`), sepBy1(`<stat>`, ',')) <* "end"

    private lazy val `<func>` = Func(`<type>`, `<ident>`, sepBy(`<param>`, ','), sepBy1(`<stat>`, ','))

    private lazy val `<param>` = Param(`<type>`, `<ident>`)

    private lazy val `<stat>` = Skip <# "skip"
                                <|> Declare(`<type>`, `<ident>`, '=' *> `<assign-rhs>`)
                                <|> Assign(`<assign-lhs>`, '=' *> `<assign-rhs>`)
                                <|> Read("read" *> `<assign-lhs>`)
                                <|> Free("free" *> `<expr>`)
                                <|> Return("return" *> `<expr>`)
                                <|> Exit("exit" *> `<expr>`)
                                <|> Print("print" *> `<expr>`)
                                <|> Println("println" *> `<expr>`)
                                <|> If("if" *> `<expr>`, "then" *> sepBy1(`<stat>`, ';'), "else" *> sepBy1(`<stat>`,';') <* "fi")
                                <|> While("while" *> `<expr>`, "do" *> sepBy1(`<stat>`, ';') <* "done")
                                <|> Scope("begin" *> sepBy1(`<stat>`,';') <* "end")

    private lazy val `<assign-lhs>` = `<ident>` <|> `<array-elem>` <|> `<pair-elem>`

    private lazy val `<assign-rhs>` = `<expr>`
                                        <|> ArrayElem(`<ident>`, '[' *> some(`<expr>`) <* ']')
                                        <|> NewPair("newpair" *> '(' *> `<expr>` <* ',', `<expr>` <* ')')
                                        <|> `<pair-elem>`
                                        <|> Call("call" *> `<ident>`, '(' *> sepBy(`<expr>`, ',') <* ')')

    private lazy val `<pair-elem>` = Fst("fst" *> `<expr>`) <|> Snd("snd" *> `<expr>`)

    private lazy val `<type>` = (IntType <# "int")
                                <|> (BoolType <# "bool")
                                <|> (CharType <# "char")
                                <|> (StringType <# "string")
                                <|> (ArrayType(`<type>` <* '[' <* ']'))
                                <|> (PairType("pair" *> '(' *> `<PairElemType>` <* ',', `<PairElemType>` <* ')'))
                                <|> (NestedPairType <# "pair")

    private lazy val `<expr>`: Parsley[Expr] =
        precedence(SOps(InfixL)(Or  <# "||" ) +:
                   SOps(InfixL)(And <# "&&") +:
                   SOps(InfixL)(Eq  <# "==", Neq <# "!=") +:
                   SOps(InfixL)(Lt  <# "<",  Leq <# "<=",
                                Gt  <# ">",  Geq <# ">=") +:
                   SOps(InfixL)(Add <# "+",  Sub <# "-") +:
                   SOps(InfixL)(Mul <# "*",  Div <# "/", Mod <# "%") +:
                   SOps(Prefix)(Neg <# "-",  Not <# "!",
                                Len <# "len", Ord <# "ord",
                                Chr <# "chr") +:
                   `<expr0>`)

    private lazy val `<expr0>` = Atoms(
        IntLiter(INT),
        BoolLiter(BOOL),
        CharLiter(CHAR),
        StrLiter(STRING),
        Null <# "null",
        `<ident>`,
        ArrayElem(`<ident>`, some('[' *> `<expr>` <* ']')),
        Paren('(' *> `<expr>` <* ')')
    )

    private lazy val `<ident>` = Ident(ID)


}
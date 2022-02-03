package frontend

import lexer._
import ast._
import parsley.Parsley, parsley.Parsley._
import parsley.expr.{InfixL, Prefix, SOps, precedence}
import parsley.implicits.character.{charLift, stringLift}
import parsley.expr.Atoms

//import lexer.implicits.implicitToken
//import parsley.errors.ErrorBuilder
//import parsley.Result
//import parsley.debug._
//import parsley.combinator.{sepBy1, many, some}

object parser {
    
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
        
    )


}
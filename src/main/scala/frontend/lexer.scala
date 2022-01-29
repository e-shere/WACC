package frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.character.{digit, isWhitespace}
import parsley.combinator.eof
import parsley.errors.combinator.ErrorMethods
import parsley.token.{LanguageDef, Lexer, Predicate}
import parsley.implicits.character.stringLift

import scala.language.implicitConversions

object lexer {
  private val wacc = LanguageDef.plain.copy(
    commentLine = "#",
    keywords = Set("begin", "end", "skip", "read", "free", "return", "exit", "print", "println",
                    "if", "then", "else", "fi", "while", "do", "done", "newpair", "call",
                    "fst", "snd", "int", "bool", "char", "string", "pair", "true", "false", "null"),
    operators = Set("!", "-", "len", "ord", "chr", "*", "/", "%", "+",
                    ">", "<", ">=", "<=", "==", "!=", "&&", "||"),
    identStart = Predicate(c => c.isLetter || c == '_'),
    identLetter = Predicate(c => c.isLetterOrDigit || c == '_'),
    space = Predicate(isWhitespace)
  )

  val lexer = new Lexer(wacc)

  // Identifiers for variables and functions
  val ID = lexer.identifier.filterOut {
    case k if wacc.keywords(k) => s"keyword $k cannot be used as an identifier"
  }

  val NAT = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
  // TODO: negative numbers - sequence an optional +/- before the digits
  val INT = ???
  val BOOL = "true" #> true <|> "false" #> false
  val CHAR = lexer.charLiteral
  val STRING = ???

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  object implicits {
    implicit def implicitToken(s: String): Parsley[Unit] = {
      if (wacc.keywords(s))       lexer.keyword(s)
      else if (wacc.operators(s)) lexer.maxOp(s)
      // check why we need symbol_ not symbol
      else                        void(lexer.symbol_(s))
    }
  }
}

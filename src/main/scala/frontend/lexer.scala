package frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.character.{digit, isWhitespace, noneOf}
import parsley.combinator.{choice, eof, many, optionally}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.{charLift, stringLift}
import parsley.token.{LanguageDef, Lexer, Predicate}

import scala.language.implicitConversions

object lexer {
  private val wacc = LanguageDef.plain.copy(
    commentLine = "#",
    // format: off
    keywords = Set("begin", "end", "skip", "read", "free", "return", "exit", "print", "println",
      "if", "then", "else", "fi", "while", "do", "done", "newpair", "call", "fst", "snd", "int",
      "bool", "char", "string", "pair", "true", "false", "null", "len", "ord", "chr"),
    operators = Set("!", "-", "len", "ord", "chr", "*", "/", "%", "+", ">", "<", ">=", "<=", "==",
      "!=", "&&", "||"),
    // format: on
    identStart = Predicate(c => c.isLetter || c == '_'),
    identLetter = Predicate(c => c.isLetterOrDigit || c == '_'),
    space = Predicate(isWhitespace)
  )
  private val lexer = new Lexer(wacc)

  // Identifiers for variables and functions
  val ID: Parsley[String] = lexer.identifier
  private def token[A](p: => Parsley[A]): Parsley[A] = {
    lexer.lexeme(attempt(p))
  }

  val NEG: Parsley[Unit] = token('-' *> notFollowedBy(digit))

  private val minus: Parsley[BigInt => BigInt] = '-' #> { x: BigInt => -x }
  private val plus: Parsley[BigInt => BigInt] = optionally('+', identity)

  private val checkOverflow: PartialFunction[BigInt, String] = {
    case x if !x.isValidInt =>
      "Integer overflow occurred, A valid integer is in the range [-2^31, 2^31 - 1]"
  }

  private val nat = digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)
  private val bigInt = token((minus <|> plus) <*> nat).filterOut(checkOverflow)
  val INT: Parsley[Int] = bigInt
    .map(_.intValue)
    .label("integer")

  val BOOL: Parsley[Boolean] =
    token("true" #> true <|> "false" #> false).label("boolean")

  private val escapeChar =
    choice(
      '0' #> '\u0000',
      'b' #> '\b',
      't' #> '\t',
      'n' #> '\n',
      'f' #> '\f',
      'r' #> '\r',
      '\"',
      '\'',
      '\\'
    )
      .label("end of escape sequence")
      .explain(
        "valid escape characters include \\0, \\b, \\t, \\n, \\f, \\r, \\\\,\\\" and \\\'"
      )
  private val charLetter = noneOf('\\', '\'', '\"').label(
    "string character"
  ) <|> ('\\' *> escapeChar).label("escape character")
  val CHAR: Parsley[Char] =
    token('\'' *> charLetter <* '\'').label("character literal")

  val STRING: Parsley[String] = token(
    '\"' *> many(charLetter).map(_.mkString) <* '\"'
  ).label("string literal")

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  object implicits {
    implicit def implicitToken(s: String): Parsley[Unit] = {
      if (wacc.keywords(s)) lexer.keyword(s)
      else if (wacc.operators(s)) lexer.maxOp(s)
      else void(lexer.symbol_(s))
    }
  }
}

package frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.character.{digit, isWhitespace, noneOf, oneOf}
import parsley.combinator.{choice, eof, many, optionally}
import parsley.implicits.character.{charLift, stringLift}
import parsley.token.{LanguageDef, Lexer, Predicate}

import scala.language.implicitConversions

object lexer {

  
  private val wacc = LanguageDef.plain.copy(
    commentLine = "#",
    // @formatter:off
    keywords = Set("begin", "end", "skip", "read", "free", "return", "exit", "print", "println",
                    "if", "then", "else", "fi", "while", "do", "done", "newpair", "call",
                    "fst", "snd", "int", "bool", "char", "string", "pair", "true", "false", "null"),
    operators = Set("!", "-", "len", "ord", "chr", "*", "/", "%", "+",
                    ">", "<", ">=", "<=", "==", "!=", "&&", "||"),
    // @formatter:on
    identStart = Predicate(c => c.isLetter || c == '_'),
    identLetter = Predicate(c => c.isLetterOrDigit || c == '_'),
    space = Predicate(isWhitespace)
  )
  private val lexer = new Lexer(wacc)
  
  // Identifiers for variables and functions
  val ID = lexer.identifier
  private def token[A](p: =>Parsley[A]): Parsley[A] = {
    lexer.lexeme(attempt(p))
  }

  // include negation token then negation legal only if it is NOT immediately followed by an integer
  // ie needs to consume whitespace/bracket/smth not an integer
  // same for plus
  val NEG = token('-' *> notFollowedBy(digit))

  private val minus: Parsley[Int => Int] = '-' #> {x: Int => -x}
  private val plus: Parsley[Int => Int] = optionally('+', identity)
  private val nat = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
  // need to make sign optional
  val INT = token((minus <|> plus) <*> nat)

  val BOOL = token("true" #> true <|> "false" #> false)

  private val escapeChar =
    choice('0' #> '\0', 'b' #> '\b', 't' #> '\t', 'f' #> '\f', 'r' #> '\r', '\"', '\'', '\\')
  private val charletter = noneOf('\\', '\'', '\"') <|> ('\\' *> escapeChar)
  val CHAR = token('\'' *> charletter <* '\'')

  val STRING = token('\"' *> many(charletter).map(_.mkString) <* '\"')

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  object implicits {
    implicit def implicitToken(s: String): Parsley[Unit] = {
      if (wacc.keywords(s)) lexer.keyword(s)
      else if (wacc.operators(s)) lexer.maxOp(s)
      else void(lexer.symbol_(s))
    }
  }
}

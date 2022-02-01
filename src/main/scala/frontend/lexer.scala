package frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.character.{digit, isWhitespace, noneOf, oneOf}
import parsley.combinator.{eof, optional, many}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.{charLift, stringLift}
import parsley.token.{LanguageDef, Lexer, Predicate}
import scala.language.implicitConversions

object lexer {
  // Identifiers for variables and functions
  val ID = lexer.identifier
  private def token[A](p: =>Parsley[A]): Parsley[A] = {
    lexer.lexeme(attempt(p))
  }

  // include negation token then negation legal only if it is NOT immediately followed by an integer
  // ie needs to consume whitespace/bracket/smth not an integer
  // same for plus
  val NEG = ??? // all of type Parsley[Int] -> Parsley[Int]
  private val MINUS = ???
  private val PLUS = ???

  private val NAT = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
  // make the sign be a a Parsley[Int] -> Parsley[Int] and ap it
  val INT = token(optional(oneOf('+','-')) <*> NAT)
  val BOOL = token("true" #> true <|> "false" #> false)

  private val escapeChar = oneOf('0', 'b', 't', 'f', 'r', '\"', '\'', '\\')
  // for things which can be in the string or escape characters
  private val charletter = token(noneOf('\\', '\'', '\"') <|> ('\\' *> escapeChar)) 
  val CHAR = token('\'' *> charletter <* '\'')
  val STRING = token('\"' *> many(charletter).map(_.mkString) <* '\"')

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

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  object implicits {
    implicit def implicitToken(s: String): Parsley[Unit] = {
      if (wacc.keywords(s)) lexer.keyword(s)
      else if (wacc.operators(s)) lexer.maxOp(s)
      // check why we need symbol_ not symbol
      else void(lexer.symbol_(s))
    }
  }
}

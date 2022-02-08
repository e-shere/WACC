package frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.character.{digit, isWhitespace, noneOf}
import parsley.combinator.{choice, eof, many, manyUntil, optionally, some}
import parsley.implicits.character.{charLift, stringLift}
import parsley.token.{LanguageDef, Lexer, Predicate}

import scala.language.implicitConversions
import parsley.errors.combinator.ErrorMethods

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
  private val lexer = new Lexer(wacc)
  
  // Identifiers for variables and functions
  val ID: Parsley[String] = lexer.identifier
  private def token[A](p: =>Parsley[A]): Parsley[A] = {
    lexer.lexeme(attempt(p))
  }

  val NEG: Parsley[Unit] = token('-' *> notFollowedBy(digit))

  private val minus: Parsley[Long => Long] = '-' #> {x: Long => -x}
  private val plus: Parsley[Long => Long] = optionally('+', identity)

  private val overflowLength: PartialFunction[String, String] = {
    case x if x.length > Int.MaxValue.toString.length => "Integer overflow occurred"
  }
  private val overflowVal: PartialFunction[Long, String] = {
    case x if (x > Int.MaxValue.toLong) => "Integer overflow occurred (+)"
    case x if (x < Int.MinValue.toLong) => "Integer overflow occurred (-)"
  }
  private val intString =  manyUntil('0',notFollowedBy('0' *> '0')) *> some(digit).map(_.mkString).filterOut(overflowLength)
  private val intLong = ((minus <|> plus) <*> intString.map(_.toLong)).filterOut(overflowVal)
  val INT: Parsley[Int] = token(intLong.map(_.toInt))

  val BOOL: Parsley[Boolean] = token("true" #> true <|> "false" #> false)

  private val escapeChar =
    choice('0' #> '\u0000', 'b' #> '\b', 't' #> '\t', 'n' #> '\n', 'f' #> '\f', 'r' #> '\r', '\"', '\'', '\\')
  private val charLetter = noneOf('\\', '\'', '\"') <|> ('\\' *> escapeChar)
  val CHAR: Parsley[Char] = token('\'' *> charLetter <* '\'')

  val STRING: Parsley[String] = token('\"' *> many(charLetter).map(_.mkString) <* '\"')

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  object implicits {
    implicit def implicitToken(s: String): Parsley[Unit] = {
      if (wacc.keywords(s)) lexer.keyword(s)
      else if (wacc.operators(s)) lexer.maxOp(s)
      else void(lexer.symbol_(s))
    }
  }
}

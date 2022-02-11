package frontend

import ast._
import parsley.errors.ErrorBuilder


object Errors {

  def format(pos: String, source: Option[String], lines: Seq[String], error: WaccError): String = {
    s"$error error \n${source.fold("")(name => s"In $name ")}$pos:\n${lines.mkString("  ", "\n  ", "")}"
  }

  def vanillaError(unexpected: Option[String], expected: Option[String], reasons: Seq[String], lines: Seq[String]): Seq[String] = {
    Seq(s"${unexpected.getOrElse("")}, ${expected.getOrElse("")}", s"${reasons}", s"${lines}")
  }

  def specialisedError(msgs: Seq[String], lines: Seq[String]): Seq[String] = {
    Seq(s"${msgs.mkString("\n")}", s"${lines.mkString("\n")}")
  }

  sealed trait WaccErrorLines {
    val errorType: String
    val lines: Seq[String]

  }

  case class WaccError(pos: (Int, Int), file: String, errorLines: WaccErrorLines) {
    override def toString: String = {
      s"""${errorLines.errorType}:
        |in file $file at line ${pos._1}, column ${pos._2}
        |${errorLines.lines.mkString("\n")}
      """.stripMargin
    }
  }

  case class SyntaxError(unexpected: String, expected: String, reasons: Seq[String]) extends WaccErrorLines {
    override val errorType = "Syntax Error"
    override val lines: Seq[String] = "unexpected: " + unexpected :: "expected: " + expected :: reasons.toList
  }

  sealed trait SemanticError extends WaccErrorLines {
    override val errorType = "Semantic Error"
  }

  case class TypeError(place: String, expectedTypes: Set[Type], foundType: Type) extends SemanticError {
    private val expectedString = expectedTypes.toList match {
      case List(ty) => ty.toTypeName
      case types@_ => "one of " + types.map(_.toTypeName).mkString(", ")
    }

    override val lines = Seq(s"Type mismatch in $place : ",
                              s"Expected $expectedString",
                              s"found $foundType")
  }

  case class UndefinedFunctionError(id: Ident) extends SemanticError {
    override val lines = Seq(s"Undefined function ${id.id}")
  }

  case class UndefinedVariableError(id: Ident) extends SemanticError {
    override val lines = Seq(s"Undefined variable ${id.id}")
  }

  case class RedefinedFunctionError(id: Ident) extends SemanticError {
    override val lines = Seq(s"Duplicate function declaration ${id.id}")
  }

  case class RedefinedVariableError(id: Ident) extends SemanticError {
    override val lines = Seq(s"Variable ${id.id} defined twice in same scope")
  }


  class WaccErrorBuilder extends ErrorBuilder[WaccError] {
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): WaccError = WaccError(pos, source, lines)

    override type Position = (Int, Int)
    override type Source = String

    override def pos(line: Int, col: Int): Position = (line, col)

    override def source(sourceName: Option[String]): Source = sourceName.getOrElse("")

    override type ErrorInfoLines = WaccErrorLines

    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = SyntaxError(unexpected.getOrElse(""), expected.getOrElse(""), reasons)

    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SyntaxError("", "", msgs)

    override type ExpectedItems = Option[String]
    override type Messages = Seq[Message]

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = if (alts.isEmpty) None else Some(alts.mkString(", "))

    override def combineMessages(alts: Seq[Message]): Messages = alts

    override type UnexpectedLine = Option[String]
    override type ExpectedLine = Option[String]
    override type Message = String
    override type LineInfo = Seq[String]

    override def unexpected(item: Option[Item]): UnexpectedLine = item

    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def reason(reason: String): Message = reason

    override def message(msg: String): Message = msg

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int): LineInfo = {
        linesBefore.map(line => s">$line") ++:
          Seq(s">$line", s" ${" " * errorPointsAt}") ++:
          linesAfter.map(line => s">$line")
    }

    override val numLinesBefore: Int = 1
    override val numLinesAfter: Int = 1
    override type Item = String
    override type Raw = String
    override type Named = String
    override type EndOfInput = String

    override def raw(item: String): Raw =
      item match {
        case cs if cs.head.isWhitespace => cs.head match {
          case c if c.isSpaceChar  => "space"
          case '\n'                => "newline"
          case '\t'                => "tab"
          case _                   => "whitespace character"
        }
        case cs              => "\"" + cs.takeWhile(!_.isWhitespace) + "\""
      }

    override def named(item: String): Named = item

    override val endOfInput: EndOfInput = "end of input"
  }

}

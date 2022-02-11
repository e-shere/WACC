package frontend

import frontend.ast._
import parsley.errors.ErrorBuilder

object Errors {

  case class LineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int
  ) {
    def toSeq: Seq[String] = {
      linesBefore.map(line => s">$line") ++:
        Seq(s">$line", s"${" " * errorPointsAt}^") ++:
        linesAfter.map(line => s">$line")
    }
  }

  object LineInfo{
    def from(pos: (Int, Int))(implicit fileLines: Array[String]): LineInfo = pos match {
      case (line, col) => LineInfo(
        fileLines(line), 
        if (line > 0) Seq(fileLines(line - 1)) else Nil,
        if (line < fileLines.length - 1) Seq(fileLines(line + 1)) else Nil,
        col
      )
    }
  }

  sealed trait WaccErrorLines {
    val errorType: String
    val lines: Seq[String]
    val lineInfo: LineInfo
  }

  case class WaccError(
      pos: (Int, Int),
      file: String,
      errorLines: WaccErrorLines
  ) {
    override def toString: String = {
      s"""${errorLines.errorType}:
        |in file $file at line ${pos._1}, column ${pos._2}
        |${errorLines.lines.mkString("\n")}
        |${errorLines.lineInfo.toSeq.mkString("\n")}
      """.stripMargin
    }
  }

  case class SyntaxError(
      unexpected: Option[String],
      expected: Option[String],
      reasons: Seq[String],
      lineInfo: LineInfo
  ) extends WaccErrorLines {
    override val errorType = "Syntax Error"
    override val lines: Seq[String] = (unexpected, expected) match {
      case (None, None) => reasons.toList
      case _ =>
        "unexpected: " + unexpected.getOrElse("") :: "expected: " + expected
          .getOrElse("") :: reasons.toList
    }
  }

  sealed trait SemanticError extends WaccErrorLines {
    override val errorType = "Semantic Error"
  }

  case class TypeError(
      place: String,
      expectedTypes: Set[Type],
      foundType: Type,
      lineInfo: LineInfo
  ) extends SemanticError {
    private val expectedString = expectedTypes.toList match {
      case List(ty)  => ty.toTypeName
      case types @ _ => "one of " + types.map(_.toTypeName).mkString(", ")
    }

    override val lines = Seq(
      s"Type mismatch in $place : ",
      s"Expected $expectedString",
      s"found $foundType"
    )
  }

  case class UndefinedFunctionError(id: Ident, lineInfo: LineInfo)
      extends SemanticError {
    override val lines = Seq(s"Undefined function ${id.id}")
  }

  case class UndefinedVariableError(id: Ident, lineInfo: LineInfo)
      extends SemanticError {
    override val lines = Seq(s"Undefined variable ${id.id}")
  }

  case class RedefinedFunctionError(id: Ident, lineInfo: LineInfo)
      extends SemanticError {
    override val lines = Seq(s"Duplicate function declaration ${id.id}")
  }

  case class RedefinedVariableError(id: Ident, lineInfo: LineInfo)
      extends SemanticError {
    override val lines = Seq(s"Variable ${id.id} defined twice in same scope")
  }

  case class NullExceptionError(place: String, lineInfo: LineInfo)
      extends SemanticError {
    override val lines = Seq(s"Unexpected null in $place")
  }

  case class NumOfArgsError(
      funcId: Ident,
      expected: Int,
      found: Int,
      lineInfo: LineInfo
  ) extends SemanticError {
    override val lines = Seq(
      s"Incorrect number of arguments in call to ${funcId.id}",
      s"Expected $expected, found $found"
    )
  }

  case class MisplacedReturnError(lineInfo: LineInfo) extends SemanticError {
    override val lines = Seq("Cannot return from outside a function")
  }

  object TypeError {
    def mkError(
      place: String,
      expectedTypes: Set[Type],
      foundType: Type,
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(foundType.pos, file, new TypeError(place, expectedTypes, foundType, LineInfo.from(foundType.pos)))
    }
  }

  object UndefinedFunctionError {
    def mkError(
      id: Ident
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(id.pos, file, new UndefinedFunctionError(id, LineInfo.from(id.pos)))
    }
  }

  object UndefinedVariableError {
    def mkError(
      id: Ident
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(id.pos, file, new UndefinedVariableError(id, LineInfo.from(id.pos)))
    }
  }

  object RedefinedFunctionError {
    def mkError(
      id: Ident
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(id.pos, file, new RedefinedFunctionError(id, LineInfo.from(id.pos)))
    }
  }

  object RedefinedVariableError {
    def mkError(
      id: Ident
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(id.pos, file, new RedefinedFunctionError(id, LineInfo.from(id.pos)))
    }
  }

  object NullExceptionError {
    def mkError(
      place: String,
      nullExpr: Expr,
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(nullExpr.pos, file, new NullExceptionError(place, LineInfo.from(nullExpr.pos)))
    }
  }

  object NumOfArgsError {
    def mkError(
      funcId: Ident,
      expected: Int,
      found: Int
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(funcId.pos, file, new NumOfArgsError(funcId, expected, found, LineInfo.from(funcId.pos)))
    }
  }

  object MisplacedReturnError {
    def mkError(
      returnStat: Stat
    )(implicit
      file: String,
      fileLines: Array[String]
    ): WaccError = {
      WaccError(returnStat.pos, file, new MisplacedReturnError(LineInfo.from(returnStat.pos)))
    }
  }

  class WaccErrorBuilder extends ErrorBuilder[WaccError] {
    override def format(
        pos: Position,
        source: Source,
        lines: ErrorInfoLines
    ): WaccError = WaccError(pos, source, lines)

    override type Position = (Int, Int)
    override type Source = String

    override def pos(line: Int, col: Int): Position = (line, col)

    override def source(sourceName: Option[String]): Source =
      sourceName.getOrElse("")

    override type ErrorInfoLines = WaccErrorLines

    override def vanillaError(
        unexpected: UnexpectedLine,
        expected: ExpectedLine,
        reasons: Messages,
        line: LineInfo
    ): ErrorInfoLines = SyntaxError(
      unexpected,
      expected,
      reasons,
      line
    )

    override def specialisedError(
        msgs: Messages,
        line: LineInfo
    ): ErrorInfoLines = SyntaxError(None, None, msgs, line)

    override type ExpectedItems = Option[String]
    override type Messages = Seq[Message]

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems =
      if (alts.isEmpty) None else Some(alts.mkString(", "))

    override def combineMessages(alts: Seq[Message]): Messages = alts

    override type UnexpectedLine = Option[String]
    override type ExpectedLine = Option[String]
    override type Message = String
    override type LineInfo = Errors.LineInfo

    override def unexpected(item: Option[Item]): UnexpectedLine = item

    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def reason(reason: String): Message = reason

    override def message(msg: String): Message = msg

    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        errorPointsAt: Int
    ): LineInfo = LineInfo(line, linesBefore, linesAfter, errorPointsAt)

    override val numLinesBefore: Int = 1
    override val numLinesAfter: Int = 1
    override type Item = String
    override type Raw = String
    override type Named = String
    override type EndOfInput = String

    override def raw(item: String): Raw =
      item match {
        case cs if cs.head.isWhitespace =>
          cs.head match {
            case c if c.isSpaceChar => "space"
            case '\n'               => "newline"
            case '\t'               => "tab"
            case _                  => "whitespace character"
          }
        case cs => "\"" + cs.takeWhile(!_.isWhitespace) + "\""
      }

    override def named(item: String): Named = item

    override val endOfInput: EndOfInput = "end of input"
  }

}

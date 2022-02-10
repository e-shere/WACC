package frontend

import frontend.semanticChecker.SemanticError
import parser._
import parsley.errors.{DefaultErrorBuilder, ErrorBuilder}

import java.io.File
import scala.io.Source
import parsley.{Failure, Success}

object Compiler {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("No source file specified!")
      System.exit(-1)
    }
    implicit val eb = new SyntaxErrorBuilder()

    val maybeAst = parse(new File(args(0)))
    maybeAst match {
      case Failure(err) => {
        println(err)
        sys.exit(100);
      }
      case Success(ast) => {
        semanticChecker.validateProgram(ast) match {
          case Nil => {
            println(ast)
            sys.exit(0)
          }
          case errors => {
            implicit val eb = new SemanticErrorBuilder(errors)
            println(eb.format("", Some(args(0)), Seq.empty))
            sys.exit(200)
          }
        }
      }
    }
  }

  class SyntaxErrorBuilder() extends DefaultErrorBuilder {
    override def format(pos: String, source: Option[String], lines: Seq[String]): String =
      "Syntax error found:\n" + super.format(pos, source, lines)
  }

  class SemanticErrorBuilder(errors: List[SemanticError]) extends DefaultErrorBuilder {
    override def format(pos: String, source: Option[String], lines: Seq[String]): String = {
      val file = source match {
        case None => ""
        case Some(source) => s"in file ${source.split('/').last} \n"
      }
      s"Semantic error(s) found $file" + errors.map(err => toPosition(err.pos) + " " + err.msg + "\n").mkString("")
    }
  }
  def toPosition(pos: (Int, Int)): String =
    s"(line ${pos._1}, column ${pos._2})"
}

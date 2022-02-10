package frontend

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
//    val source = Source.fromFile(args(0)).mkString

    //TODO: override any settings here for error builder
    implicit val eb = new DefaultErrorBuilder {
      override def format(pos: String, source: Option[String], lines: Seq[String]): String =
        "Syntax error found:\n" + super.format(pos, source, lines)
    }

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
            println(errors)
            sys.exit(200)
          }
        }
      }
    }
  }
}

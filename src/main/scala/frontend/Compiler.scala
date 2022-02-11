package frontend

import parser._
import java.io.File

import frontend.Errors.WaccErrorBuilder

import scala.io.Source
import parsley.{Failure, Success}

object Compiler {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("No source file specified!")
      System.exit(-1)
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
            println(errors.mkString("\n"))
            sys.exit(200)
          }
        }

      }
    }
  }
}

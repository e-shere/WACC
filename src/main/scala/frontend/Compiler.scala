package frontend

import parser._

import java.io.File
import scala.io.Source
import parsley.{Failure, Success}

object Compiler {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("No source file specified!")
      System.exit(-1)
    }
    val source = Source.fromFile(args(0)).mkString
    val maybeAst = parse(new File(args(0)))
    maybeAst match {
      case Failure(err) => {
        // TODO: change later. Is needed this way for now for our tests
        println(Failure(err))
        sys.exit(100);
      }
      case Success(ast) => {
        semanticChecker.validateProgram(ast) match {
          case Nil => {
            println(Success(ast))
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

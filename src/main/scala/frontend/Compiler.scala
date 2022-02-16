package frontend

import frontend.parser._
import parsley.{Failure, Success}

import java.io.File

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
        semanticChecker.validateProgram(ast, args(0)) match {
          case (typeTree, Nil) => {
            println(typeTree)
            sys.exit(0)
          }
          case (_, errors) => {
            println(errors.mkString("\n"))
            sys.exit(200)
          }
        }

      }
    }
  }
}

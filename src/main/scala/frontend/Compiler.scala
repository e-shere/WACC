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
    print(maybeAst)
    maybeAst match {
      case Failure(err) => {
        // todo: output syntax error
        sys.exit(100);
      }
      case Success(ast) => {
        //todo: semantic analysis
        sys.exit(0)
      }
    }
  }
}

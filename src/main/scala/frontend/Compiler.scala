package frontend

import backend.generator.genProgram
import frontend.parser._
import parsley.{Failure, Success}

import java.io.{File, PrintWriter}

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
          case Nil => {
            val filename = args(0).split("/").last.split("\\.").head + ".s"
            val pw = new PrintWriter(new File(filename))
            pw.write(".text\n\n.global main\n")
            pw.write(genProgram(ast).mkString("\n") + "\n")
            pw.close()
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

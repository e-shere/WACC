package frontend

import parser._
import scala.io.Source

object Compiler {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("No source file specified!")
      System.exit(-1)
    }
    val source = Source.fromFile(args(0)).mkString
    val ast = parse(source)
    println(ast)
  }
}
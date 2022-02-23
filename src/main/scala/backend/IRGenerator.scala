package backend

import frontend.ast._
import backend.intermediateRep._
import java.io._

object IRGenerator {
  type IRFunc = List[IRNode]

  def generateIR(ast: WaccProgram): List[IRFunc] = {
    List(DIRECTIVE("text\n"), DIRECTIVE("global main")) +:
    ast.funcs.map(f => generateFunc(f.id.id, f.body)) :+
      generateFunc("main", ast.stats :+ frontend.ast.Return(IntLiter(0)(NO_POS))(NO_POS))
  }

  def generateFunc(name: String, body: List[Stat]): IRFunc = {
    LABEL(name) +:
    START_FUNC() +:
    generateBlock(body) :+
    EXIT_FUNC() :+
    DIRECTIVE("ltorg")
  }

  def generateBlock(stats: List[Stat]): List[IRNode] = {
    stats match {
      case Nil => List.empty
      case stat :: tail => generateStat(stat) ++: generateBlock(tail)
    }
  }

  def generateStat(stat: Stat): List[IRNode] = {
    stat match {
      case Skip() => Nil
      case Declare(ty, id, rhs) => ???
      case Assign(lhs, rhs) => ???
      case Read(lhs) => ???
      case Free(expr) => ???
      case Return(expr) => {
        // todo: if expr is a literal, would rather use returnval(literal)
        val (v, nodes) = generateExpression(expr)
        // todo: need to lookup v in some register table
        nodes :+ RETURNREG("r0")
      }
      case Exit(_) => List(RETURNVALUE(0))
      case Print(expr) => ???
      case Println(expr) => ???
      case If(expr, thenStats, elseStats) => ???
      case While(expr, doStats) => ???
      case Scope(stats) => ???
    }
  }

  //Store result of expression in an extra variable
  // This variable is kept in the symbol table to be looked up later
  def generateExpression(expr: Expr): (String, List[IRNode]) = {
    expr match {
      case _ => ("", Nil)
    }
  }

  def generateCode(ast: WaccProgram, outputPath: String): Unit = {
    val rep = generateIR(ast)
    val pw = new PrintWriter(new File(outputPath + ".s"))
    rep.foreach(f => pw.write(f.mkString("\n\t") + "\n"))
    pw.close()
  }

}

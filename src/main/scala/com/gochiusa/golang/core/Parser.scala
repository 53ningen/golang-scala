package com.gochiusa.golang.core

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  protected val ifKeyword = "if"
  protected val elseKeyword = "else"
  protected val varKeyword = "var"
  protected val substituteKeyword = "="
  protected val whileKeyword = "while"
  protected val printKeyword = "println"


  /**
   * statements ::= statement | statement; statement
   * statement  ::= expression
   * expression ::=
   *
   * @return
   */

  // statement
  def stmts: Parser[AST] = chainl1(stmt|expr, ";" ^^ { _ => (left: AST, right: AST) => Statements(left, right)}) | stmt
  def stmt: Parser[AST] = assignStmt | ifStmt | printlnStmt
  def ifStmt: Parser[AST] = ifKeyword~expr~"{"~(expr|stmts)~"}"~elseKeyword~"{"~(expr|stmts)~"}" ^^ { case ifKeyword~cond~"{"~cons~"}"~elseKeyword~"{"~alt~"}" => IfStmt(cond, cons, alt) }

  def assignStmt: Parser[AST] = varKeyword~>ident~"="~expr ^^ { case id~_~exp => AssignStmt(id, exp) }
  def printlnStmt: Parser[AST] = printKeyword~"("~>expr~")" ^^ { case expr~_ => PrintlnStmt(expr) }

  // expression
  def expr: Parser[AST] = identifier | binOpExpr | relOpExp | primitive

  def relOpExp: Parser[AST] = (identifier|number) ~ ("<"|">"|"<="|">="|"=="|"!=") ~ (identifier|number) ^^ { case left ~ op ~ right => RelOpExpr(left, op, right)}

  def binOpExpr: Parser[AST] = additionOpExpr | multiplyOpExpr
  def additionOpExpr: Parser[AST] = chainl1(multiplyOpExpr, ("+"|"-")^^{ op => (left: AST, right: AST) => BinOpExpr(left, op, right)})
  def multiplyOpExpr: Parser[AST] = chainl1(number, ("*"|"/")^^{ op => (left: AST, right: AST) => BinOpExpr(left, op, right)})

  // primitive
  def primitive = string | number | bool
  def number: Parser[AST] = identifier | floatingPointNumber ^^ { v => NumberValue(v.toDouble) } | "("~>expr<~")"
  def identifier: Parser[AST] = ident ^^ { n => IdentifierExpr(n) }
  def bool: Parser[AST] = ("true"|"false") ^^ { b => BooleanValue(b.toBoolean) }
  def string: Parser[AST] = stringLiteral ^^ { str => StringValue(str.substring(1, str.length - 1)) }

}

// fixme: あとでsealed修飾子をつける
trait AST

case class Statements(left: AST, right: AST) extends AST
case class DoNothingStmt(env: Environment) extends AST
case class AssignStmt(name: String, expr: AST) extends AST
case class IfStmt(condition: AST, consequence: AST, alternative: AST) extends AST
case class PrintlnStmt(body: AST) extends AST

case class BinOpExpr(left: AST, op: String, right: AST) extends AST
case class RelOpExpr(left: AST, op: String, right: AST) extends AST

case class IdentifierExpr(name: String) extends AST
case class NumberValue(value: Double) extends AST
case class BooleanValue(value: Boolean) extends AST
case class StringValue(value: String) extends AST

package com.gochiusa.golang.core

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  protected val ifKeyword = "if"
  protected val elseKeyword = "else"
  protected val varKeyword = "var"
  protected val substituteKeyword = "="
  protected val whileKeyword = "while"
  protected val printKeyword = "println"

  // statement
  def stmt: Parser[AST] = chainl1(expr, ";" ^^ { _ => (left: AST, right: AST) => Stmt(left, right)})
  def ifStmt: Parser[AST] = (
    ifKeyword~expr~"{"~expr~"}"~elseKeyword~"{"~expr~"}" ^^ { case ifKeyword~cond~"{"~cons~"}"~elseKeyword~"{"~alt~"}" => IfStmt(cond, cons, alt)}
  | ifKeyword~expr~"{"~expr~"}" ^^ { case ifKeyword~cond~"{"~cons~"}" => IfStmt(cond, cons, cons)})
  def assignStmt: Parser[AST] = varKeyword~>ident~"="~expr ^^ { case id~_~exp => AssignStmt(id, exp) }

  // expression
  def expr: Parser[AST] = binOpExpr | relOpExp | identifierExpr | primitive
  def binOpExpr: Parser[AST] = additionOpExpr | multiplyOpExpr
  def additionOpExpr: Parser[AST] = chainl1(multiplyOpExpr, ("+"|"-")^^{ op => (left: AST, right: AST) => BinOpExpr(left, op, right)})
  def multiplyOpExpr: Parser[AST] = chainl1(number, ("*"|"/")^^{ op => (left: AST, right: AST) => BinOpExpr(left, op, right)})

  def relOpExp: Parser[AST] = (number|identifierExpr) ~ ("<"|">"|"<="|">="|"=="|"!=") ~ (number|identifierExpr) ^^ { case left ~ op ~ right => RelOpExpr(left, op, right)}
  def identifierExpr: Parser[AST] = ident ^^ { n => IdentifierExpr(n) }

  // primitive
  def primitive = number | bool | string
  def number: Parser[AST] = floatingPointNumber ^^ { v => NumberValue(v.toDouble) } | "("~>expr<~")"
  def bool: Parser[AST] = ("true"|"false") ^^ { b => BooleanValue(b.toBoolean) }
  def string: Parser[AST] = stringLiteral ^^ { str => StringValue(str.substring(1, str.length - 1)) }

}

// fixme: あとでsealed修飾子をつける
trait AST

case class Stmt(left: AST, right: AST) extends AST
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

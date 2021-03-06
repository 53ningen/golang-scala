package com.gochiusa.golang.core

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  protected val ifKeyword = "if"
  protected val elseKeyword = "else"
  protected val varKeyword = "var"
  protected val substituteKeyword = "="
  protected val whileKeyword = "while"
  protected val printKeyword = "println"
  protected val trueKeyword = "true"
  protected val falseKeyword = "false"

  // statement

  def stmts: Parser[AST] =  chainl1(stms, ";" ^^ { _ => (left: AST, right: AST) => Statements(left, right) })

  def stms: Parser[AST] = assignStmt | expr

  def assignStmt: Parser[AST] = varKeyword ~> ident ~ substituteKeyword ~ expr ^^ { case id ~ _ ~ exp => AssignStmt(id, exp)}

  def printlnStmt: Parser[AST] = printKeyword ~ "(" ~> expr ~ ")" ^^ { case expr ~ _ => PrintlnStmt(expr)}

  // expression:
  def expr: Parser[AST] = ifExpr | relOpExp | printlnStmt | bool | string | binOpExpr | identifier

  def ifExpr: Parser[AST] = ifKeyword ~ "(" ~> expr ~ ")" ~ "{" ~ expr ~ "}" ~ elseKeyword ~ "{" ~ expr <~ "}" ^^ { case cond ~ _ ~ _ ~ cons ~ _ ~ _ ~ _ ~ alt => IfExpr(cond, cons, alt)}

  def relOpExp: Parser[AST] = (identifier | number) ~ ("<" | ">" | "<=" | ">=" | "==" | "!=") ~ (identifier | number) ^^ { case left ~ op ~ right => RelOpExpr(left, op, right)}

  def binOpExpr: Parser[AST] = binTermOpExpr

  def binTermOpExpr: Parser[AST] = chainl1(binFactorOpExpr, ("+" | "-") ^^ { op => (left: AST, right: AST) => BinOpExpr(left, op, right)})

  def binFactorOpExpr: Parser[AST] = chainl1(numberExpr, ("*" | "/") ^^ { op => (left: AST, right: AST) => BinOpExpr(left, op, right)})

  def numberExpr: Parser[AST] = identifier | number | "(" ~> expr <~ ")"

  // primitive
  def primitive = string | number | bool

  def number: Parser[AST] = floatingPointNumber ^^ { v => NumberValue(v.toDouble)}

  def bool: Parser[AST] = trueKeyword ^^ { _ => BooleanValue(true)} | falseKeyword ^^ { _ => BooleanValue(false)}

  def string: Parser[AST] = stringLiteral ^^ { str => StringValue(str.substring(1, str.length - 1))}

  def identifier: Parser[AST] = ident ^^ { n => IdentifierExpr(n)}

}

// fixme: あとでsealed修飾子をつける
trait AST

case class Statements(left: AST, right: AST) extends AST

case class DoNothingStmt(env: Environment) extends AST

case class AssignStmt(name: String, expr: AST) extends AST

case class PrintlnStmt(body: AST) extends AST

case class IfExpr(condition: AST, consequence: AST, alternative: AST) extends AST

case class BinOpExpr(left: AST, op: String, right: AST) extends AST

case class RelOpExpr(left: AST, op: String, right: AST) extends AST

case class IdentifierExpr(name: String) extends AST

case class NumberValue(value: Double) extends AST

case class BooleanValue(value: Boolean) extends AST

case class StringValue(value: String) extends AST

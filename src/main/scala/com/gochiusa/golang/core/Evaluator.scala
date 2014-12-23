package com.gochiusa.golang.core

class Evaluator {
  def eval(ast: AST, env: Environment): Any = {
    ast match {
      case IfStmt(cond, cons, alt) => eval(cond, env) match {
        case true => eval(cons, env)
        case false => eval(alt, env)
      }
      case AssignStmt(name, expr) => DoNothingStmt(env.set(name, expr))
      case BinOpExpr(left, op, right) => (eval(left, env), op, eval(right, env)) match {
        case (left: Double, _, right: Double) => op match {
          case "+" => left + right
          case "-" => left - right
          case "*" => left * right
          case "/" => left / right
        }
      }
      case RelOpExpr(left, op, right) => (eval(left, env), op, eval(right, env)) match {
        case (left: Double, _, right: Double) => op match {
          case "<" => left < right
          case ">" => left > right
          case "<=" => left <= right
          case ">=" => left >= right
          case "==" => left == right
          case "!=" => left != right
        }
        case (left: String, _, right: String) => op match {
          case "==" => left == right
          case "!=" => left != right
        }
        case (left: Boolean, _, right: Boolean) => op match {
          case "==" => left == right
          case "!=" => left != right
        }
      }
      case IdentifierExpr(name) => eval(env.get(name), env)
      case NumberValue(value) => value
      case BooleanValue(value) => value
      case StringValue(value) => value
    }
  }
}

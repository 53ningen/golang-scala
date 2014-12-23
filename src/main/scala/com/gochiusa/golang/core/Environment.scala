package com.gochiusa.golang.core

case class Environment(table: Map[String, AST]) {
  def get(key: String): AST = table(key)
  def set(key: String, value: AST): Environment = Environment(table + (key -> value))
}

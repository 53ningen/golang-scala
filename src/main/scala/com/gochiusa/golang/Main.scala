package com.gochiusa.golang

import com.gochiusa.golang.core.{Evaluator, Parser, Environment}

import scala.io.Source


object Main {
  def main (args: Array[String]): Unit = {
    val parser = new Parser {
      override protected val ifKeyword = "if"
      override protected val elseKeyword = "also"
      override protected val varKeyword = "var"
      override protected val substituteKeyword = "="
      override protected val whileKeyword = "while"
      override protected val printKeyword = "print"
      override protected val trueKeyword = "true"
      override protected val falseKeyword = "false"
    }

    val evaluator = new Evaluator

    if (args.length > 0) {
      val source = Source.fromFile(args(0))
      val result = parser.parseAll(parser.stmts, source.mkString)
      evaluator.eval(result.get, Environment(Map()))
    }

  }
}

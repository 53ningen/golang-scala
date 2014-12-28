package com.gochiusa.golang

import com.gochiusa.golang.core.{Evaluator, AST, Environment, Parser}

import scala.io.Source


object Main {
  def main (args: Array[String]): Unit = {
    val parser = new Parser {
      override protected val ifKeyword = "心ぴょんぴょん待ち？"
      override protected val elseKeyword = "ふわふわどきどき"
      override protected val varKeyword = "らんらん"
      override protected val substituteKeyword = "希望？"
      override protected val whileKeyword = "マジ？"
      override protected val printKeyword = "もふもふ"
      override protected val trueKeyword = "ぴょんぴょん"
      override protected val falseKeyword = "わわわ！"
    }

    val evaluator = new Evaluator

    if (args.length > 0) {
      val source = Source.fromFile(args(0))
      val result = parser.parseAll(parser.stmts, source.mkString)
      evaluator.eval(result.get, Environment(Map()))
    }

  }
}

package com.gochiusa.golang

import com.gochiusa.golang.core.{Evaluator, AST, Environment, Parser}


object Main {
  def main (args: Array[String]): Unit = {
    val parser = new Parser {
      override protected val ifKeyword = "心ぴょんぴょん"
      override protected val elseKeyword = "待ち？"
      override protected val varKeyword = "心"
      override protected val substituteKeyword = "ぴょんぴょん"
      override protected val whileKeyword = "心ぴょんぴょん待ち？考えるフリしてもうちょっと！"
      override protected val printKeyword = "心ぴょんぴょん可能"
    }

    val evaluator = new Evaluator

    if (args.length > 1) {
      val result = parser.parseAll(parser.stmts, args(1))
      evaluator.eval(result.get, Environment(Map()))
    }

  }
}

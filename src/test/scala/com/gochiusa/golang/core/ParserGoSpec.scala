package com.gochiusa.golang.core

import org.specs2.mutable.Specification

class ParserGoSpec extends Specification {

  val parser = new Parser {
    override protected val ifKeyword = "心ぴょんぴょん"
    override protected val elseKeyword = "待ち？"
    override protected val varKeyword = "var"
    override protected val substituteKeyword = "="
    override protected val whileKeyword = "while"
    override protected val printKeyword = "println"
  }

  "Parser" should {

    "真偽値trueをパースできる" in {
      val result = parser.parseAll(parser.expr, "true")
      val expected = BooleanValue(true)
      result.get mustEqual expected
    }

    "真偽値falseをパースできる" in {
      val result = parser.parseAll(parser.expr, "false")
      val expected = BooleanValue(false)
      result.get mustEqual expected
    }

    "If文をパースできる" in {
      val result = parser.parse(parser.expr, "心ぴょんぴょん (x > 12470) { \"ティッピーゴールデンフラワリーオレンジペコ\" } 待ち？ { \"清川元夢\" }")
      val expected = IfStmt(RelOpExpr(IdentifierExpr("x"), ">", NumberValue(12470)), StringValue("ティッピーゴールデンフラワリーオレンジペコ"), StringValue("清川元夢"))
      result.get mustEqual expected
    }

    "Assign文をパースできる" in {
      val result = parser.parse(parser.stmt, "var order = \"そのうさぎさん\"")
      val expected = AssignStmt("order", StringValue("そのうさぎさん"))
      result.get mustEqual expected
    }

  }

}

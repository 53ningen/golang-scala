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
      val result = parser.parseAll(parser.stmts, "true")
      val expected = BooleanValue(true)
      result.get mustEqual expected
    }

    "真偽値falseをパースできる" in {
      val result = parser.parseAll(parser.stmts, "false")
      val expected = BooleanValue(false)
      result.get mustEqual expected
    }

    "If文をパースできる" in {
      val result = parser.parse(parser.stmts, "心ぴょんぴょん (x > 12470) { \"ティッピーゴールデンフラワリーオレンジペコ\" } 待ち？ { \"清川元夢\" }")
      val expected = IfExpr(RelOpExpr(IdentifierExpr("x"), ">", NumberValue(12470)), StringValue("ティッピーゴールデンフラワリーオレンジペコ"), StringValue("清川元夢"))
      result.get mustEqual expected
    }

    "Assign文をパースできる" in {
      val result = parser.parse(parser.stmts, "var order = \"そのうさぎさん\"")
      val expected = AssignStmt("order", StringValue("そのうさぎさん"))
      result.get mustEqual expected
    }

    "If式のネストをパースできる" in {
      val result = parser.parse(parser.stmts, "心ぴょんぴょん (x > 12470) { 心ぴょんぴょん ( y > 367){ \"ティッピーゴールデンフラワリーオレンジペコ\"} 待ち？ {  \"あんこ\"  }  } 待ち？ { \"清川元夢\" }")
      val expected = IfExpr(RelOpExpr(IdentifierExpr("x"), ">", NumberValue(12470.0)), IfExpr(RelOpExpr(IdentifierExpr("y"), ">", NumberValue(367.0)), StringValue("ティッピーゴールデンフラワリーオレンジペコ"), StringValue("あんこ")), StringValue("清川元夢"))
      result.get mustEqual expected
    }

  }

}

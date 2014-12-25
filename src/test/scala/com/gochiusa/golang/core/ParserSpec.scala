package com.gochiusa.golang.core

import org.specs2.mutable.Specification

class ParserSpec extends Specification {

  val parser = new Parser

  "Parser" should {

    "数字をパースできる" in {
      val result = parser.parseAll(parser.stmt, "-12.470")
      val expected = NumberValue(-12.470)
      result.get mustEqual expected
    }

    "文字をパースできる" in {
      val result = parser.parseAll(parser.stmt, "\"ご注文はうさぎですか?\"")
      val expected = StringValue("ご注文はうさぎですか?")
      result.get mustEqual expected
    }

    "真偽値trueをパースできる" in {
      val result = parser.parseAll(parser.stmt, "true")
      val expected = BooleanValue(true)
      result.get mustEqual expected
    }

    "真偽値falseをパースできる" in {
      val result = parser.parseAll(parser.stmt, "false")
      val expected = BooleanValue(false)
      result.get mustEqual expected
    }

    "識別子をパースできる" in {
      val result = parser.parseAll(parser.stmt, "cocoa")
      val expected = IdentifierExpr("cocoa")
      result.get mustEqual expected
    }

    "比較演算子を用いた式をパースできる" in {
      val result = parser.parseAll(parser.stmt, "x > 10")
      val expected = RelOpExpr(IdentifierExpr("x"), ">", NumberValue(10))
      result.get mustEqual expected
    }

    "簡単な数式をパースできる" in {
      val result = parser.parseAll(parser.stmt, "7 + 60 * 6")
      val expected = BinOpExpr(NumberValue(7), "+", BinOpExpr(NumberValue(60), "*", NumberValue(6)))
      result.get mustEqual expected
    }

    "括弧つきの数式をパースできる" in {
      val result = parser.parseAll(parser.stmt, "(128+367*12470)")
      val expected = BinOpExpr(NumberValue(128), "+", BinOpExpr(NumberValue(367), "*", NumberValue(12470)))
      result.get mustEqual expected
    }

    "単純な括弧つきの数式をパースできる" in {
      val result = parser.parseAll(parser.stmt, "(128+367*12470)*3620")
      val expected = BinOpExpr(BinOpExpr(NumberValue(128), "+", BinOpExpr(NumberValue(367), "*", NumberValue(12470))), "*", NumberValue(3620))
      result.get mustEqual expected
    }

    "単純な括弧つきの変数を含む数式をパースできる" in {
      val result = parser.parseAll(parser.stmt, "(128+367*x)*3620")
      val expected = BinOpExpr(BinOpExpr(NumberValue(128), "+", BinOpExpr(NumberValue(367), "*", IdentifierExpr("x"))), "*", NumberValue(3620))
      result.get mustEqual expected
    }

    "そこそこ複雑な数式をパースできる" in {
      val result = parser.parseAll(parser.stmt, "(33-4)*(334+33*4-3*3*4)")
      val expected = BinOpExpr(BinOpExpr(NumberValue(33.0), "-", NumberValue(4.0)), "*", BinOpExpr(BinOpExpr(NumberValue(334.0), "+", BinOpExpr(NumberValue(33.0), "*", NumberValue(4.0))), "-", BinOpExpr(BinOpExpr(NumberValue(3.0), "*", NumberValue(3.0)), "*", NumberValue(4.0))))
      result.get mustEqual expected
    }

    "変数を含むそこそこ複雑な数式をパースできる" in {
      val result = parser.parseAll(parser.stmt, "(33-x)*(334+33*4-3*3*4)")
      val expected = BinOpExpr(BinOpExpr(NumberValue(33), "-", IdentifierExpr("x")), "*", BinOpExpr(BinOpExpr(NumberValue(334.0), "+", BinOpExpr(NumberValue(33.0), "*", NumberValue(4.0))), "-", BinOpExpr(BinOpExpr(NumberValue(3.0), "*", NumberValue(3.0)), "*", NumberValue(4.0))))
      result.get mustEqual expected
    }

    "If式をパースできる" in {
      val result = parser.parse(parser.stmt, "if (x > 12470) { \"ティッピーゴールデンフラワリーオレンジペコ\" } else { \"清川元夢\" }")
      val expected = IfExpr(RelOpExpr(IdentifierExpr("x"), ">", NumberValue(12470)), StringValue("ティッピーゴールデンフラワリーオレンジペコ"), StringValue("清川元夢"))
      result.get mustEqual expected
    }

    "If式のネストをパースできる" in {
      val result = parser.parse(parser.stmt, "if (x > 12470) { if ( y > 367){ \"ティッピーゴールデンフラワリーオレンジペコ\"} else {  \"あんこ\"  }  } else { \"清川元夢\" }")
      val expected = IfExpr(RelOpExpr(IdentifierExpr("x"), ">", NumberValue(12470.0)), IfExpr(RelOpExpr(IdentifierExpr("y"), ">", NumberValue(367.0)), StringValue("ティッピーゴールデンフラワリーオレンジペコ"), StringValue("あんこ")), StringValue("清川元夢"))
      result.get mustEqual expected
    }

    "print文を含むIf式をパースできる" in {
      val result = parser.parse(parser.stmt, "if (x > 12470) { println(\"ティッピーゴールデンフラワリーオレンジペコ\") } else { \"清川元夢\" }")
      val expected = IfExpr(RelOpExpr(IdentifierExpr("x"), ">", NumberValue(12470)), PrintlnStmt(StringValue("ティッピーゴールデンフラワリーオレンジペコ")), StringValue("清川元夢"))
      result.get mustEqual expected
    }

    "Assign文をパースできる" in {
      val result = parser.parse(parser.stmt, "var order = \"そのうさぎさん\"")
      val expected = AssignStmt("order", StringValue("そのうさぎさん"))
      result.get mustEqual expected
    }

  }

}

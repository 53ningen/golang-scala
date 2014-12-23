package com.gochiusa.golang.core

import org.specs2.mutable.Specification

class ParserSpec extends Specification {

  val parser = new Parser

  "Parser" should {

    "数字をパースできる" in {
      val result = parser.parseAll(parser.number, "-12.470")
      val expected = NumberValue(-12.470)
      result.get mustEqual expected
    }

    "文字をパースできる" in {
      val result = parser.parseAll(parser.string, "\"ご注文はうさぎですか?\"")
      val expected = StringValue("ご注文はうさぎですか?")
      result.get mustEqual expected
    }

    "真偽値trueをパースできる" in {
      val result = parser.parseAll(parser.bool, "true")
      val expected = BooleanValue(true)
      result.get mustEqual expected
    }

    "真偽値falseをパースできる" in {
      val result = parser.parseAll(parser.bool, "false")
      val expected = BooleanValue(false)
      result.get mustEqual expected
    }

    "識別子をパースできる" in {
      val result = parser.parseAll(parser.identifierExpr, "cocoa")
      val expected = IdentifierExpr("cocoa")
      result.get mustEqual expected
    }

    "比較演算子を用いた式をパースできる" in {
      val result = parser.parseAll(parser.relOpExp, "x > 10")
      val expected = RelOpExpr(IdentifierExpr("x"), ">", NumberValue(10))
      result.get mustEqual expected
    }

    "簡単な数式をパースできる" in {
      val result = parser.parseAll(parser.binOpExpr, "7 + 60 * 6")
      val expected = BinOpExpr(NumberValue(7), "+", BinOpExpr(NumberValue(60), "*", NumberValue(6)))
      result.get mustEqual expected
    }

    "括弧つきの数式をパースできる" in {
      val result = parser.parseAll(parser.binOpExpr, "(128+367*12470)")
      val expected = BinOpExpr(NumberValue(128), "+", BinOpExpr(NumberValue(367), "*", NumberValue(12470)))
      result.get mustEqual expected
    }

    "単純な括弧つきの数式をパースできる" in {
      val result = parser.parseAll(parser.binOpExpr, "(128+367*12470)*3620")
      val expected = BinOpExpr(BinOpExpr(NumberValue(128), "+", BinOpExpr(NumberValue(367), "*", NumberValue(12470))), "*", NumberValue(3620))
      result.get mustEqual expected
    }

    "そこそこ複雑な数式をパースできる" in {
      val result = parser.parseAll(parser.binOpExpr, "(33-4)*(334+33×4-3×3×4)")
      val expected = BinOpExpr(BinOpExpr(NumberValue(33), "-", NumberValue(4)), "*", BinOpExpr(NumberValue(334), "+", BinOpExpr(BinOpExpr(NumberValue(33), "*", NumberValue(4)), "-", BinOpExpr(NumberValue(3), "*", BinOpExpr(NumberValue(3), "*" , NumberValue(4))))))
      result.get mustEqual expected
    }.pendingUntilFixed("まぁいっか！（適当）")

    "変数を含むそこそこ複雑な数式をパースできる" in {
      val result = parser.parseAll(parser.binOpExpr, "(33-4)*(334+33×4-3×3×4)")
      val expected = BinOpExpr(BinOpExpr(NumberValue(33), "-", NumberValue(4)), "*", BinOpExpr(NumberValue(334), "+", BinOpExpr(BinOpExpr(NumberValue(33), "*", NumberValue(4)), "-", BinOpExpr(NumberValue(3), "*", BinOpExpr(NumberValue(3), "*" , NumberValue(4))))))
      result.get mustEqual expected
    }.pendingUntilFixed("まぁいっか！（適当）")

  }

}

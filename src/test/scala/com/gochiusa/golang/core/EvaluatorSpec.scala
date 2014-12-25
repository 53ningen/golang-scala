package com.gochiusa.golang.core

import org.specs2.mutable.Specification

class EvaluatorSpec extends Specification {

  val evaluator = new Evaluator {
    override def printOut(any: Any): Unit = {

    }
  }
  val env = Environment(Map())

  "Evaluator" should {

    "数字を評価できる" in {
      val actual = evaluator.eval(NumberValue(-12470), env)
      val expected = -12470
      actual mustEqual expected
    }

    "文字を評価できる" in {
      val actual = evaluator.eval(StringValue("少女は赤い外套を纏いウサギを駆りて聖夜の空を行く"), env)
      val expected = "少女は赤い外套を纏いウサギを駆りて聖夜の空を行く"
      actual mustEqual expected
    }

    "真偽値を評価できる" in {
      val actual = evaluator.eval(BooleanValue(true), env)
      val expected = true
      actual mustEqual expected
    }

    "変数を評価できる" in {
      val actual = evaluator.eval(IdentifierExpr("cocoa"), env.set("cocoa", StringValue("保登心愛")).set("sharo", StringValue("桐間紗路")))
      val expected = "保登心愛"
      actual mustEqual expected
    }

    "比較演算子を用いた式を評価できる" in {
      val actual = evaluator.eval(RelOpExpr(NumberValue(10), "<", NumberValue(20)), env)
      val expected = true
      actual mustEqual expected
    }

    "環境の値と比較演算子を用いた式を評価できる" in {
      val actual = evaluator.eval(RelOpExpr(IdentifierExpr("x"), "<", NumberValue(20)), env.set("x", NumberValue(12470)))
      val expected = false
      actual mustEqual expected
    }

    "簡単な数式を評価できる" in {
      val result = evaluator.eval(BinOpExpr(NumberValue(7), "+", BinOpExpr(NumberValue(60), "*", NumberValue(6))), env)
      val expected = 367
      result mustEqual expected
    }

    "単純な括弧つきの数式を評価できる" in {
      val result = evaluator.eval(BinOpExpr(BinOpExpr(NumberValue(12470), "+", BinOpExpr(NumberValue(367), "*", NumberValue(128))), "*", NumberValue(3620)), env)
      val expected = 215194520
      result mustEqual expected
    }

    "そこそこ複雑な数式をパースできる" in {
      val result = evaluator.eval(BinOpExpr(BinOpExpr(NumberValue(33.0), "-", NumberValue(4.0)), "*", BinOpExpr(BinOpExpr(NumberValue(334.0), "+", BinOpExpr(NumberValue(33.0), "*", NumberValue(4.0))), "-", BinOpExpr(BinOpExpr(NumberValue(3.0), "*", NumberValue(3.0)), "*", NumberValue(4.0)))), env)
      val expected = 12470
      result mustEqual expected
    }

    "変数を含むそこそこ複雑な数式をパースできる" in {
      val result = evaluator.eval(BinOpExpr(BinOpExpr(NumberValue(33), "-", IdentifierExpr("x")), "*", BinOpExpr(BinOpExpr(NumberValue(334.0), "+", BinOpExpr(NumberValue(33.0), "*", NumberValue(4.0))), "-", BinOpExpr(BinOpExpr(NumberValue(3.0), "*", NumberValue(3.0)), "*", NumberValue(4.0)))), env.set("x", NumberValue(4)))
      val expected = 12470
      result mustEqual expected
    }

    "If文を評価できる：true編" in {
      val result = evaluator.eval(IfExpr(BooleanValue(true), StringValue("佐倉綾音"), StringValue("徳井青空")), env)
      val expected = "佐倉綾音"
      result mustEqual expected
    }

    "If文を評価できる：false編" in {
      val result = evaluator.eval(IfExpr(BooleanValue(false), StringValue("佐倉綾音"), StringValue("徳井青空")), env)
      val expected = "徳井青空"
      result mustEqual expected
    }

    "If文を評価できる：環境の値を使える" in {
      val result = evaluator.eval(IfExpr(IdentifierExpr("is百合"), StringValue("佐倉綾音"), StringValue("徳井青空")), env.set("is百合", BooleanValue(true)))
      val expected = "佐倉綾音"
      result mustEqual expected
    }

    "If文を評価できる：Statement" in {
      val result = evaluator.eval(IfExpr(RelOpExpr(IdentifierExpr("x"), ">", NumberValue(12470)), PrintlnStmt(StringValue("ティッピーゴールデンフラワリーオレンジペコ")), PrintlnStmt(StringValue("清川元夢"))), env.set("x", NumberValue(367)))
      val expected = DoNothingStmt(env.set("x", NumberValue(367)))
      result mustEqual expected
    }

    "If式のネストを評価できる" in {
      val result = evaluator.eval(IfExpr(RelOpExpr(IdentifierExpr("x"), "<", NumberValue(12470.0)), IfExpr(RelOpExpr(IdentifierExpr("y"), ">", NumberValue(367.0)), StringValue("ティッピーゴールデンフラワリーオレンジペコ"), StringValue("あんこ")), StringValue("清川元夢")), env.set("x", NumberValue(367)).set("y", NumberValue(3620)))
      val expected = "ティッピーゴールデンフラワリーオレンジペコ"
      result mustEqual expected
    }

    "Assign文を評価すると環境を更新したDoNothingStmtを返す" in {
      val result = evaluator.eval(AssignStmt("香風智乃", StringValue("かわいい")), env)
      val expected = DoNothingStmt(Environment(Map("香風智乃" -> StringValue("かわいい"))))
      result mustEqual expected
    }

    "Println文を評価するとDoNothingStmtを返す" in {
      val result = evaluator.eval(PrintlnStmt(StringValue("マジなパンだからね！本気で可愛い子ぶってるんだよ！")), env)
      val expected = DoNothingStmt(env)
      result mustEqual expected
    }

    "Statementsを評価できる" in {
      val result = evaluator.eval(Statements(DoNothingStmt(env), StringValue("")), env)
      val expected = ""
      result mustEqual expected
    }

    "複数の数字を評価できる" in {
      val result = evaluator.eval(Statements(Statements(Statements(NumberValue(1.0), NumberValue(2.0)), NumberValue(3.0)), NumberValue(4.0)), env)
      val expected = ()
      result mustEqual expected
    }
  }

}

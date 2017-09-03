import org.scalatest.{Matchers, WordSpecLike}

import scala.collection.immutable.HashMap

class ExprSpec extends WordSpecLike with Matchers {
  val cellNames = List("A1", "A2", "A3", "B1", "B2", "B3")
  val cellMap: HashMap[String, Expr] = HashMap(
    "A1" -> Variable(Some("A1"), "A2", dependsOn = List("A2")),
    "A2" -> BinOp(Some("A2"), "*", Number(None, 4), Number(None, 5)),
    "A3" -> Variable(Some("A3"), "A1", dependsOn = List("A1")),
    "B1" -> BinOp(Some("B1"), "+", BinOp(None, "/", Variable(None, "A1"), Variable(None, "B2")), Number(None, 2.0), dependsOn = List("A1", "B2")),
    "B2" -> Number(Some("B2"), 3.0),
    "B3" -> BinOp(Some("B3"), "/", Number(None, 39.0), BinOp(None, "*", Variable(None, "B1"), Variable(None, "B2")), dependsOn = List("B1", "B2"))
  )

  "apply" should {
    "return an appropriate Expr depending on the input" in {
      Expr(Some("A1"), cellNames, "A2") should be(Variable(Some("A1"), "A2", dependsOn = List("A2")))
      Expr(None, cellNames, "1") should be(Number(None, 1.0))
      Expr(Some("A2"), cellNames, "4 5 *") should be(BinOp(Some("A2"), "*", Number(None, 4.0), Number(None, 5.0)))
    }
  }

  "createBinOp" should {
    "return a BinOp Expr when given a postfix expression" in {
      Expr.createBinOp(Some("B1"), cellNames, "A1 B2 / 2 +") should be(
        BinOp(Some("B1"), "+", BinOp(None, "/", Variable(None, "A1"), Variable(None, "B2")), Number(None, 2.0), dependsOn = List("A1", "B2"))
      )
      Expr.createBinOp(Some("B3"), cellNames, "39 B1 B2 * /") should be(
        BinOp(Some("B3"), "/", Number(None, 39.0), BinOp(None, "*", Variable(None, "B1"), Variable(None, "B2")), dependsOn = List("B1", "B2"))
      )
      Expr.createBinOp(Some("A2"), cellNames, "4 5 *") should be(
        BinOp(Some("A2"), "*", Number(None, 4.0), Number(None, 5.0))
      )
    }
    "throw AssertionError if postfix expression contains illegal operators" in {
      a[AssertionError] should be thrownBy Expr.createBinOp(Some("B1"), cellNames, "A1 B2 / 2 @")
    }
    "throw AssertionError if postfix expression is not well formed" in {
      a[AssertionError] should be thrownBy Expr.createBinOp(Some("B1"), cellNames, "A1 B2 / 2 * +")
    }
  }

  "evalExpr" should {
    "calculate the value of expr if its not already been calculated (value=None). " +
      "Return the calculated value and an updated map of Exprs with possibly calculated values" in {
      val (value, newCellMap) = Expr.evalExpr(Variable(Some("A3"), "A1", dependsOn = List("A1")), cellMap, List.empty[String])
      value should be(20.0)
      newCellMap should be(
        HashMap(
          "A1" -> Variable(Some("A1"), "A2", Some(20.0), dependsOn = List("A2")),
          "A2" -> BinOp(Some("A2"), "*", Number(None, 4), Number(None, 5), Some(20.0)),
          "A3" -> Variable(Some("A3"), "A1", Some(20.0), dependsOn = List("A1")),
          "B1" -> BinOp(Some("B1"), "+", BinOp(None, "/", Variable(None, "A1"), Variable(None, "B2")), Number(None, 2.0), dependsOn = List("A1", "B2")),
          "B2" -> Number(Some("B2"), 3.0),
          "B3" -> BinOp(Some("B3"), "/", Number(None, 39.0), BinOp(None, "*", Variable(None, "B1"), Variable(None, "B2")), dependsOn = List("B1", "B2"))
        )
      )

      val (finalValue, finalCellMap) = Expr.evalExpr(
        BinOp(Some("B3"), "/", Number(None, 39.0), BinOp(None, "*", Variable(None, "B1"), Variable(None, "B2")), dependsOn = List("B1", "B2"))
        , newCellMap, List.empty[String])
      finalValue should be(1.4999999999999998)
      finalCellMap should be(
        HashMap(
          "A1" -> Variable(Some("A1"), "A2", Some(20.0), dependsOn = List("A2")),
          "A2" -> BinOp(Some("A2"), "*", Number(None, 4), Number(None, 5), Some(20.0)),
          "A3" -> Variable(Some("A3"), "A1", Some(20.0), dependsOn = List("A1")),
          "B1" -> BinOp(Some("B1"), "+", BinOp(None, "/", Variable(None, "A1"), Variable(None, "B2")), Number(None, 2.0), Some(8.666666666666668), dependsOn = List("A1", "B2")),
          "B2" -> Number(Some("B2"), 3.0),
          "B3" -> BinOp(Some("B3"), "/", Number(None, 39.0), BinOp(None, "*", Variable(None, "B1"), Variable(None, "B2")), Some(1.4999999999999998), dependsOn = List("B1", "B2"))
        )
      )
    }
    "throw RuntimeException if spreadsheet has self referencing cell" in {
      val cellMap: HashMap[String, Expr] = HashMap(
        "A1" -> Variable(Some("A1"), "A1", dependsOn = List("A1"))
      )
      a[RuntimeException] should be thrownBy Expr.evalExpr(Variable(Some("A1"), "A1", dependsOn = List("A1")), cellMap, List.empty[String])
    }
    "throw RuntimeException if spreadsheet has cyclical due to Variable Expr" in {
      val cellMap: HashMap[String, Expr] = HashMap(
        "A1" -> Variable(Some("A1"), "A2", dependsOn = List("A2")),
        "A2" -> Variable(Some("A2"), "A3", dependsOn = List("A3")),
        "A3" -> Variable(Some("A3"), "A1", dependsOn = List("A1"))
      )
      a[RuntimeException] should be thrownBy Expr.evalExpr(
        Variable(Some("A1"), "A2", dependsOn = List("A2")), cellMap, List.empty[String])
    }
    "throw RuntimeException if spreadsheet has cyclical postfix expression" in {
      // A1 [A2 A3 +]
      // A2 [5]
      // A3 [A1]
      val cellMap: HashMap[String, Expr] = HashMap(
        "A1" -> BinOp(Some("A1"), "+", Variable(None, "A2"), Variable(None, "A3"), dependsOn = List("A2","A3")),
        "A2" -> Number(Some("A2"), 5.0),
        "A3" -> Variable(Some("A3"), "A1", dependsOn = List("A1"))
      )
      a[RuntimeException] should be thrownBy Expr.evalExpr(
        BinOp(Some("A1"), "+", Variable(None, "A2"), Variable(None, "A3"), dependsOn = List("A2","A3")), cellMap, List.empty[String])
    }
    "be able to evaluate a more complex postfix expression" in {
      val expr = Expr(None, cellNames, "15 7 1 1 + - / 3 * 2 1 1 + + -")
      val (value, _) = Expr.evalExpr(expr, HashMap.empty[String, Expr], List.empty[String])
      value should be(5.0)
    }

  }
}

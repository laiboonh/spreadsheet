import org.scalatest._

import scala.collection.immutable.HashMap

class SpreadsheetSpec extends WordSpecLike with Matchers {

  val cellExpressions: List[String] = List("A2", "4 5 *", "A1", "A1 B2 / 2 +", "3", "39 B1 B2 * /")
  val cellNames = List("A1", "A2", "A3", "B1", "B2", "B3")
  val cellMap: HashMap[String, Expr] = HashMap(
    "A1" -> Variable(Some("A1"), "A2", dependsOn = List("A2")),
    "A2" -> BinOp(Some("A2"), "*", Number(None, 4), Number(None, 5)),
    "A3" -> Variable(Some("A3"), "A1", dependsOn = List("A1")),
    "B1" -> BinOp(Some("B1"), "+", BinOp(None, "/", Variable(None, "A1"), Variable(None, "B2")), Number(None, 2.0), dependsOn = List("A1", "B2")),
    "B2" -> Number(Some("B2"), 3.0),
    "B3" -> BinOp(Some("B3"), "/", Number(None, 39.0), BinOp(None, "*", Variable(None, "B1"), Variable(None, "B2")), dependsOn = List("B1", "B2"))
  )
  val rootCellNames = List("A3", "B3")

  "createCellNames" should {
    "return a list of cell names for the number of rows and columns given" in {
      Spreadsheet.createCellNames(3, 2) should be(cellNames)
    }
    "throw AssertionError if number of rows is > 26" in {
      an[AssertionError] should be thrownBy Spreadsheet.createCellNames(3, 27)
    }

  }

  "createCellMap" should {
    "prepare a map of cellName -> Cell give a list of cell expressions" in {
      Spreadsheet.createCellMap(cellNames, cellExpressions) should be(cellMap)
    }
  }

  "detectRootCells" should {
    "return a list of cellNames that are root cells (it does not depend on other cells)" in {
      Spreadsheet.detectRootCells(cellNames, cellMap) should be(rootCellNames)
    }
  }

  "postfixVariables" should {
    "return a list of cell names being referred to in a postfix expression" in {
      Spreadsheet.detectVariables(cellNames, List("4", "5", "*")) should be(List.empty[String])
      Spreadsheet.detectVariables(cellNames, List("A1", "B2", "/", "2", "+")) should be(List("A1", "B2"))
      Spreadsheet.detectVariables(cellNames, List("39", "B1", "B2", "*", "/")) should be(List("B1", "B2"))
    }
    "throw AssertionError if illegal cell names are detected" in {
      an[AssertionError] should be thrownBy Spreadsheet.detectVariables(cellNames, List("39", "B11", "B2", "*", "/"))
    }
  }

  "evalSpreadsheet" should {
    "return a hashMap of all the cells with individual values evaluated" in {
      Spreadsheet.evalSpreadsheet(cellMap, rootCellNames) should be(
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
  }

}

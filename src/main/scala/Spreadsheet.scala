import Spreadsheet.detectVariables

import scala.collection.immutable.HashMap
import scala.util.matching.Regex

object Spreadsheet extends App {

  try {
    val Array(nrCol, nrRow) = io.StdIn.readLine().split(" ").map(_.toInt)
    val nrCells = nrCol * nrRow
    val cellExpressions = (1 to nrCells).foldRight(List.empty[String]) {
      (_, acc) => acc.:+(io.StdIn.readLine())
    }
    val cellNames = createCellNames(nrCol, nrRow)
    val cellMap = createCellMap(cellNames, cellExpressions)
    val rootCellNames = detectRootCells(cellNames, cellMap)
    val finalCellMap = evalSpreadsheet(cellMap, rootCellNames)
    outputAnswer(nrCol, nrRow, finalCellMap, cellNames)
  } catch {
    case e: Throwable =>
      println(e)
      System.exit(1)
  }


  def outputAnswer(nrCol: Int, nrRow: Int, finalCellMap: HashMap[String, Expr], cellNames: List[String]): Unit = {
    println(s"$nrCol $nrRow")
    cellNames.foreach {
      cellName =>
        finalCellMap(cellName).valueOpt match {
          case None => throw new RuntimeException(s"Cell (Expr) $cellName found with un-evaluated value")
          case Some(value) => println(("%.5f").format(value))
        }
    }
  }

  def evalSpreadsheet(cellMap: HashMap[String, Expr], rootCellNames: List[String]): HashMap[String, Expr] = {
    rootCellNames.foldRight(cellMap) {
      (rootCellName, cellMap) =>
        val rootCell = cellMap(rootCellName)
        val (_, newCellMap) = Expr.evalExpr(rootCell, cellMap, List.empty[String])
        newCellMap
    }
  }

  def createCellNames(nrCol: Int, nrRow: Int): List[String] = {
    val rowNames = 'A' to 'Z'
    assert(nrRow <= rowNames.length, s"input row value > ${rowNames.length}")

    (for {
      rowName <- rowNames.take(nrRow)
      nrCol <- 1 to nrCol
    } yield s"$rowName$nrCol").toList
  }

  def createCellMap(cellNames: List[String], cellExpressions: List[String]): HashMap[String, Expr] = {
    cellNames.zip(cellExpressions).foldRight(HashMap.empty[String, Expr]) {
      (item, acc) => {
        val (cellName, cellExpression) = item
        val expr = Expr(Some(cellName), cellNames, cellExpression)
        acc + (cellName -> expr)
      }
    }
  }

  def detectRootCells(cellNames: List[String], cellMap: HashMap[String, Expr]): List[String] = {
    val nonRootCells = cellMap.values.foldRight(Set.empty[String]) {
      (expr, acc) => acc ++ expr.dependsOn
    }
    cellNames.diff(nonRootCells.toList)
  }

  def detectVariables(cellNames: List[String], tokens: List[String]): List[String] = {
    val variableRegEx = "([A-Z]\\d+)".r
    tokens.foldLeft(List.empty[String]) {
      (acc, item) =>
        item match {
          case variableRegEx(variable) =>
            assert(cellNames.contains(variable), s"Variable $variable is not an available variable in map")
            acc.:+(variable)
          case _ => acc
        }
    }
  }


}


sealed trait Expr {
  def dependsOn: List[String]

  def cellNameOpt: Option[String]

  def valueOpt: Option[Double]
}

case class Variable(
  cellNameOpt: Option[String],
  varName: String,
  valueOpt: Option[Double] = None,
  dependsOn: List[String] = List.empty[String]) extends Expr

case class Number(
  cellNameOpt: Option[String],
  value: Double,
  dependsOn: List[String] = List.empty[String]) extends Expr {
  override def valueOpt: Option[Double] = Some(value)
}

case class BinOp(
  cellNameOpt: Option[String],
  operator: String,
  left: Expr,
  right: Expr,
  valueOpt: Option[Double] = None,
  dependsOn: List[String] = List.empty[String]) extends Expr

object Expr {
  val legalOperators = List("+", "-", "*", "/")
  val numberRegEx: Regex = "(\\d+)".r
  val variableRegEx: Regex = "([A-Z]\\d+)".r

  def apply(cellNameOpt: Option[String], cellNames: List[String], expr: String): Expr = {
    expr match {
      case numberRegEx(number) => Number(cellNameOpt, number.toDouble)
      case variableRegEx(varName) => Variable(cellNameOpt, varName, dependsOn = List(varName))
      case postfixExpr =>
        createBinOp(cellNameOpt, cellNames: List[String], postfixExpr)
    }
  }

  def createBinOp(cellNameOpt: Option[String], cellNames: List[String], postfixExpr: String): Expr = {
    val tokens = postfixExpr.split(" ").toList
    val dependsOn = detectVariables(cellNames, tokens)
    val binOp = tokens.foldLeft(List.empty[Expr]) {
      (acc, token) =>
        token match {
          case numberRegEx(number) => acc :+ Number(None, number.toDouble)
          case variableRegEx(varName) => acc :+ Variable(None, varName)
          case operator =>
            assert(legalOperators.contains(operator), s"$operator is not one of the legal operators $legalOperators")
            assert(acc.length >= 2, s"upon seeing an operator there should at least be 2 tokens in the list (stack) $acc")
            val rightOperand = acc.last
            val leftOperand = acc(acc.length - 2)
            val newAcc = acc.dropRight(2)
            newAcc :+ BinOp(None, operator, leftOperand, rightOperand)
        }
    }
    assert(binOp.length == 1, s"postfix expression should leave us with 1 and only 1 BinOp expr, if not expression $postfixExpr is not well-formed")
    val binOpExpr = binOp.head.asInstanceOf[BinOp]
    binOpExpr.copy(
      cellNameOpt = cellNameOpt,
      dependsOn = dependsOn
    )
  }

  def evalExpr(
    expression: Expr,
    cellMap: HashMap[String, Expr],
    history: List[String]): (Double, HashMap[String, Expr]) = {

    expression.cellNameOpt.foreach(cellName => if(history.contains(cellName))
      throw new RuntimeException(s"cycle detected : ${(history :+ cellName).mkString(" -> ")}"))

    expression match {
      case variableExpr@Variable(cellNameOpt, varName, valueOption, _) =>
        valueOption match {
          case Some(value) => (value, cellMap) //expr already evaluated
          case None =>
            //cell not evaluated
            val (value, newCellMap) = evalExpr(cellMap(varName),
              cellMap, cellNameOpt.fold(history)(cellName => history :+ cellName))
            //if this is a labelled cell, we need to update cellMap
            (value, cellNameOpt.fold(newCellMap) {
              cellName => newCellMap + (cellName -> variableExpr.copy(valueOpt = Some(value)))
            })
        }
      case Number(_, value, _) => (value, cellMap)
      case binOpExpr@BinOp(cellNameOpt, operator, leftExpr, rightExpr, valueOption, _) =>
        valueOption match {
          case Some(value) => (value, cellMap) //expr already evaluated
          case None =>
            //evaluate leftExpr
            val (leftExprValue, cellMapWithLeftUpdate) = evalExpr(
              leftExpr, cellMap, cellNameOpt.fold(history)(cellName => history.:+(cellName)))
            //evaluate rightExpr
            val (rightExprValue, cellMapWithRightUpdate) = evalExpr(
              rightExpr, cellMapWithLeftUpdate, cellNameOpt.fold(history)(cellName => history.:+(cellName)))
            val binOpValue = operator match {
              case "+" => leftExprValue + rightExprValue
              case "-" => leftExprValue - rightExprValue
              case "*" => leftExprValue * rightExprValue
              case "/" => leftExprValue / rightExprValue
            }
            //if this is a labelled cell, we need to update cellMap
            (binOpValue, cellNameOpt.fold(cellMapWithRightUpdate) {
              cellName => cellMapWithRightUpdate + (cellName -> binOpExpr.copy(valueOpt = Some(binOpValue)))
            })
        }
    }
  }



}
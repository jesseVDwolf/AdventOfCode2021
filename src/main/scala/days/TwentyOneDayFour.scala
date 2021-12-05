package days

import scala.collection.mutable
import scala.util.control.Breaks._

/* https://adventofcode.com/2021/day/4 */
class TwentyOneDayFour(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayFour.day
  val year  : Int = TwentyOneDayFour.year
  val input : String = get_input(cookieHeader)

  val bingoTableSize = 5
  val test_input: String = """
      |7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7
      |""".stripMargin


  def getWinningBoardScore(board: Array[mutable.HashMap[String, Int]], bingoNumber: Int): Int = {
    val sumUnmarkedNumbers = board
      .filter(_("marked") == 0)
      .map{ (map: mutable.HashMap[String, Int]) => map("num") }
      .sum

    sumUnmarkedNumbers * bingoNumber
  }

  def boardHasBingo(board: Array[mutable.HashMap[String, Int]]) : Boolean = {
    val bingo2DMatrix = board
      .map { (map: mutable.HashMap[String, Int]) => map("marked") }
      .grouped(bingoTableSize)
      .toArray

    for ((_, idx) <- board.zipWithIndex) {
      /* check column and row for that index */
      val row     : Int = idx % bingoTableSize
      val column  : Int = idx / bingoTableSize

      if (bingo2DMatrix(row).sum == bingoTableSize || bingo2DMatrix.transpose.view(column).sum == bingoTableSize) {
        return true
      }
    }
    false
  }

  def getNumberOrder(allComponents: Array[String]) : Array[Int] = allComponents(0)
    .split(',')
    .map(_.trim)
    .map(_.toInt)

  def getBingoBoards(allComponents: Array[String]) : Array[Array[mutable.HashMap[String, Int]]] = {
    allComponents.slice(1, allComponents.length)
      .map(_.split("\\s+"))
      .map(_.filter(_.nonEmpty))
      .map(_.map(_.toInt))
      .map(_.map{ (num: Int) => mutable.HashMap("num" -> num, "marked" -> 0) })
  }

  override def solve: Unit = {
    val allComponents     : Array[String] = input.split("\n\n")

    var winningBoardScore = 0
    val bingoBoards = getBingoBoards(allComponents)
    val bingoNumberOrder = getNumberOrder(allComponents)
    breakable { for (bingoNumber <- bingoNumberOrder) {

      breakable { for (idx <- bingoBoards.indices) {
        val board = bingoBoards(idx)

        for (map <- board) {
          if (map("num") == bingoNumber) {
            map("marked") = 1

            if (boardHasBingo(board)) {
              winningBoardScore = getWinningBoardScore(board, bingoNumber)
            }
          }
        }
        if (winningBoardScore > 0)
          break ;
      }}
      if (winningBoardScore > 0)
        break ;
    }}
    println(winningBoardScore)
  }

  override def solve_bonus: Unit = {
    val allComponents     : Array[String] = input.split("\n\n")

    var loosingBoardScore = 0
    val bingoBoards = getBingoBoards(allComponents)
    val bingoNumberOrder = getNumberOrder(allComponents)
    val boardHadBingoList : Array[Boolean] = Array.fill(bingoBoards.length)(false)
    breakable { for (bingoNumber <- bingoNumberOrder) {

      breakable { for (idx <- bingoBoards.indices) {
        val board = bingoBoards(idx)

        for (map <- board) {
          if (map("num") == bingoNumber) {
            map("marked") = 1

            if (boardHasBingo(board)) {
              boardHadBingoList(idx) = true
              if (boardHadBingoList.count(_ == true) == bingoBoards.length) {
                loosingBoardScore = getWinningBoardScore(board, bingoNumber)
              }
            }
          }
        }
        if (loosingBoardScore > 0)
          break ;
      }}
      if (loosingBoardScore > 0)
        break ;
    }}
    println(loosingBoardScore)
  }
}

object TwentyOneDayFour {
  private val year = 2021
  private val day = 4
}
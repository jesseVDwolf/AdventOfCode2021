package days

import scala.collection.mutable
import scala.annotation.tailrec

/* https://adventofcode.com/2021/day/7 */
class TwentyOneDaySeven(cookieHeader: Map[String, String]) extends Solution {
  val day: Int = TwentyOneDaySeven.day
  val year: Int = TwentyOneDaySeven.year
  val input: String = get_input(cookieHeader)

  val test_input : String = "16,1,2,0,4,2,7,1,2,14"

  def factorialSum(n: BigInt)  : BigInt = {

    @tailrec
    def factorialSumAcc(acc: BigInt, n: BigInt) : BigInt = {
      if (n == 0)
        acc
      else
        factorialSumAcc(acc + n, n - 1)
    }
    factorialSumAcc(0, n)
  }

  override def solve: Unit = {
    val horizontalPositions : Array[Int] = input.split(',')
      .map(_.trim)
      .map(_.toInt)

    val possibleOptions : mutable.ArrayBuffer[Int] = mutable.ArrayBuffer()
    for (possiblePos <- horizontalPositions.min until horizontalPositions.max) {
      var fuelSum = 0
      horizontalPositions.foreach{ (pos: Int) => fuelSum += (pos - possiblePos).abs }
      possibleOptions += fuelSum
    }

    println(possibleOptions.min)
  }

  override def solve_bonus: Unit = {
    val horizontalPositions : Array[BigInt] = input.split(',')
      .map(_.trim)
      .map(_.toInt)

    val possibleOptions : mutable.ArrayBuffer[BigInt] = mutable.ArrayBuffer()
    for (possiblePos <- horizontalPositions.min until horizontalPositions.max) {
      var fuelSum : BigInt = 0
      horizontalPositions.foreach{ (pos: BigInt) => fuelSum += factorialSum((pos - possiblePos).abs) }
      possibleOptions += fuelSum
    }

    println(possibleOptions.min)
  }
}

object TwentyOneDaySeven {
  private val year = 2021
  private val day = 7
}


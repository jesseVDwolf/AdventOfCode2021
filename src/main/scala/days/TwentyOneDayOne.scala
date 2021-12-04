package days

import scala.collection.mutable

/* https://adventofcode.com/2021/day/1 */
class TwentyOneDayOne(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayOne.day
  val year  : Int = TwentyOneDayOne.year
  val input : String = get_input(cookieHeader)

  override def solve: Unit = {
    val input_list : Array[Int] = input.split('\n').map(_.toInt)

    var has_increased_count = 0
    for ((num, i) <- input_list.view.zipWithIndex) {
      if (i > 0 && num > input_list(i - 1)) {
        has_increased_count += 1
      }
    }
    println(has_increased_count)
  }

  override def solve_bonus : Unit = {
    val input_list : Array[Int] = input.split('\n').map(_.toInt)

    var has_increased_count = 0
    var deque : mutable.ArrayDeque[Int] = mutable.ArrayDeque()
    for ((num, i) <- input_list.view.zipWithIndex) {
      deque = deque :+ num
      if (i > 2) {
        if (deque.drop(1).sum > deque.init.sum) {
          has_increased_count += 1
        }
        deque = deque.drop(1)
      }
    }
    println(has_increased_count)
  }
}

object TwentyOneDayOne {
  private val year = 2021
  private val day = 1
}

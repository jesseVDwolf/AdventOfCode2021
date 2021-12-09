package days

/* https://adventofcode.com/2021/day/5 */
class TwentyOneDayEight(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayEight.day
  val year  : Int = TwentyOneDayEight.year
  val input : String = get_input(cookieHeader)

  object Segment extends Enumeration {
    type Segment = Value

    val A,B,C,D,E,F,G = Value
  }

  class Digits {

  }

  override def solve: Unit = {

  }

  override def solve_bonus: Unit = {

  }
}

object TwentyOneDayEight {
  private val year = 2021
  private val day = 5
}
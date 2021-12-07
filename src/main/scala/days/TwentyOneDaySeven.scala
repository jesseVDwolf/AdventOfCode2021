package days

/* https://adventofcode.com/2021/day/7 */
class TwentyOneDaySeven(cookieHeader: Map[String, String]) extends Solution {
  val day: Int = TwentyOneDaySeven.day
  val year: Int = TwentyOneDaySeven.year
  val input: String = get_input(cookieHeader)

  val test_input : String = "16,1,2,0,4,2,7,1,2,14"

  override def solve: Unit = {
    val horizontalPositions : Array[Int] = input.split(',')
      .map(_.trim)
      .map(_.toInt)


  }

  override def solve_bonus: Unit = {

  }
}

object TwentyOneDaySeven {
  private val year = 2021
  private val day = 6
}


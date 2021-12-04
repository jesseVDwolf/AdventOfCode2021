package days

/* https://adventofcode.com/2021/day/2 */
class TwentyOneDayTwo(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayTwo.day
  val year  : Int = TwentyOneDayTwo.year
  val input : String = get_input(cookieHeader)

  override def solve: Unit = {
    val instructions = input.split('\n')
      .map(_.split(' '))
      .map { case Array(ins: String, num: String) => (ins, num.toInt) }

    var dept, horizontal_position  = 0
    instructions.view.foreach {
      case ("forward", num) => horizontal_position += num
      case ("down", num)    => dept += num
      case ("up", num)      => dept -= num
      case _ =>
    }
    println(dept * horizontal_position)
  }

  override def solve_bonus : Unit = {
    val instructions = input.split('\n')
      .map(_.split(' '))
      .map { case Array(ins: String, num: String) => (ins, num.toInt) }

    var aim, dept, horizontal_position  = 0
    instructions.view.foreach {
      case ("forward", num) => horizontal_position += num ; dept += (aim * num)
      case ("down", num)    => aim += num
      case ("up", num)      => aim -= num
      case _ =>
    }
    println(dept * horizontal_position)
  }
}

object TwentyOneDayTwo {
  private val year = 2021
  private val day = 2
}

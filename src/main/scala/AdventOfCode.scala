import scalaj.http._
import scopt.OptionParser
import scala.collection.mutable

// import assignments from each day

abstract class Solution() {
  val input : String
  val day   : Int
  val year  : Int

  def solve       : Unit
  def solve_bonus : Unit
  def get_input(cookieHeader: Map[String, String]) : String = {
    val url = AdventOfCode.AocBaseUrl + year + "/day/" + day + "/input"
    val response : HttpResponse[String] = Http(url).headers(cookieHeader).asString

    response.body
  }
}

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

/* https://adventofcode.com/2021/day/1 */
class TwentyOneDayTwo(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayTwo.day
  val year  : Int = TwentyOneDayTwo.year
  val input : String = get_input(cookieHeader)

  override def solve: Unit = {

  }

  override def solve_bonus : Unit = {

  }
}

object TwentyOneDayTwo {
  private val year = 2021
  private val day = 2
}

case class Config(args: Map[String, String] = Map())

object AdventOfCode {
  val AocBaseUrl  = "https://adventofcode.com/"

  def main(args: Array[String]): Unit = {
    val cookieHeader : Map[String, String] = getCookieHeader(args) match {
      case Some(kv) => Map("cookie" -> kv.map(_.productIterator.mkString("=")).mkString(""))
      case _ => return
    }
    val solutions = List(new TwentyOneDayOne(cookieHeader), new TwentyOneDayTwo(cookieHeader))
    solutions.foreach(solution => println(solution.solve, solution.solve_bonus))

  }

  def getCookieHeader(args: Array[String]) : Option[Map[String, String]] = {
    val parser = new scopt.OptionParser[Config]("scopt") {
      head("scopt", "3.x")
      opt[Map[String, String]]("key")
        .required()
        .valueName("<auth_session_key>")
        .action((x, c) => c.copy(args = x))
        .text("Session key is required")
    }
    parser.parse(args, Config()) match {
      case Some(config) => Some(config.args)
      case _ => None
    }
  }
}
import scalaj.http._
import scopt.OptionParser
import scala.collection.mutable

// import assignments from each day

abstract class Solution() {
  val input : String
  val day   : Int
  val year  : Int

  def solve : Unit
  def solve_bonus : Boolean = { false}
}

/* https://adventofcode.com/2021/day/1 */
class Day(val year: Int, val day: Int, val input: String) extends Solution {

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

  override def solve_bonus : Boolean = {
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
    true
  }
}

object Day {
  def apply(year: Int, day: Int, input: String) : Day = { new Day(year, day, input) }
}

case class Config(args: Map[String, String] = Map())

object AdventOfCode {
  val AocBaseUrl  = "https://adventofcode.com/"

  def main(args: Array[String]): Unit = {
    val header : Map[String, String] = getCookieHeader(args) match {
      case Some(kv) => Map("cookie" -> kv.map(_.productIterator.mkString("=")).mkString(""))
      case _ => return
    }
    val solutions = Map((2021, List(1)))
    solutions.keys.foreach(year => solutions(year).foreach(day => {
      val url = AocBaseUrl + year + "/day/" + day + "/input"
      val response : HttpResponse[String] = Http(url).headers(header).asString

      val daySolution : Day = Day(year, day, response.body)
      daySolution.solve
      daySolution.solve_bonus
    }))
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
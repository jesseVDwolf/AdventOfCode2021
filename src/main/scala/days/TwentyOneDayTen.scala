package days

import scala.collection.mutable
import scala.util.control.Breaks._

/* https://adventofcode.com/2021/day/10 */
class TwentyOneDayTen(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayTen.day
  val year  : Int = TwentyOneDayTen.year
  val input : String = get_input(cookieHeader)

  val test_input : String = """
    |[({(<(())[]>[[{[]{<()<>>
    |[(()[<>])]({[<{<<[]>>(
    |{([(<{}[<>[]}>{[]{[(<()>
    |(((({<>}<{<{<>}{[]{[]{}
    |[[<[([]))<([[{}[[()]]]
    |[{[{({}]{}}([{[{{{}}([]
    |{<[[]]>}<{[{[{[]{()[[[]
    |[<(<(<(<{}))><([]([]()
    |<{([([[(<>()){}]>(<<{{
    |<{([{{}}[<[[[<>{}]]]>[]]
    """.stripMargin

  def solve: Unit = {
    import TwentyOneDayTen.pairs
    val lines = input
      .trim
      .split('\n')
      .map(_.toList)
    val openChars = pairs.keys.toArray
    val closeChars = pairs.values.map(_("close").asInstanceOf[Char]).toArray

    /* returns 0 if match found else points for invalid */
    var syntaxErrorScore = 0
    lines.foreach((line: List[Char]) => {

      var curLine = line
      val stackOpen = mutable.Stack[Char]()
      breakable {
        while (curLine.nonEmpty) {
          if (openChars contains curLine.head)
            stackOpen.push(curLine.head)
          if (closeChars contains curLine.head) {
            if (curLine.head != pairs(stackOpen.top)("close")) {
              syntaxErrorScore += pairs
                .values
                .filter(_("close") == curLine.head)
                .head("errorPoints")
              break ;
            }
            stackOpen.pop()
          }
          curLine = curLine.tail
        }
      }
    })
    println(syntaxErrorScore)
  }

  def solve_bonus: Unit = {
    import TwentyOneDayTen.pairs
    val lines = input
      .trim
      .split('\n')
      .map(_.toList)
    val openChars = pairs.keys.toArray
    val closeChars = pairs.values.map(_("close").asInstanceOf[Char]).toArray

    /* returns 0 if match found else points for invalid */
    val completeScores = mutable.ArrayBuffer[BigInt]()
    lines.foreach((line: List[Char]) => {

      var curLine = line
      val stackOpen = mutable.Stack[Char]()
      breakable {
        while (curLine.nonEmpty) {
          if (openChars contains curLine.head)
            stackOpen.push(curLine.head)
          if (closeChars contains curLine.head) {
            if (curLine.head != pairs(stackOpen.top)("close")) {
              /* discard the corrupted lines */
              break ;
            }
            stackOpen.pop()
          }
          curLine = curLine.tail
        }

        /* finish incomplete line and count the points */
        var acc : BigInt = 0
        stackOpen.foreach((c: Char) => acc = acc * 5 + pairs(c)("completePoints"))
        completeScores += acc
      }
    })
    println(completeScores.sortWith(_ < _)(completeScores.size / 2))
  }
}

object TwentyOneDayTen {

  val pairs: Map[Char, Map[String, Int]] = Map(
    '(' -> Map("close" -> ')', "errorPoints" -> 3, "completePoints" -> 1),
    '[' -> Map("close" -> ']', "errorPoints" -> 57, "completePoints" -> 2),
    '{' -> Map("close" -> '}', "errorPoints" -> 1197, "completePoints" -> 3),
    '<' -> Map("close" -> '>', "errorPoints" -> 25137, "completePoints" -> 4)
  )

  private val year = 2021
  private val day = 10
}
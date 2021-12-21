package days


/* https://adventofcode.com/2021/day/9 */
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
    val lines = test_input
      .trim
      .split('\n')
      .map(_.toList)

    /* returns 0 if match found else points for invalid */
    
  }

  def solve_bonus: Unit = {

  }
}

object TwentyOneDayTen {

  val pairs = Map(
    '(' -> Map("close" -> ')', "points" -> 3),
    '[' -> Map("close" -> ']', "points" -> 57),
    '{' -> Map("close" -> '}', "points" -> 1197),
    '<' -> Map("close" -> '>', "points" -> 25137)
  )

  private val year = 2021
  private val day = 9
}
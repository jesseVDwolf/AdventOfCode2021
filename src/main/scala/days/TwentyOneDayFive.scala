package days

/* https://adventofcode.com/2021/day/5 */
class TwentyOneDayFive(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayFive.day
  val year  : Int = TwentyOneDayFive.year
  val input : String = get_input(cookieHeader)

  val test_input : String = """
    |0,9 -> 5,9
    |8,0 -> 0,8
    |9,4 -> 3,4
    |2,2 -> 2,1
    |7,0 -> 7,4
    |6,4 -> 2,0
    |0,9 -> 2,9
    |3,4 -> 1,4
    |0,0 -> 8,8
    |5,5 -> 8,2
    """.stripMargin.trim

  def getPointRange(coordPointOne: Int, coordPointTwo: Int) : Range = {
    (coordPointOne min coordPointTwo) until (coordPointOne max coordPointTwo) + 1
  }

  /*
  ** Create grid using min(x) - max(x) and min(y) - max(y)
  ** In test_input case that would be:
  **  - min(x) = 0
  **  - max(x) = 9
  **  - min(y) = 0
  **  - max(y) = 9
  */
  def solve: Unit = {
    val inputRanges = input.split('\n')  // Array[String]
      .map(_.split(" -> "))                   // Array[Array[String]]
      .map(_.map(_.split(',')))               // Array[Array[Array[String]]]
      .map(_.map(_.map(_.toInt)))             // Array[Array[Array[Int]]]
      /* For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2 */
      .filter{ (range: Array[Array[Int]]) => (range(0)(0) == range(1)(0)) || (range(0)(1) == range(1)(1)) }

    val xMax : Int = inputRanges.flatten.map{ (point: Array[Int]) => point(0) }.max
    val yMax : Int = inputRanges.flatten.map{ (point: Array[Int]) => point(1) }.max

    /* create 2D matrix of 0's */
    val rows    : Int = yMax + 1
    val columns : Int = xMax + 1
    val gridLines2DMatrix = Array.fill(rows * columns)(0).grouped(columns).toArray

    inputRanges.foreach{ (range: Array[Array[Int]]) => {
      /* get all points in range. Can be a horizontal line (y1 == y2) or vertical line (x1 == x2) */
      val pointsInRange : Array[Array[Int]] = {
        if (range(0)(1) == range(1)(1)) {
          getPointRange(range(0)(0), range(1)(0)).toArray.map{ (x: Int) => Array(x, range(0)(1)) }
        }
        else {
          getPointRange(range(0)(1), range(1)(1)).toArray.map{ (y: Int) => Array(range(0)(0), y) }
        }
      }

      pointsInRange.foreach(point => gridLines2DMatrix(point(1))(point(0)) += 1)
    }}

    val numberPointsLinesOverlap = gridLines2DMatrix.flatten.filter(_ > 1).length
    println(numberPointsLinesOverlap)
  }

  def solve_bonus: Unit = {
    val inputRanges = input.split('\n')  // Array[String]
      .map(_.split(" -> "))                   // Array[Array[String]]
      .map(_.map(_.split(',')))               // Array[Array[Array[String]]]
      .map(_.map(_.map(_.toInt)))             // Array[Array[Array[Int]]]

    val xMax : Int = inputRanges.flatten.map{ (point: Array[Int]) => point(0) }.max
    val yMax : Int = inputRanges.flatten.map{ (point: Array[Int]) => point(1) }.max

    /* create 2D matrix of 0's */
    val rows    : Int = yMax + 1
    val columns : Int = xMax + 1
    val gridLines2DMatrix = Array.fill(rows * columns)(0).grouped(columns).toArray

    inputRanges.foreach{ (range: Array[Array[Int]]) => {
      /* get all points in range. Can be a horizontal line (y1 == y2) or vertical line (x1 == x2) */
      val pointsInRange : Array[Array[Int]] = {
        if (range(0)(1) == range(1)(1)) {
          getPointRange(range(0)(0), range(1)(0)).toArray.map{ (x: Int) => Array(x, range(0)(1)) }
        }
        else if (range(0)(0) == range(1)(0)) {
          getPointRange(range(0)(1), range(1)(1)).toArray.map{ (y: Int) => Array(range(0)(0), y) }
        }
        else {
          val xPointRange = getPointRange(range(0)(0), range(1)(0))
          val yPointRange = getPointRange(range(0)(1), range(1)(1))

          if ((range(0)(1) - range(1)(1)) == (range(0)(0) - range(1)(0))) {
            (xPointRange zip yPointRange).toArray.map{ (xy: Tuple2[Int, Int]) => Array(xy._1, xy._2) }
          }
          else {
            (xPointRange zip yPointRange.reverse).toArray.map{ (xy: Tuple2[Int, Int]) => Array(xy._1, xy._2) }
          }
        }
      }

      pointsInRange.foreach(point => gridLines2DMatrix(point(1))(point(0)) += 1)
    }}

    val numberPointsLinesOverlap = gridLines2DMatrix.flatten.filter(_ > 1).length
    println(numberPointsLinesOverlap)
  }
}

object TwentyOneDayFive {
  private val year = 2021
  private val day = 5
}

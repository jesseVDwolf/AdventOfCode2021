package days

/* https://adventofcode.com/2021/day/9 */
class TwentyOneDayNine(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayNine.day
  val year  : Int = TwentyOneDayNine.year
  val input : String = get_input(cookieHeader)

  val test_input : String = """
    |2199943210
    |3987894921
    |9856789892
    |8767896789
    |9899965678
    """.stripMargin

  def solve: Unit = {
    val heightMap: Array[Array[Int]] = input
      .trim
      .split('\n')
      .map(_.toArray)
      .map(_.map(_.toInt - 48))

    def isLowestPoint(heightMap: Array[Array[Int]], value: Int, coords:Tuple2[Int, Int]*): Boolean = {
      var countLower = 0

      for (coord <- coords) {
        if (heightMap(coord._2)(coord._1) > value)
          countLower += 1
      }
      countLower == coords.length
    }

    var sumRiskLevels = 0
    val rows = heightMap.length - 1
    val cols = heightMap(0).length - 1
    heightMap.view.flatten.zipWithIndex.foreach{
      case (point: Int, idx: Int) => {
        val curRow = idx / (cols + 1)
        val curCol = idx % (cols + 1)
        val coords = (curCol, curRow)
        
        val valueAtLocation = heightMap(curRow)(curCol)
        coords match {
          /* corners with only two adjacent locations */
          case (0, 0) => {
            if (isLowestPoint(heightMap, valueAtLocation, (curCol +1, curRow), (curCol, curRow +1)))
              sumRiskLevels += valueAtLocation + 1
          }
          case (0, `rows`) => {
            if (isLowestPoint(heightMap, valueAtLocation, (curCol +1, curRow), (curCol, curRow -1)))
              sumRiskLevels += valueAtLocation + 1
          }
          case (`cols`, 0) => {
            if (isLowestPoint(heightMap, valueAtLocation, (curCol -1, curRow), (curCol, curRow +1)))
              sumRiskLevels += valueAtLocation + 1
          }
          case (`cols`, `rows`) => {
            if (isLowestPoint(heightMap, valueAtLocation, (curCol -1, curRow), (curCol, curRow -1)))
              sumRiskLevels += valueAtLocation + 1
          }
          /* sides with only three adjacent locations */
          case (0, _) => {
            if (isLowestPoint(heightMap, valueAtLocation,
               (curCol, curRow -1), (curCol +1, curRow), (curCol, curRow +1)))
               sumRiskLevels += valueAtLocation + 1
          }
          case (`cols`, _) => {
            if (isLowestPoint(heightMap, valueAtLocation,
               (curCol, curRow -1), (curCol -1, curRow), (curCol, curRow +1)))
               sumRiskLevels += valueAtLocation + 1
          }
          case (_, 0) => {
            if (isLowestPoint(heightMap, valueAtLocation,
               (curCol -1, curRow), (curCol, curRow +1), (curCol +1, curRow)))
               sumRiskLevels += valueAtLocation + 1
          }
          case (_, `rows`) => {
            if (isLowestPoint(heightMap, valueAtLocation,
               (curCol -1, curRow), (curCol, curRow -1), (curCol +1, curRow)))
               sumRiskLevels += valueAtLocation + 1
          }
          /* all locations in the middle with four adjacent locations */
          case _ => {
            if (isLowestPoint(heightMap, valueAtLocation,
               (curCol, curRow -1), (curCol, curRow +1), (curCol -1, curRow), (curCol +1, curRow)))
              sumRiskLevels += valueAtLocation + 1
          }
        }
      }
    }
    println(sumRiskLevels)
  }

  def solve_bonus: Unit = {

  }

}

object TwentyOneDayNine {

  private val year = 2021
  private val day = 9
}

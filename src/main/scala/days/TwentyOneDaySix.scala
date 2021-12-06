package days

import scala.collection.mutable

/* https://adventofcode.com/2021/day/6 */
class TwentyOneDaySix(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDaySix.day
  val year  : Int = TwentyOneDaySix.year
  val input : String = get_input(cookieHeader)

  val test_input : String = "3,4,3,1,2"

  /*
  Fish properties:
    1. Makes new fish every 7 days
    2. Has an internal timer that decreases each day
    3. Fish can be created in two ways:
      3.1. From the initial shoal
      3.2. From another fish
    4. Fish reset their own timer to 6 after creating
       a new fish themselves.
   */
  class FishShoal(private var fishShoal: mutable.HashMap[Int, Int]) {

    def initFishShoal(initialState: Array[Int]) : Unit = {
      initialState.foreach{
        fishState => fishShoal.filter{
          case (day,_) => day == fishState
        }.map{
          case (day: Int, _: Int) => fishShoal(day) += 1
        }
      }
      println(fishShoal)
    }
  }

  object FishShoal {
    private val maxInternalTimer : Int = 8

    def apply() = new FishShoal(getEmptyShoal)

    def getEmptyShoal: mutable.HashMap[Int, Int] = {
      (0 until maxInternalTimer)
        .toArray
        .map{ (day: Int) => mutable.HashMap(day -> 0) } reduce (_ ++ _)
    }
  }

  override def solve: Unit = {
    val initialState = test_input.split(',').map(_.toInt)
    val fishShoal : FishShoal = FishShoal()

    fishShoal.initFishShoal(initialState)
  }

  override def solve_bonus: Unit = {

  }
}

object TwentyOneDaySix {
  private val year = 2021
  private val day = 6
}
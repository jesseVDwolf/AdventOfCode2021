package days

import scala.collection.mutable

/* https://adventofcode.com/2021/day/6 */
class TwentyOneDaySix(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDaySix.day
  val year  : Int = TwentyOneDaySix.year
  val input : String = get_input(cookieHeader)

  val test_input : String = "3,4,3,1,2"

  /*
  ** Fish properties:
  **  1. Makes new fish every 7 days
  **  2. Has an internal timer that decreases each day
  **  3. Fish can be created in two ways:
  **    3.1. From the initial shoal
  **    3.2. From another fish
  **  4. Fish reset their own timer to 6 after creating
  **     a new fish themselves.
  */
  class FishShoal(private var fishShoal: mutable.HashMap[Int, BigInt]) {

    def getShoal: mutable.Map[Int, BigInt] = fishShoal

    def createFishShoal(initialState: Array[Int]) : Unit = {
      initialState.foreach{
        fishState => fishShoal.filter{
          case (day,_) => day == fishState
        }.map{
          case (day: Int, _: BigInt) => fishShoal(day) += 1
        }
      }
    }

    /*
    ** 1. Save the number of fishes at timer 0
    ** 2. Each timer will now have the amount of fishes from the previous day
    ** 3. Add to timer 6 and replace for 8, the amount of fishes saved from day 0
    */
    def simulateDay : Unit = {
      var fishAtZero : BigInt = 0

      fishShoal.keys.foreach {
        case key@FishShoal.maxInternalTimer => fishShoal(key) = 0
        case key@0 => fishAtZero += fishShoal(key) ; fishShoal(key) = fishShoal(key + 1)
        case key@_ => fishShoal(key) = fishShoal(key + 1)
      }
      fishShoal(FishShoal.maxInternalTimer) = fishAtZero
      fishShoal(FishShoal.resetToAfterDeath) += fishAtZero
    }
  }

  object FishShoal {
    private val maxInternalTimer  : Int = 8
    private val resetToAfterDeath : Int = 6

    def apply() = new FishShoal(getEmptyShoal)

    def getEmptyShoal: mutable.HashMap[Int, BigInt] = {
      (0 until maxInternalTimer + 1)
        .toArray
        .map{ (day: Int) => mutable.HashMap(day -> BigInt(0)) } reduce (_ ++ _)
    }
  }

  override def solve: Unit = {
    val initialState = input.split(',').map(_.trim).map(_.toInt)
    val fishShoal : FishShoal = FishShoal()

    val simulationDays = 80
    fishShoal.createFishShoal(initialState)
    for (_ <- 0 until simulationDays) { fishShoal.simulateDay }

    println(fishShoal.getShoal.values.sum)
  }

  override def solve_bonus: Unit = {
    val initialState = input.split(',').map(_.trim).map(_.toInt)
    val fishShoal : FishShoal = FishShoal()

    val simulationDays = 256
    fishShoal.createFishShoal(initialState)
    for (_ <- 0 until simulationDays) { fishShoal.simulateDay }

    println(fishShoal.getShoal.values.sum)
  }
}

object TwentyOneDaySix {
  private val year = 2021
  private val day = 6
}
package days

import scala.collection.mutable

/* https://adventofcode.com/2021/day/8 */
class TwentyOneDayEight(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayEight.day
  val year  : Int = TwentyOneDayEight.year
  val input : String = get_input(cookieHeader)

  val test_input : String = """
    |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
    |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
    |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
    |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
    |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
    |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
    |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
    |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
    |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
    |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
    """.stripMargin

    /*
     0:      1:      2:      3:      4:
   aaaa    ....    aaaa    aaaa    ....
  b    c  .    c  .    c  .    c  b    c
  b    c  .    c  .    c  .    c  b    c
   ....    ....    dddd    dddd    dddd
  e    f  .    f  e    .  .    f  .    f
  e    f  .    f  e    .  .    f  .    f
   gggg    ....    gggg    gggg    ....

    5:      6:      7:      8:      9:
   aaaa    aaaa    aaaa    aaaa    aaaa
  b    .  b    .  .    c  b    c  b    c
  b    .  b    .  .    c  b    c  b    c
   dddd    dddd    ....    dddd    dddd
  .    f  e    f  .    f  e    f  .    f
  .    f  e    f  .    f  e    f  .    f
   gggg    gggg    ....    gggg    gggg
    */
  
  override def solve: Unit = {
    val inputEntries : Array[Array[Array[String]]] = input
      .trim
      .split('\n')
      .map(_.split('|'))
      .map(_.map(_.trim))
      .map(_.map(_.split(' ')))

    /* find the digits 1,4,7,8 */
    val allDigits = inputEntries.map( (entry: Array[Array[String]]) => entry(1) )
      .map( (entry: Array[String]) => {
        entry.filter( (s: String) => (
          s.length == TwentyOneDayEight.digits(1).segments.length ||
          s.length == TwentyOneDayEight.digits(4).segments.length ||
          s.length == TwentyOneDayEight.digits(7).segments.length ||
          s.length == TwentyOneDayEight.digits(8).segments.length
        ))
      }
    ) reduce(_ ++ _)

    println(allDigits.length)
  }

  override def solve_bonus: Unit = {
    import TwentyOneDayEight.Segment
    import TwentyOneDayEight.Segment._

    val inputEntries : Array[Array[Array[String]]] = test_input
      .trim
      .split('\n')
      .map(_.split('|'))
      .map(_.map(_.trim))
      .map(_.map(_.split(' ')))

    val segmentMapping : Array[mutable.HashMap[String, Object]] = Segment
      .values
      .toArray
      .map( (s: Segment.Value) =>
        mutable.HashMap(
          "segment" -> s,
          "mappedSegment" -> NONE,
          "possibleSegments" -> mutable.HashSet[Segment.Value]()
        )
      )

    val digitMapping : Array[(TwentyOneDayEight.Digit, Int)] = TwentyOneDayEight
      .digits
      .zipWithIndex
      .sortWith(_._1.segments.length < _._1.segments.length)

    /* Map("Segment" -> {Segment.Value}, "mappedSegment" -> {Segment.Value}, "possibleSegments" -> List[Segment.Value]) */

    // 1. Start at Digit with least amount of segements.
    // 2. Add possible segments to digitMapping
    // 3. When multiple digits have the same amount of digits, try to isolate one
    //    based on previously found segments not present in others.
    // 4. Remove possible segments when you can be sure that a pssible segment maps to an actual segment
    //    i.e
    //    [acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf]
    //    - There is only one digit with 2 segments. This is the digit 1. Segments ab make up digit 1.
    //    - There is only one digit with 3 segments. This is the digit 7. Segments dab make up digit 7.
    //    - When you filter out all corresponding segments, only segment "d" remains. This is actually
    //      segment A. Map("Segment" -> A, "mappedSegment" -> D, "possibleSegments" -> List())

    inputEntries.foreach((entry: Array[Array[String]]) => {
      val uniqueCombinations = entry(0).sortWith(_.length < _.length)

      /* for each combination find digits with corresponding amount of segments */
      uniqueCombinations.foreach((combination: String) => {
        val digits = digitMapping.filter(_._1.segments.length == combination.length)

        /* for each digit found add possible options to segment mapping possibleSegments */
        digits.foreach((digit: (TwentyOneDayEight.Digit, Int)) => {

          digit._1.segments.foreach((seg: Segment) => {
            /* i.e convert "be" to Array(B, E) */
            val newPossibleSegments = combination.map(charToSegment(_)).toSet
            val indexOfSegment = segmentMapping.indexWhere(_("segment") == seg)

            segmentMapping(indexOfSegment)("possibleSegments") = segmentMapping(indexOfSegment)("possibleSegments")
              .asInstanceOf[mutable.HashSet[Segment.Value]] ++ newPossibleSegments
          })
        })

        println("test\n");
      })
    })

    println(segmentMapping)
  }
}

object TwentyOneDayEight {

  object Segment extends Enumeration {
    type Segment = Value

    val A,B,C,D,E,F,G, NONE = Value

    def charToSegment(c: Char) : Segment.Value = Segment(c - 97)
  }

  case class Digit(val segments: Array[Segment.Value]);

  import Segment._

  private val year = 2021
  private val day = 8
  private val digits : Array[Digit] = Array(
    Digit(Array(A,B,C,E,F,G)),  // 0
    Digit(Array(C,F)),          // 1
    Digit(Array(A,C,D,E,G)),    // ..
    Digit(Array(A,C,D,F,G)),
    Digit(Array(B,C,D,F)),
    Digit(Array(A,B,D,F,G)),
    Digit(Array(A,B,D,E,F,G)),
    Digit(Array(A,C,F)),
    Digit(Array(A,B,C,D,E,F,G)),
    Digit(Array(A,B,C,D,F,G))
  )
}
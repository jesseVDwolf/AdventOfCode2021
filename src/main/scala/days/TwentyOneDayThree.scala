package days

/* https://adventofcode.com/2021/day/3 */
class TwentyOneDayThree(cookieHeader: Map[String, String]) extends Solution {
  val day   : Int = TwentyOneDayThree.day
  val year  : Int = TwentyOneDayThree.year
  val input : String = get_input(cookieHeader)

  object Diagnostic extends Enumeration {
    type Diagnostic = Value

    val GAMMA, EPSILON, O2_RATING, CO2_RATING = Value
  }

  def getDiagnostic(binaryTable: Array[Array[Int]], diagnosticType: Diagnostic.Value) : Int = {
    val amountBits = 12
    var diagnosticValue = 0
    val sumList: Array[Int] = Array.fill(amountBits)(0)

    for (y <- binaryTable.indices) {
      for (x <- 0 until amountBits) {
        sumList(x) += binaryTable(y)(x)
      }
    }
    sumList.foreach(num => {
      diagnosticValue = diagnosticValue << 1
      diagnosticType match {
        case Diagnostic.GAMMA   => if (num >= (binaryTable.length * 0.5)) diagnosticValue += 1
        case Diagnostic.EPSILON => if (num <= (binaryTable.length * 0.5)) diagnosticValue += 1
        case _ =>
      }
    })
    diagnosticValue
  }

  override def solve: Unit = {
    val binaryTable = input.split('\n')
      .map(_.toArray)
      .map(_.map(_.toInt - 48))

    println(getDiagnostic(binaryTable, Diagnostic.GAMMA) * getDiagnostic(binaryTable, Diagnostic.EPSILON))
  }

  /* convert the string with the binary representation to decimal (i.e) 010010011001 to 1177 */
  def binaryStringToDecimal(binaryString: String) : Int = {
    var decimal = 0

    binaryString.foreach(bit => { decimal = decimal << 1 ; decimal += (bit.toInt - 48) })
    decimal
  }

  /* based on the diagnosticType filter out the rows with the least or most common bit for that column */
  def getDiagnosticBonus(table: Array[Array[Int]], diagnosticType: Diagnostic.Value) : Int = {
    var bitColumn = 0
    val amountBits = 12
    var binaryTable = table

    while (bitColumn < amountBits && binaryTable.length > 1) {
      val transposedView = binaryTable.transpose.view
      val bitCriteriaMatched : Boolean = diagnosticType match {
        case Diagnostic.O2_RATING   => transposedView(bitColumn).sum >= (transposedView(bitColumn).length * 0.5)
        case Diagnostic.CO2_RATING  => transposedView(bitColumn).sum < (transposedView(bitColumn).length * 0.5)
        case _ => false
      }

      binaryTable = binaryTable.filter(_(bitColumn) == (if (bitCriteriaMatched) 1 else 0))
      bitColumn += 1
    }
    binaryStringToDecimal(binaryTable(0).mkString(""))
  }

  override def solve_bonus : Unit = {
    val binaryTable = input.split('\n')
      .map(_.toArray)
      .map(_.map(_.toInt - 48))

    println(getDiagnosticBonus(binaryTable, Diagnostic.O2_RATING) * getDiagnosticBonus(binaryTable, Diagnostic.CO2_RATING))
  }
}

object TwentyOneDayThree {
  private val year = 2021
  private val day = 3
}

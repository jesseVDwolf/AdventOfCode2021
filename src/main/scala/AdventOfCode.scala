import days._

// import assignments from each day

case class Config(args: Map[String, String] = Map())

object AdventOfCode {

  def main(args: Array[String]): Unit = {
    val cookieHeader : Map[String, String] = getCookieHeader(args) match {
      case Some(kv) => Map("cookie" -> kv.map(_.productIterator.mkString("=")).mkString(""))
      case _ => return
    }
    val solutions = List(
      // new TwentyOneDayOne(cookieHeader), new TwentyOneDayTwo(cookieHeader),
      // new TwentyOneDayThree(cookieHeader), new TwentyOneDayFour(cookieHeader),
      // new TwentyOneDayFive(cookieHeader), new TwentyOneDaySix(cookieHeader),
      // new TwentyOneDaySeven(cookieHeader),
      new TwentyOneDayEight(cookieHeader)
    )
    solutions.foreach(solution => { solution.solve ; solution.solve_bonus })

  }

  /* TODO specify specific day to run and / or only bonus */
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
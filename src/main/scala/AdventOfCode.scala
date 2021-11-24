import scalaj.http._
import scopt.OptionParser

// import assignments from each day

case class Config(args: Map[String, String] = Map())

object AdventOfCode {
  val AocBaseUrl  = "https://adventofcode.com/"
  val year        = "2020"

  def main(args: Array[String]): Unit = {
    val cookie: Option[Map[String, String]] = getCookieHeader(args) match {
      case None => return
      case Some(kv) => {
        val header : Map[String, String] = Map("cookie" -> kv.map { case (key, value) => s"$key=$value"}.mkString(""))
        val url = AocBaseUrl + year + "/day/" + "8" + "/input"
        val response : HttpResponse[String] = Http(url).headers(header).asString

        println(response.body)
        println(response.headers)
        println(response.code)
        return
      }
    }
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
      case None => None
    }
  }

}
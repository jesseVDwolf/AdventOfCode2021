package days

import scalaj.http.{Http, HttpResponse}

abstract class Solution() {
  val input : String
  val day   : Int
  val year  : Int

  val AocBaseUrl  = "https://adventofcode.com/"

  def solve       : Unit
  def solve_bonus : Unit
  def get_input(cookieHeader: Map[String, String]) : String = {
    val url = AocBaseUrl + year + "/day/" + day + "/input"
    val response : HttpResponse[String] = Http(url).headers(cookieHeader).asString

    response.body
  }
}

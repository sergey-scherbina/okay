package okay2.http

/** a url's decoded path segments and query parameters, as a route reads
 * them (okay-http's Urls): None where the escaping is malformed */
object Urls {
  def segments(url: String): Option[Vector[String]] = Route.segmentsOf(url)
  def params(url: String): Option[Map[String, Vector[String]]] = Route.paramsOf(url)
}

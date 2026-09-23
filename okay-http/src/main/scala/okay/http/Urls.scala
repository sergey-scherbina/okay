package okay.http

/**
 * A URL's path and query, decoded — the same reading `Route` does, as a
 * public door for code that is not a `Route` (okay-scala2-http, whose
 * Scala 2 callers match on segments with an extractor instead). One
 * implementation: these delegate to `Route`'s own, so a percent-decoding
 * rule changed there is changed here.
 */
object Urls:

  /** the path's segments, each percent-decoded after the split; `None`
   * if a segment's escape is malformed */
  def segments(url: String): Option[Vector[String]] = Route.segmentsOf(url)

  /** the query's parameters, each name with all its values in order;
   * `None` if an escape is malformed */
  def params(url: String): Option[Map[String, Vector[String]]] = Route.paramsOf(url)

package okay.dlm.remote

/**
 * THE WIRE, AS A SEAM: one POST, one body back — everything a remote
 * judge or encoder needs, and nothing a test cannot fake. Ours is
 * `java.net.http` with a deadline; a suite passes `Wire.canned` and
 * asserts on the request it sees.
 *
 * A failure is a `Left` with a sentence: a status code, a timeout, a
 * refused connection. Nothing here throws across the seam — the judge
 * above turns a `Left` into an abstention, and `Judge.guarded` turns
 * a run of them into a cooldown.
 */
trait Wire:
  def post(url: String, headers: Map[String, String], body: String): Either[String, String]

object Wire:

  /** OURS BY DEFAULT: java.net.http, one request, one deadline */
  given ours: Wire = http()

  def http(timeoutMs: Long = 10000L,
           client: java.net.http.HttpClient = java.net.http.HttpClient.newHttpClient()): Wire =
    (url, headers, body) =>
      try
        val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url))
          .timeout(java.time.Duration.ofMillis(timeoutMs))
          .POST(java.net.http.HttpRequest.BodyPublishers.ofString(body))
        headers.foreach((k, v) => b.header(k, v))
        val r = client.send(b.build(), java.net.http.HttpResponse.BodyHandlers.ofString())
        if r.statusCode() / 100 == 2 then Right(r.body())
        else Left(s"$url: HTTP ${r.statusCode()} ${r.body().take(200)}")
      catch case e: Exception => Left(s"$url: ${e.getClass.getSimpleName} ${Option(e.getMessage).getOrElse("")}".trim)

  /** a wire that answers from a function of the request and records
   * what it was asked — the test double */
  final class Canned(answer: (String, Map[String, String], String) => Either[String, String]) extends Wire:
    var seen = Vector.empty[(String, Map[String, String], String)]
    def post(url: String, headers: Map[String, String], body: String): Either[String, String] =
      seen :+= (url, headers, body)
      answer(url, headers, body)

  def canned(reply: String): Canned = Canned((_, _, _) => Right(reply))
  def failing(why: String): Canned = Canned((_, _, _) => Left(why))

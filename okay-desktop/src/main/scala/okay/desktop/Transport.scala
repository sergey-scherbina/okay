package okay.desktop

/**
 * WHAT THE WINDOW ASKS ITS SERVICE, whichever road it is on
 * (specs/app-in-process.md): the file a Save dialog writes, a picked
 * file posted, "is it busy", a menu's POST. In the process (`inProcess`)
 * or over a port (`http`, the browser road's), the same calls.
 */
trait Transport:
  /** where the service's pages are: `app://<host>` or `http://127.0.0.1:<port>` */
  def base: String
  /** a request; `url` under `base` or a path */
  def send(method: String, url: String, headers: Seq[(String, String)] = Nil, body: Array[Byte] = Array.empty): Answer
  /** the URL the window loads to SHOW this answer: in the process the
   * answer is held for it and not asked again; over a port it is asked again */
  def open(a: Answer): String

object Transport:
  def inProcess(s: InProcess.Server): Transport = new Transport:
    def base: String = s.base
    def send(method: String, url: String, headers: Seq[(String, String)], body: Array[Byte]): Answer =
      s.send(method, url, headers, body)
    def open(a: Answer): String =
      if a.url.startsWith(s.base) then s.hold(a.url, a)
      a.url

  /** over a port, with the window's own cookies, redirects followed */
  def http(base0: String, cookies: java.net.CookieHandler): Transport = new Transport:
    private val client = java.net.http.HttpClient.newBuilder().cookieHandler(cookies)
      .followRedirects(java.net.http.HttpClient.Redirect.NORMAL).build()
    def base: String = base0
    def send(method: String, url: String, headers: Seq[(String, String)], body: Array[Byte]): Answer =
      val to = if url.contains("://") then url else base0 + (if url.startsWith("/") then url else "/" + url)
      val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(to))
      headers.foreach((k, v) => b.header(k, v))
      b.method(method.toUpperCase,
        if body.isEmpty then java.net.http.HttpRequest.BodyPublishers.noBody()
        else java.net.http.HttpRequest.BodyPublishers.ofByteArray(body))
      scala.util.Try(client.send(b.build(), java.net.http.HttpResponse.BodyHandlers.ofByteArray())) match
        case scala.util.Success(r) =>
          import scala.jdk.CollectionConverters.*
          Answer(r.statusCode, r.headers.map.asScala.toSeq.flatMap((k, vs) => vs.asScala.map(k -> _)), r.body, r.uri.toString)
        case scala.util.Failure(e) =>
          Answer(503, Seq("content-type" -> "text/plain; charset=utf-8"),
            s"the service did not answer: ${e.getMessage}".getBytes("UTF-8"), to)
    def open(a: Answer): String = a.url

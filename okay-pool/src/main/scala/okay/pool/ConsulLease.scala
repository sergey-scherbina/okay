package okay.pool

import okay.cluster.Lease
import okay.codec.Json
import okay.codec.Json.{JArr, JNum, JObj, JStr}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * A CONSUL SESSION HOLDING A KV LOCK, AS THE SEAT
 * (specs/cluster-pool.md, stage 5) — the same three-method seam
 * `KubeLease` fills for Kubernetes, over Consul's own session API
 * (`/v1/session/...`) and its KV store's `?acquire=`/`?release=` lock
 * primitives, which exist for exactly this purpose.
 *
 * PLAIN, SYNCHRONOUS `java.net.http.HttpClient`, the same choice
 * `KubeLease` makes and for the identical reason: `Lease` is plain
 * synchronous Scala, and a blocking round trip once per epoch is
 * already the documented cost of `held`.
 *
 * THE FENCING TOKEN IS THE KV ENTRY'S OWN `LockIndex` — Consul's own
 * counter that increments EVERY time the key is successfully
 * acquired, never on a mere value write, which is a fencing token
 * Consul built in rather than one assembled from a general-purpose
 * revision number. `held` does not re-read it: a Consul lock lives
 * and dies with its SESSION, so a session that still renews still
 * holds whatever it acquired, and a failed renew (404, the session
 * expired or was invalidated) is the one signal that matters.
 */
final class ConsulLease(base: String, key: String, holder: String,
                        ttlSeconds: Int = 15) extends Lease:

  // FORCED TO HTTP/1.1, the same fix `KubeLease` needed: the JDK
  // client's default HTTP/2-with-upgrade attempt reads a 2xx answered
  // mid-upgrade as "invalid upgrade response" against an HTTP/1.1-only
  // agent, which Consul's own HTTP API is.
  private val client: HttpClient = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()
  @volatile private var session: String = ""

  private def send(method: String, path: String, body: Option[String]): HttpResponse[String] =
    val b = HttpRequest.newBuilder(URI.create(s"$base$path")).header("content-type", "application/json")
    val pub = body.fold(HttpRequest.BodyPublishers.noBody())(HttpRequest.BodyPublishers.ofString(_, UTF_8))
    method match
      case "GET" => b.GET()
      case "PUT" => b.PUT(pub)
    client.send(b.build(), HttpResponse.BodyHandlers.ofString(UTF_8))

  private def field(j: Json, k: String): Option[Json] = j match
    case JObj(fs) => fs.collectFirst { case (`k`, v) => v }
    case _ => None

  private def createSession(): Option[String] =
    val body = Json.print(JObj(Vector("Name" -> JStr(s"okay-pool-$key"), "TTL" -> JStr(s"${ttlSeconds}s"),
      "LockDelay" -> JStr("0s"))))
    val res = send("PUT", "/v1/session/create", Some(body))
    if res.statusCode() != 200 then None
    else field(Json.parse(res.body()), "ID").collect { case JStr(id) => id }

  private def destroySession(id: String): Unit =
    try send("PUT", s"/v1/session/destroy/$id", None): Unit
    catch case _: Exception => ()

  def take(): Option[Long] =
    createSession() match
      case None => None
      case Some(sid) =>
        val res = send("PUT", s"/v1/kv/$key?acquire=$sid", Some(holder))
        if res.statusCode() == 200 && res.body().trim == "true" then
          session = sid
          val got = send("GET", s"/v1/kv/$key", None)
          Json.parse(got.body()) match
            case JArr(Vector(entry)) => field(entry, "LockIndex").collect { case JNum(n) => n.toLong }
            case _ => Some(0L)
        else
          destroySession(sid)
          None

  def held(term: Long): Boolean =
    if session.isEmpty then false
    else send("PUT", s"/v1/session/renew/$session", None).statusCode() == 200

  override def release(term: Long): Unit =
    if session.nonEmpty then
      try
        send("PUT", s"/v1/kv/$key?release=$session", Some(holder)): Unit
        destroySession(session)
      catch case _: Exception => ()   // best-effort: the session also just expires

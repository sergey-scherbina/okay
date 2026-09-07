package okay.acme

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Http, Method, Request, Response}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * A certificate authority small enough to read: RFC 8555's shapes,
 * a nonce it actually checks, the http-01 fetch done for real over a
 * socket, and a certificate signed by its own CA key with openssl.
 *
 * It is a TEST DOUBLE and says so: it does not verify the JWS
 * signature (the client's signing is exercised by okay-security's own
 * suite), and it accepts any account. What it does prove is the part
 * a client gets wrong — the order of the steps, the nonce discipline,
 * the token round trip, and the CSR the CA has to be able to sign.
 */
final class FakeCa(dir: Path, refuse: Option[String] = None):

  Files.createDirectories(dir)

  private def sh(cmd: String*): Boolean =
    try ProcessBuilder(cmd*).redirectErrorStream(true).start().waitFor() == 0
    catch case _: Exception => false

  val ready: Boolean =
    sh("openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
      "-keyout", s"$dir/ca-key.pem", "-out", s"$dir/ca.pem", "-days", "1",
      "-subj", "/CN=Okay Fake CA")

  @volatile private var nonces = Set.empty[String]
  @volatile private var issued: Option[String] = None
  @volatile private var pendingToken: Option[String] = None
  @volatile var accounts = 0
  @volatile var orders = 0
  @volatile var fetchedTokens = Vector.empty[String]

  private def base(port: Int) = s"http://127.0.0.1:$port"

  private def json(status: Int, body: Json, headers: Vector[(String, String)] = Vector.empty): Response =
    Response(status, ("Content-Type" -> "application/json") +: (nonce() +: headers),
      Http.one(Json.print(body).getBytes(UTF_8)))

  private def nonce(): (String, String) =
    val n = java.util.UUID.randomUUID().toString.replace("-", "")
    nonces = nonces + n
    ("Replay-Nonce", n)

  /** the CA fetches the challenge from the client's own server, which
   * is the whole point of http-01 -- `challengePort` is where that is */
  def routes(challengePort: Int)(using CanBlock): PartialFunction[Request, Response ! Async] = {
    case r if r.method == Method.Head && path(r.url) == "/nonce" =>
      pure(Response(200, Vector(nonce()), Http.one(Array.emptyByteArray)))

    case r if path(r.url) == "/directory" =>
      val b = authority(r)
      pure(json(200, Json.JObj(Vector(
        "newNonce" -> Json.JStr(s"$b/nonce"),
        "newAccount" -> Json.JStr(s"$b/account"),
        "newOrder" -> Json.JStr(s"$b/order")))))

    case r if path(r.url) == "/account" =>
      accounts += 1
      pure(json(201, Json.JObj(Vector("status" -> Json.JStr("valid"))),
        Vector("Location" -> s"${authority(r)}/account/1")))

    case r if path(r.url) == "/order" =>
      orders += 1
      val b = authority(r)
      pure(json(201, Json.JObj(Vector(
        "status" -> Json.JStr("pending"),
        "authorizations" -> Json.JArr(Vector(Json.JStr(s"$b/authz/1"))),
        "finalize" -> Json.JStr(s"$b/finalize"))),
        Vector("Location" -> s"$b/order/1")))

    case r if path(r.url) == "/authz/1" =>
      val b = authority(r)
      refuse match
        case Some(why) =>
          pure(Response(403, Vector("Content-Type" -> "application/problem+json", nonce()),
            Http.one(Json.print(Json.JObj(Vector(
              "type" -> Json.JStr("urn:ietf:params:acme:error:unauthorized"),
              "detail" -> Json.JStr(why)))).getBytes(UTF_8))))
        case None =>
          // pending until the token has been fetched, then valid --
          // exactly the poll a client must be able to sit through
          val status = if pendingToken.exists(fetchedTokens.contains) then "valid" else "pending"
          pure(json(200, Json.JObj(Vector(
            "status" -> Json.JStr(status),
            "challenges" -> Json.JArr(Vector(Json.JObj(Vector(
              "type" -> Json.JStr("http-01"),
              "url" -> Json.JStr(s"$b/challenge/1"),
              "token" -> Json.JStr(token())))))))))

    case r if path(r.url) == "/challenge/1" =>
      // the CA now does what a CA does: fetches the token over HTTP
      val t = token()
      val fetched = fetch(s"${base(challengePort)}/.well-known/acme-challenge/$t")
      if fetched.exists(_.startsWith(t + ".")) then fetchedTokens = fetchedTokens :+ t
      pure(json(200, Json.JObj(Vector("status" -> Json.JStr("pending")))))

    case r if path(r.url) == "/finalize" =>
      issued = signFromCsr(r)
      pure(json(200, Json.JObj(Vector("status" -> Json.JStr("processing")))))

    case r if path(r.url) == "/order/1" =>
      val b = authority(r)
      pure(json(200, Json.JObj(
        if issued.isDefined then Vector(
          "status" -> Json.JStr("valid"), "certificate" -> Json.JStr(s"$b/cert/1"))
        else Vector("status" -> Json.JStr("processing")))))

    case r if path(r.url) == "/cert/1" =>
      pure(Response(200, Vector("Content-Type" -> "application/pem-certificate-chain", nonce()),
        Http.one(issued.getOrElse("").getBytes(UTF_8))))
  }

  private def token(): String =
    pendingToken.getOrElse {
      val t = "tok-" + java.util.UUID.randomUUID().toString.take(8)
      pendingToken = Some(t)
      t
    }

  /** the JWS payload, unwrapped -- the double does not verify the
   * signature, only reads what was signed */
  private def payloadOf(r: Request): Option[Json] =
    try
      val body = Json.parse(String(r.body.bytes, UTF_8))
      Acme.str(body, "payload").map { p =>
        val raw = java.util.Base64.getUrlDecoder.decode(p)
        if raw.isEmpty then Json.JObj(Vector.empty) else Json.parse(String(raw, UTF_8))
      }
    catch case _: Exception => None

  private def signFromCsr(r: Request): Option[String] =
    for
      payload <- payloadOf(r)
      csr <- Acme.str(payload, "csr")
    yield
      val der = dir.resolve("req.der")
      val out = dir.resolve("leaf.pem")
      Files.write(der, java.util.Base64.getUrlDecoder.decode(csr)): Unit
      val ok = sh("openssl", "x509", "-req", "-in", der.toString, "-inform", "DER",
        "-CA", s"$dir/ca.pem", "-CAkey", s"$dir/ca-key.pem", "-CAcreateserial",
        "-days", "1", "-copy_extensions", "copyall", "-out", out.toString)
      if ok then Files.readString(out) + Files.readString(dir.resolve("ca.pem")) else ""

  private def fetch(url: String)(using CanBlock): Option[String] =
    try Some(Resource.run[String, Pure](okay.jetty.Jetty.http().map { h =>
      Async.run[String, Pure](h.send(Request.get(url)).flatMap(Http.text)).runWith
    }).runWith)
    catch case _: Throwable => None

  private def authority(r: Request): String =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("host") => s"http://$v" }
      .getOrElse("http://127.0.0.1")

  private def path(url: String): String = url.takeWhile(_ != '?')

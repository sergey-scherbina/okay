package okay.acme

import okay.*
import okay.given
import okay.codec.Json
import okay.conf.{Schemes, Secret, Secrets}
import okay.http.{Body, Http, Method, Request, Response}

import java.nio.charset.StandardCharsets.UTF_8

/**
 * `Acme.Dns` for the providers people actually run — Cloudflare,
 * deSEC and Route 53 (acme-dns-providers).
 *
 * Three, not one, and that is the point. The seam shipped with NO
 * implementation because a seam with one favourite baked in is worse
 * than a seam with none; three with a credential each is a menu, and
 * the fourth is still the deployment's own `Dns` — nothing here is
 * privileged, they are all just callers of the same trait.
 *
 * What they have in common, and what a fourth should copy:
 *  - the credential is a `Secret`, resolved at construction through
 *    the resolver the deployment already uses; a token never inlines.
 *  - a failure is the PROVIDER's own sentence. "Cloudflare answered
 *    403: Invalid access token" is actionable; "DNS write failed" is
 *    not.
 * The platform's `CanBlock` is taken at CONSTRUCTION rather than by
 * the trait's methods: `Dns` stays a plain seam anyone can implement
 * (a test double writes to a map), and a provider that has to park on
 * a socket is built where a server already has the capability.
 *
 *  - `propagation` is that provider's documented figure, and
 *    overridable, because the number is theirs and changes without
 *    telling us.
 *  - the record this wrote is remembered, so `removeTxt` takes down
 *    exactly what went up rather than everything at that name — a
 *    zone can hold TXT records that are none of our business.
 */
object Providers:

  /** the JSON a provider answered, or its status and body when that
   * is all there is */
  private def call(http: Http, r: Request, who: String)(using CanBlock): Either[String, Json] =
    try
      val res = Async.run[Response, Pure](http.send(r)).runWith
      val text = Async.run[String, Pure](Http.text(res)).runWith
      if res.status >= 400 then Left(s"$who answered ${res.status}: ${detail(text)}")
      else Right(if text.isEmpty then Json.JObj(Vector.empty) else parse(text))
    catch case e: Throwable => Left(s"$who could not be reached: ${Option(e.getMessage).getOrElse(e.toString)}")

  private def parse(text: String): Json =
    try Json.parse(text) catch case _: Throwable => Json.JStr(text.take(300))

  /** the sentence inside a provider's error, whatever shape it took */
  private def detail(text: String): String =
    val j = parse(text)
    Acme.str(j, "detail")
      .orElse(Acme.str(j, "message"))
      .orElse(Acme.field(j, "errors").collect {
        case Json.JArr(es) if es.nonEmpty => es.flatMap(e => Acme.str(e, "message")).mkString("; ")
      }.filter(_.nonEmpty))
      .getOrElse(text.trim.take(300))

  private def json(url: String, method: Method, body: String, headers: Seq[(String, String)]): Request =
    Request(method, url, ("Content-Type" -> "application/json") +: headers, Body.Text(body))

  // ---- Cloudflare -------------------------------------------------

  /**
   * Cloudflare, by API token (not the legacy global key): a Bearer
   * token scoped to Zone:DNS:Edit for the one zone this writes in.
   *
   * The zone ID is the CALLER's, deliberately: looking it up by name
   * would be a second request, a second permission and a second way
   * to fail, for a value an operator reads off the dashboard once.
   */
  def cloudflare(http: Http, zoneId: String, token: Secret,
                 secrets: Secrets = Schemes.all(),
                 propagationOf: java.time.Duration = java.time.Duration.ofSeconds(10),
                 endpoint: String = "https://api.cloudflare.com")
                (using CanBlock): Either[String, Acme.Dns] =
    secrets.get(token).map { value =>
      new Acme.Dns:
        private val base = s"$endpoint/client/v4/zones/$zoneId/dns_records"
        private val auth = Seq("Authorization" -> s"Bearer $value")
        private val written = new java.util.concurrent.ConcurrentHashMap[String, String]

        def putTxt(name: String, txt: String): Either[String, Unit] =
          call(http, json(base, Method.Post,
            Json.print(Json.JObj(Vector(
              "type" -> Json.JStr("TXT"),
              "name" -> Json.JStr(name),
              "content" -> Json.JStr(txt),
              "ttl" -> Json.JNum(60)))), auth), "Cloudflare")
            .map { answer =>
              Acme.field(answer, "result").flatMap(Acme.str(_, "id"))
                .foreach(id => written.put(name, id): Unit)
            }

        def removeTxt(name: String): Unit =
          Option(written.remove(name)).foreach { id =>
            call(http, Request(Method.Delete, s"$base/$id", auth), "Cloudflare"): Unit
          }

        override def propagation: java.time.Duration = propagationOf
    }

  // ---- deSEC ------------------------------------------------------

  /**
   * deSEC, by API token in the `Token` header. Its rrset API is
   * declarative — a PUT states what the record set IS — so the delete
   * is the same call with an empty list, and there is no id to keep.
   *
   * `domain` is the zone; the name written is split against it,
   * because deSEC takes the subname rather than the full name.
   */
  def desec(http: Http, domain: String, token: Secret,
            secrets: Secrets = Schemes.all(),
            propagationOf: java.time.Duration = java.time.Duration.ofSeconds(30),
            endpoint: String = "https://desec.io")
           (using CanBlock): Either[String, Acme.Dns] =
    secrets.get(token).map { value =>
      new Acme.Dns:
        private val url = s"$endpoint/api/v1/domains/$domain/rrsets/"
        private val auth = Seq("Authorization" -> s"Token $value")

        private def set(name: String, records: Vector[String]): Either[String, Unit] =
          val subname = name.stripSuffix("." + domain).stripSuffix(".")
          call(http, json(url, Method.Put,
            Json.print(Json.JArr(Vector(Json.JObj(Vector(
              "subname" -> Json.JStr(if subname == name then subname else subname),
              "type" -> Json.JStr("TXT"),
              "ttl" -> Json.JNum(3600),
              // deSEC stores the QUOTED presentation form, as the DNS
              // wire does; a bare value is rejected
              "records" -> Json.JArr(records.map(r => Json.JStr("\"" + r + "\"")))))))), auth),
            "deSEC").map(_ => ())

        def putTxt(name: String, txt: String): Either[String, Unit] = set(name, Vector(txt))

        def removeTxt(name: String): Unit = set(name, Vector.empty): Unit

        override def propagation: java.time.Duration = propagationOf
    }

  // ---- Route 53 ---------------------------------------------------

  /**
   * Route 53, signed with the repository's OWN SigV4 (okay-blob) at
   * `service = "route53"` — the alternative was a second signature
   * implementation, which is exactly what one shared signer exists to
   * prevent.
   *
   * Its API is XML and stays XML here: two templates, no parser. The
   * response is only read for its status, because a change that was
   * accepted is accepted — Route 53's own INSYNC polling is about
   * propagation, which is what `propagation` covers.
   */
  def route53(http: Http, hostedZoneId: String, accessKey: String, secret: Secret,
              secrets: Secrets = Schemes.all(),
              propagationOf: java.time.Duration = java.time.Duration.ofSeconds(60),
              endpoint: String = "https://route53.amazonaws.com")
             (using CanBlock): Either[String, Acme.Dns] =
    secrets.get(secret).map { secretValue =>
      new Acme.Dns:
        private val creds = okay.blob.SigV4.Creds(accessKey, secretValue)
        private val written = new java.util.concurrent.ConcurrentHashMap[String, String]
        private val path = s"/2013-04-01/hostedzone/${hostedZoneId.stripPrefix("/hostedzone/")}/rrset/"

        private def change(action: String, name: String, txt: String): Either[String, Unit] =
          val body =
            s"""<?xml version="1.0" encoding="UTF-8"?>
               |<ChangeResourceRecordSetsRequest xmlns="https://route53.amazonaws.com/doc/2013-04-01/">
               |<ChangeBatch><Changes><Change>
               |<Action>$action</Action>
               |<ResourceRecordSet>
               |<Name>${name.stripSuffix(".")}.</Name><Type>TXT</Type><TTL>60</TTL>
               |<ResourceRecords><ResourceRecord><Value>"$txt"</Value></ResourceRecord></ResourceRecords>
               |</ResourceRecordSet>
               |</Change></Changes></ChangeBatch>
               |</ChangeResourceRecordSetsRequest>""".stripMargin
          val host = java.net.URI.create(endpoint).getHost
          val stamp = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
            .withZone(java.time.ZoneOffset.UTC).format(java.time.Instant.now())
          val headers = okay.blob.SigV4.sign("POST", path, Seq.empty,
            Seq("host" -> host, "content-type" -> "text/xml"),
            okay.blob.SigV4.sha256Hex(body.getBytes(UTF_8)),
            region = "us-east-1", stamp = stamp, creds = creds, service = "route53")
          call(http, Request(Method.Post, s"$endpoint$path",
            headers ++ Seq("content-type" -> "text/xml"), Body.Text(body)), "Route 53").map(_ => ())

        def putTxt(name: String, txt: String): Either[String, Unit] =
          change("UPSERT", name, txt).map(_ => written.put(name, txt): Unit)

        /** a delete states the record it removes, so what went up is
         * what comes down -- a zone holds records that are not ours */
        def removeTxt(name: String): Unit =
          Option(written.remove(name)).foreach(txt => change("DELETE", name, txt): Unit)

        override def propagation: java.time.Duration = propagationOf
    }

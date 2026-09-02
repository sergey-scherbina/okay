package okay.blob

import okay.{!, +, Async, Chunk, Produce, Stream, Writer, effect, pure}
import okay.given
import okay.http.{Body, Http, Method, Request, Response}
import scala.collection.immutable.ArraySeq

/**
 * The S3 engine (specs/blob.md stage 1): the REST subset this seam
 * needs — PUT/GET/HEAD/DELETE/ListObjectsV2 — over the one http
 * client, signed by our own SigV4. Path-style URLs, so MinIO, R2 and
 * AWS all fit; one engine, the whole S3-compatible family.
 *
 * Puts buffer: okay-http's Body is deliberately unstreamed
 * (specs/http.md), and the payload hash a signature wants is of the
 * WHOLE body anyway — so the producer is drained first and the hash
 * is real, never UNSIGNED-PAYLOAD. When the http client learns
 * streaming bodies, multipart and constant-memory puts arrive
 * together. Gets stream: the response body is already chunked.
 */
final class S3(http: Http, endpoint: String, bucket: String, region: String,
               creds: SigV4.Creds,
               clock: () => java.time.Instant = () => java.time.Instant.now) extends Blob {

  private type F = Produce + Async

  private val hostHeader =
    val u = java.net.URI(endpoint)
    if u.getPort == -1 then u.getHost else s"${u.getHost}:${u.getPort}"

  private def stamp(): String =
    java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
      .withZone(java.time.ZoneOffset.UTC).format(clock())

  private def signed(method: Method, path: String,
                     query: Seq[(String, String)] = Nil,
                     payload: Array[Byte] = Array.empty): Request =
    val hash = if payload.isEmpty then SigV4.emptyHash else SigV4.sha256Hex(payload)
    val auth = SigV4.sign(method.toString.toUpperCase, path, query,
      Seq("host" -> hostHeader), hash, region, stamp(), creds)
    val qs = if query.isEmpty then ""
      else "?" + query.map((k, v) => s"${SigV4.uriEncode(k)}=${SigV4.uriEncode(v)}").mkString("&")
    Request(method, s"$endpoint${SigV4.uriEncode(path, keepSlash = true)}$qs",
      auth, if payload.isEmpty then Body.Empty else Body.Bytes(ArraySeq.unsafeWrapArray(payload)))

  private def keyPath(key: String) = s"/$bucket/$key"

  def put(key: String, bytes: Chunk[Byte] ! F): Etag ! Async =
    drainBytes(bytes).flatMap { payload =>
      http.send(signed(Method.Put, keyPath(key), payload = payload)).flatMap { r =>
        r.release.map { _ =>
          if !r.ok then throw IllegalStateException(s"put '$key': HTTP ${r.status}")
          Etag(r.header("etag").getOrElse("").stripPrefix("\"").stripSuffix("\""))
        }
      }
    }

  def get(key: String, range: Option[(Long, Long)] = None): Either[String, Unit] ! F =
    val headers = range.map((from, until) => "range" -> s"bytes=$from-${until - 1}").toSeq
    // range participates in the signature (a signed header like host)
    val auth = SigV4.sign("GET", keyPath(key), Nil,
      Seq("host" -> hostHeader) ++ headers, SigV4.emptyHash, region, stamp(), creds)
    val send = http.send(Request(Method.Get,
      s"$endpoint${SigV4.uriEncode(keyPath(key), keepSlash = true)}", auth ++ headers))
    // a row is a union: Async + Produce IS Produce + Async, so the
    // ascriptions are the compiler's equality, not casts
    (okay.!.widen[Response, Async, Produce](send): Response ! F).flatMap { r =>
      if r.status == 404 then
        okay.!.widen[Either[String, Unit], Async, Produce](
          r.release.map(_ => Left(s"no such key '$key'"))): Either[String, Unit] ! F
      else if !r.ok && r.status != 206 then
        okay.!.widen[Either[String, Unit], Async, Produce](
          r.release.map(_ => Left(s"get '$key': HTTP ${r.status}"))): Either[String, Unit] ! F
      else emit(r.body)
    }

  /** re-produce a response body Source as this seam's chunk stream */
  private def emit(src: okay.Source[Chunk[Byte]]): Either[String, Unit] ! F =
    (okay.!.widen[Either[Unit, (Chunk[Byte], okay.Source[Chunk[Byte]])], Async, Produce](
      Writer.uncons[Chunk[Byte], Unit, Async](src)): Either[Unit, (Chunk[Byte], okay.Source[Chunk[Byte]])] ! F)
      .flatMap {
        case Left(_) => pure(Right(()))
        case Right((c, more)) =>
          if c.isEmpty then emit(more)
          else effect[F, Chunk[Byte]](c).flatMap(_ => emit(more))
      }

  def head(key: String): Option[Meta] ! Async =
    http.send(signed(Method.Head, keyPath(key))).flatMap { r =>
      r.release.map { _ =>
        if r.status == 404 then None
        else if !r.ok then throw IllegalStateException(s"head '$key': HTTP ${r.status}")
        else Some(Meta(key,
          r.header("content-length").flatMap(_.toLongOption).getOrElse(0L),
          Etag(r.header("etag").getOrElse("").stripPrefix("\"").stripSuffix("\"")),
          r.header("last-modified").flatMap(rfc1123).getOrElse(0L)))
      }
    }

  private def rfc1123(s: String): Option[Long] =
    try Some(java.time.ZonedDateTime.parse(s,
      java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME).toInstant.toEpochMilli)
    catch case _: Exception => None

  def list(prefix: String): Chunk[Meta] ! F =
    def page(token: Option[String]): Chunk[Meta] ! F =
      val query = Seq("list-type" -> "2", "prefix" -> prefix) ++
        token.map("continuation-token" -> _)
      val send = http.send(signed(Method.Get, s"/$bucket", query.sortBy(_._1)))
        .flatMap(r => Http.text(r).map(t => (r.status, t)))
      (okay.!.widen[(Int, String), Async, Produce](send): (Int, String) ! F).flatMap { (status, xml) =>
          if status != 200 then throw IllegalStateException(s"list '$prefix': HTTP $status")
          val metas = contents(xml)
          val next =
            if tag(xml, "IsTruncated").contains("true") then tag(xml, "NextContinuationToken")
            else None
          val chunk: Chunk[Meta] = ArraySeq.unsafeWrapArray(metas.toArray[Meta])
          (metas.isEmpty, next) match
            case (true, None) => pure(okay.Chunks.emptyChunk)
            case (true, Some(t)) => page(Some(t))
            case (false, None) => effect[F, Chunk[Meta]](chunk)
            case (false, Some(t)) => effect[F, Chunk[Meta]](chunk).flatMap(_ => page(Some(t)))
        }
    page(None)

  def delete(key: String): Unit ! Async =
    http.send(signed(Method.Delete, keyPath(key))).flatMap { r =>
      r.release.map { _ =>
        if !r.ok && r.status != 404 then
          throw IllegalStateException(s"delete '$key': HTTP ${r.status}")
      }
    }

  // ── the narrow XML the S3 list answers — flat, known tags ─────────

  private def contents(xml: String): Vector[Meta] =
    blocks(xml, "Contents").map { c =>
      Meta(
        tag(c, "Key").map(unescape).getOrElse(""),
        tag(c, "Size").flatMap(_.toLongOption).getOrElse(0L),
        Etag(tag(c, "ETag").map(unescape).getOrElse("")
          .stripPrefix("&quot;").stripPrefix("\"").stripSuffix("&quot;").stripSuffix("\"")),
        tag(c, "LastModified").flatMap(iso).getOrElse(0L))
    }

  private def blocks(xml: String, name: String): Vector[String] =
    val open = s"<$name>"; val close = s"</$name>"
    val out = Vector.newBuilder[String]
    var i = xml.indexOf(open)
    while i >= 0 do
      val j = xml.indexOf(close, i)
      if j < 0 then i = -1
      else
        out += xml.substring(i + open.length, j)
        i = xml.indexOf(open, j)
    out.result()

  private def tag(xml: String, name: String): Option[String] =
    blocks(xml, name).headOption

  private def unescape(s: String): String = s
    .replace("&quot;", "\"").replace("&lt;", "<").replace("&gt;", ">")
    .replace("&#39;", "'").replace("&amp;", "&")   // amp LAST, the usual rule

  private def iso(s: String): Option[Long] =
    try Some(java.time.Instant.parse(s).toEpochMilli)
    catch case _: Exception => None

  private def drainBytes(p: Chunk[Byte] ! F): Array[Byte] ! Async =
    val out = java.io.ByteArrayOutputStream()
    val S = summon[Stream[[X] =>> X ! F, Async]]
    def go(rest: Chunk[Byte] ! F): Array[Byte] ! Async =
      S.uncons(rest).flatMap {
        case None => pure(out.toByteArray)
        case Some((c, more)) =>
          out.write(c.toArray)
          go(more)
      }
    go(p)
}

object S3:
  /** the wiring form (ctx-everywhere): the engine awaiting the one
   * http client — provide(http){ S3.wired(...) } */
  def wired(endpoint: String, bucket: String, region: String,
            creds: SigV4.Creds): okay.http.Http ?=> S3 =
    S3(summon[okay.http.Http], endpoint, bucket, region, creds)

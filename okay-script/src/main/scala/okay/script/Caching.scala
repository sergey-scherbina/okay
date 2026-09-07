package okay.script

import java.nio.file.{Files, Path}
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

/** Conditional requests and cache directives -- the validators a
 * response carries and the reading of the ones a request sends. See
 * specs/okay-script.md "Caching".
 */
object Caching:

  private val httpDate = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC)

  def formatDate(millis: Long): String = httpDate.format(Instant.ofEpochMilli(millis).atZone(ZoneOffset.UTC))

  /** an HTTP-date, or `None` -- damage is "not fresh", never a throw */
  def parseDate(s: String): Option[Long] =
    try Some(Instant.from(httpDate.parse(s.trim)).toEpochMilli)
    catch case _: Exception => None

  /** a strong ETag for bytes: sha-256, base64url, quoted */
  def etagOf(bytes: Array[Byte]): String =
    val d = java.security.MessageDigest.getInstance("SHA-256").digest(bytes)
    "\"" + java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(d).take(22) + "\""

  /** a strong ETag for a file, without reading it: size and mtime --
   * what a file server has always used, and what makes a large static
   * file cheap to validate */
  def etagOf(f: Path): String =
    val size = Files.size(f)
    val mtime = Files.getLastModifiedTime(f).toMillis
    "\"" + java.lang.Long.toHexString(size) + "-" + java.lang.Long.toHexString(mtime) + "\""

  /** does `If-None-Match` name this ETag? `*` matches anything; a
   * list is comma-separated; a weak prefix compares by its opaque
   * part (a weak validator is enough for a 304, per RFC 9110) */
  def matches(ifNoneMatch: String, etag: String): Boolean =
    val want = strip(etag)
    ifNoneMatch.split(",").iterator.map(_.trim).exists(t => t == "*" || strip(t) == want)

  private def strip(t: String): String = if t.startsWith("W/") then t.drop(2) else t

  /** is the client's copy still fresh by date? */
  def notModifiedSince(ifModifiedSince: String, lastModified: Long): Boolean =
    parseDate(ifModifiedSince).exists(since => lastModified / 1000 <= since / 1000)

  /** `cache: <seconds>` / `cache: none` from a page's front-matter */
  def maxAge(front: Map[String, String]): Option[Int] =
    front.get("cache").map(_.trim).flatMap {
      case "none" | "no" | "off" => None
      case v => v.toIntOption.filter(_ >= 0)
    }

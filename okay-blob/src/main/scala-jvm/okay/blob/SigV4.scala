package okay.blob

/**
 * AWS Signature Version 4, ourselves (specs/blob.md, Decisions): the
 * needed subset is an HMAC chain over a canonical request — stable,
 * documented, and PINNED here by the AWS documentation's own test
 * vectors rather than by trust. One implementation signs for AWS S3,
 * MinIO, R2 and every S3-compatible endpoint.
 *
 * S3 specifics honored: the canonical URI is SINGLE-encoded (s3 is
 * the one service that does not double-encode), and the payload hash
 * always travels as x-amz-content-sha256.
 */
object SigV4 {

  final case class Creds(accessKey: String, secret: String)

  private val unreserved =
    (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')).toSet ++ Set('-', '.', '_', '~')

  /** RFC 3986 strict; `keepSlash` for the path, where segments keep
   * their separators */
  def uriEncode(s: String, keepSlash: Boolean = false): String =
    val sb = StringBuilder()
    for b <- s.getBytes("UTF-8") do
      val c = b.toChar
      if unreserved(c) || (keepSlash && c == '/') then sb.append(c)
      else sb.append(f"%%${b & 0xff}%02X")
    sb.toString

  def sha256Hex(bytes: Array[Byte]): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(bytes)
      .iterator.map(b => f"$b%02x").mkString

  val emptyHash: String = sha256Hex(Array.empty)

  private def hmac(key: Array[Byte], data: String): Array[Byte] =
    val mac = javax.crypto.Mac.getInstance("HmacSHA256")
    mac.init(javax.crypto.spec.SecretKeySpec(key, "HmacSHA256"))
    mac.doFinal(data.getBytes("UTF-8"))

  private def hex(bs: Array[Byte]): String = bs.iterator.map(b => f"$b%02x").mkString

  /**
   * The headers a request must gain: x-amz-date, x-amz-content-sha256
   * and Authorization. `headers` must already include host; `stamp`
   * is basic ISO-8601 (yyyyMMdd'T'HHmmss'Z').
   */
  def sign(method: String, path: String, query: Seq[(String, String)],
           headers: Seq[(String, String)], payloadHash: String,
           region: String, stamp: String, creds: Creds,
           service: String = "s3"): Seq[(String, String)] =
    val date = stamp.take(8)
    val all = (headers ++ Seq(
      "x-amz-date" -> stamp, "x-amz-content-sha256" -> payloadHash))
      .map((k, v) => (k.toLowerCase, v.trim))
      .sortBy(_._1)
    val signedHeaders = all.map(_._1).mkString(";")
    val canonicalQuery = query
      .map((k, v) => (uriEncode(k), uriEncode(v)))
      .sorted.map((k, v) => s"$k=$v").mkString("&")
    val canonical =
      s"""$method
         |${uriEncode(path, keepSlash = true)}
         |$canonicalQuery
         |${all.map((k, v) => s"$k:$v\n").mkString}
         |$signedHeaders
         |$payloadHash""".stripMargin
    val scope = s"$date/$region/$service/aws4_request"
    val toSign =
      s"""AWS4-HMAC-SHA256
         |$stamp
         |$scope
         |${sha256Hex(canonical.getBytes("UTF-8"))}""".stripMargin
    val signingKey =
      hmac(hmac(hmac(hmac(("AWS4" + creds.secret).getBytes("UTF-8"), date),
        region), service), "aws4_request")
    val signature = hex(hmac(signingKey, toSign))
    Seq(
      "x-amz-date" -> stamp,
      "x-amz-content-sha256" -> payloadHash,
      "authorization" -> (s"AWS4-HMAC-SHA256 Credential=${creds.accessKey}/$scope, " +
        s"SignedHeaders=$signedHeaders, Signature=$signature"))
}

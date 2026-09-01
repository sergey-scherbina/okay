package okay.security

import okay.codec.Json

/**
 * JWT over the Crypto seam: HS256 and RS256 in stage 0 (ES256 is
 * staged — JOSE's raw R||S signature format is not JCA's DER, and
 * that conversion deserves its own tested task).
 *
 * Verification is TOTAL and refuses by name: tampered, wrong key,
 * wrong audience, expired, not yet valid, `alg: none`, and the
 * classic alg/key CONFUSION (an HS256 token presented against an RSA
 * key verifies the signature with the public key as an HMAC secret —
 * refused here by matching the alg to the KEY'S kind, not the
 * token's word).
 */
object Jwt {

  enum Key:
    case Hmac(secret: Array[Byte])
    case RsaPublic(key: java.security.PublicKey)
    case RsaPair(pub: java.security.PublicKey, priv: java.security.PrivateKey)

  private val enc = java.util.Base64.getUrlEncoder.withoutPadding
  private val dec = java.util.Base64.getUrlDecoder

  private def b64(bytes: Array[Byte]): String = enc.encodeToString(bytes)
  private def b64s(s: String): String = b64(s.getBytes("UTF-8"))
  private def unb64(s: String): Option[Array[Byte]] =
    try Some(dec.decode(s)) catch case _: IllegalArgumentException => None

  /** sign claims into a compact JWT */
  def sign(claims: Claims, key: Key, kid: Option[String] = None)(using c: Crypto): String =
    val alg = key match
      case Key.Hmac(_) => "HS256"
      case _ => "RS256"
    val header = Json.JObj(Vector(
      "alg" -> Json.JStr(alg), "typ" -> Json.JStr("JWT")) ++
      kid.map(k => "kid" -> Json.JStr(k)).toVector)
    val signing = b64s(Json.print(header)) + "." + b64s(Json.print(Claims.json(claims)))
    val sig = key match
      case Key.Hmac(secret) => c.hmacSha256(secret, signing.getBytes("UTF-8"))
      case Key.RsaPair(_, priv) => c.signRsaSha256(priv, signing.getBytes("UTF-8"))
      case Key.RsaPublic(_) =>
        throw IllegalArgumentException("a public key cannot sign")   // a broken invariant, not hostile input
    signing + "." + b64(sig)

  /**
   * Verify a compact JWT: the key is looked up by the token's kid
   * ("" when it has none), the signature is checked against the
   * KEY'S algorithm, then time and audience. Hostile input of any
   * shape is a `No`, never a throw.
   */
  def verify(token: String, keys: String => Option[Key],
             audience: Option[String], now: Long, skew: Long = 60)
            (using c: Crypto): Verified =
    token.split('.') match
      case Array(h, p, s) =>
        (unb64(h), unb64(p), unb64(s)) match
          case (Some(hb), Some(pb), Some(sig)) =>
            val header = Json.parse(String(hb, "UTF-8"))
            val alg = Claims.str(header, "alg").getOrElse("")
            val kid = Claims.str(header, "kid").getOrElse("")
            keys(kid) match
              case None => Verified.No(s"no key for kid '$kid'")
              case Some(key) =>
                val signed = (token.take(h.length + 1 + p.length)).getBytes("UTF-8")
                val holds = (key, alg) match
                  // the KEY decides the algorithm; the token only gets
                  // to agree — that is what defuses the confusion
                  case (Key.Hmac(secret), "HS256") =>
                    Crypto.constantTimeEquals(c.hmacSha256(secret, signed), sig)
                  case (Key.RsaPublic(pub), "RS256") => c.verifyRsaSha256(pub, signed, sig)
                  case (Key.RsaPair(pub, _), "RS256") => c.verifyRsaSha256(pub, signed, sig)
                  case _ => false
                if !holds then Verified.No(s"signature does not verify (alg '$alg')")
                else
                  val claims = Claims.of(Json.parse(String(pb, "UTF-8")))
                  timeAndAudience(claims, audience, now, skew)
          case _ => Verified.No("not base64url")
      case _ => Verified.No("not a compact JWT")

  private def timeAndAudience(claims: Claims, audience: Option[String],
                              now: Long, skew: Long): Verified =
    claims.expires match
      case Some(exp) if now > exp + skew => Verified.No("expired")
      case _ => claims.notBefore match
        case Some(nbf) if now < nbf - skew => Verified.No("not yet valid")
        case _ => audience match
          case Some(aud) if !claims.audience.contains(aud) =>
            Verified.No(s"audience does not include '$aud'")
          case _ =>
            val id = claims.subject.getOrElse("")
            val name = Claims.str(claims.json, "name").getOrElse(id)
            Verified.Ok(Principal(id, name, claims))
}

/**
 * JWKS — the key set an issuer publishes, parsed into verifying keys
 * (RSA in stage 0). `fetch` is one Http call; `parse` is total: a
 * damaged or unsupported entry is skipped, not thrown.
 */
object Jwks {

  import okay.{!, Async}
  import okay.http.{Http, Request}

  def parse(j: Json): Map[String, Jwt.Key] =
    Claims.field(j, "keys") match
      case Some(Json.JArr(entries)) => entries.flatMap(rsa).toMap
      case _ => Map.empty

  private def rsa(j: Json): Option[(String, Jwt.Key)] =
    for
      kty <- Claims.str(j, "kty") if kty == "RSA"
      n <- Claims.str(j, "n").flatMap(b64uint)
      e <- Claims.str(j, "e").flatMap(b64uint)
      key <- try Some(java.security.KeyFactory.getInstance("RSA").generatePublic(
        java.security.spec.RSAPublicKeySpec(n.bigInteger, e.bigInteger)))
      catch case _: Exception => None
    yield (Claims.str(j, "kid").getOrElse(""), Jwt.Key.RsaPublic(key))

  private def b64uint(s: String): Option[BigInt] =
    try Some(BigInt(1, java.util.Base64.getUrlDecoder.decode(s)))
    catch case _: IllegalArgumentException => None

  def fetch(http: Http, url: String): Map[String, Jwt.Key] ! Async =
    http.send(Request.get(url)).flatMap(r => okay.http.Http.text(r))
      .map(t => parse(Json.parse(t)))
}

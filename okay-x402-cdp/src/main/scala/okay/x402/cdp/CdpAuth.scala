package okay.x402.cdp

import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyFactory, MessageDigest, PrivateKey, SecureRandom, Signature}
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64
import okay.codec.Json
import okay.codec.Json.*
import org.bouncycastle.asn1.{ASN1Integer, ASN1Sequence}
import org.bouncycastle.asn1.pkcs.PrivateKeyInfo
import org.bouncycastle.asn1.x509.AlgorithmIdentifier
import org.bouncycastle.asn1.x9.X9ObjectIdentifiers

/**
 * Coinbase CDP's two tokens, built as the official SDK builds them
 * (coinbase/cdp-sdk, java `com.coinbase.cdp.auth`: `JwtGenerator`,
 * `WalletJwtGenerator`, `KeyParser`, `JsonUtils`) — the SDK is the
 * reference, where the prose docs disagree with it (`uri` vs `uris`).
 *
 * - the BEARER token, from the API key: EdDSA for an Ed25519 key (64
 *   bytes, base64: seed ‖ public key) or ES256 for an EC key (PEM);
 *   header `alg`, `kid` = key id, `typ`, `nonce` (16 random bytes, hex);
 *   claims `sub` = key id, `iss` = `cdp`, `iat` = `nbf` = now, `exp` =
 *   now + 120, `uris` = [`METHOD host/path`];
 * - the WALLET token (`X-Wallet-Auth`), from the Wallet Secret (base64
 *   PKCS#8, EC P-256): ES256; claims `iat` = `nbf` = now, `jti`, `uris`,
 *   and `reqHash` = SHA-256 hex of the request body with its keys SORTED
 *   recursively and printed compactly.
 */
object CdpAuth:
  private val random = SecureRandom()
  private def b64url(b: Array[Byte]): String = Base64.getUrlEncoder.withoutPadding.encodeToString(b)
  private def hex(b: Array[Byte]): String = b.map(x => f"${x & 0xFF}%02x").mkString
  private def randomHex(n: Int): String = { val b = new Array[Byte](n); random.nextBytes(b); hex(b) }
  def sha256Hex(s: String): String = hex(MessageDigest.getInstance("SHA-256").digest(s.getBytes(UTF_8)))

  /** the key of an API key's secret: PEM (EC, SEC1 or PKCS#8) or Ed25519
   * as 64 base64 bytes — the two forms CDP issues */
  def apiKey(secret: String): Either[String, PrivateKey] =
    val s = secret.replace("\\n", "\n").trim
    scala.util.Try {
      if s.contains("-----BEGIN") then
        val der = Base64.getMimeDecoder.decode(s.linesIterator.filterNot(_.startsWith("-----")).mkString)
        val pkcs8 =
          if s.contains("BEGIN EC PRIVATE KEY") then   // SEC1: wrap it as PKCS#8 for the JDK
            PrivateKeyInfo(AlgorithmIdentifier(X9ObjectIdentifiers.id_ecPublicKey, X9ObjectIdentifiers.prime256v1),
              ASN1Sequence.getInstance(der)).getEncoded
          else der
        KeyFactory.getInstance("EC").generatePrivate(PKCS8EncodedKeySpec(pkcs8))
      else
        val raw = Base64.getDecoder.decode(s)
        require(raw.length == 64, s"an Ed25519 API key secret is 64 bytes, this one is ${raw.length}")
        // PKCS#8 for Ed25519: SEQ { 0, SEQ { 1.3.101.112 }, OCTET { OCTET seed } }
        val prefix = Array[Byte](0x30, 0x2e, 0x02, 0x01, 0x00, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x04, 0x22, 0x04, 0x20)
        KeyFactory.getInstance("Ed25519").generatePrivate(PKCS8EncodedKeySpec(prefix ++ raw.take(32)))
    }.toEither.left.map(e => s"the API key secret is neither an EC PEM nor a 64-byte Ed25519 key: ${e.getMessage}")

  /** the Wallet Secret: base64 PKCS#8, EC P-256 */
  def walletKey(secret: String): Either[String, PrivateKey] =
    scala.util.Try(KeyFactory.getInstance("EC").generatePrivate(PKCS8EncodedKeySpec(Base64.getDecoder.decode(secret.trim))))
      .toEither.left.map(e => s"the Wallet Secret is not a base64 PKCS#8 EC key: ${e.getMessage}")

  private def isEc(k: PrivateKey) = k.getAlgorithm == "EC" || k.getAlgorithm == "ECDSA"

  /** a compact JWS: `header.claims.signature`, ES256 as the raw `r ‖ s`
   * JWS requires (the JDK answers DER), EdDSA as the JDK answers it */
  def jws(key: PrivateKey, header: Json, claims: Json): String =
    val input = s"${b64url(Json.print(header).getBytes(UTF_8))}.${b64url(Json.print(claims).getBytes(UTF_8))}"
    val sig =
      if isEc(key) then
        val s = Signature.getInstance("SHA256withECDSA")
        s.initSign(key); s.update(input.getBytes(UTF_8))
        val seq = ASN1Sequence.getInstance(s.sign())
        def word(i: Int) =
          val b = ASN1Integer.getInstance(seq.getObjectAt(i)).getPositiveValue.toByteArray.dropWhile(_ == 0)
          new Array[Byte](32 - b.length) ++ b
        word(0) ++ word(1)
      else
        val s = Signature.getInstance("Ed25519")
        s.initSign(key); s.update(input.getBytes(UTF_8)); s.sign()
    s"$input.${b64url(sig)}"

  /** `METHOD host/path`, the form both tokens name the request by */
  def uri(method: String, host: String, path: String): String = s"$method $host$path"

  def bearer(keyId: String, key: PrivateKey, method: String, host: String, path: String, now: Long): String =
    jws(key,
      JObj(Vector("alg" -> JStr(if isEc(key) then "ES256" else "EdDSA"), "kid" -> JStr(keyId),
        "typ" -> JStr("JWT"), "nonce" -> JStr(randomHex(16)))),
      JObj(Vector("sub" -> JStr(keyId), "iss" -> JStr("cdp"), "iat" -> JNum(now.toDouble),
        "nbf" -> JNum(now.toDouble), "exp" -> JNum((now + 120).toDouble),
        "uris" -> JArr(Vector(JStr(uri(method, host, path)))))))

  /** keys sorted, recursively — the body `reqHash` is taken over */
  def sorted(j: Json): Json = j match
    case JObj(fs) => JObj(fs.sortBy(_._1).map((k, v) => k -> sorted(v)))
    case JArr(xs) => JArr(xs.map(sorted))
    case other => other

  def wallet(key: PrivateKey, method: String, host: String, path: String, body: Json, now: Long): String =
    jws(key, JObj(Vector("alg" -> JStr("ES256"), "typ" -> JStr("JWT"))),
      JObj(Vector("iat" -> JNum(now.toDouble), "nbf" -> JNum(now.toDouble),
        "jti" -> JStr(java.util.UUID.randomUUID().toString),
        "uris" -> JArr(Vector(JStr(uri(method, host, path)))),
        "reqHash" -> JStr(sha256Hex(Json.print(sorted(body)))))))

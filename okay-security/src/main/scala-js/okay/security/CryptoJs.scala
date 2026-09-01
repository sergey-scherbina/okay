package okay.security

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}

/**
 * The Crypto seam over node:crypto — the security-node stage. What
 * Node has natively is real here: HMAC-SHA256, SHA-256, PBKDF2,
 * secure random. RSA STAYS JVM, and the reason is the seam's own
 * honesty: the trait speaks `java.security` keys, which exist on JS
 * only as signatures — there is no key material to hand to Node. A
 * JWK-native key type is the follow-up IF JS ever needs RS256; the
 * service-to-service case (HS256, passwords, API keys, PKCE) is what
 * this stage serves, and serves fully.
 */
@js.native @JSImport("crypto", JSImport.Namespace)
private object NodeCrypto extends js.Any:
  def createHmac(alg: String, key: Uint8Array): js.Dynamic = js.native
  def createHash(alg: String): js.Dynamic = js.native
  def pbkdf2Sync(password: String, salt: Uint8Array, iterations: Int,
                 keylen: Int, digest: String): js.Dynamic = js.native
  def randomBytes(n: Int): js.Dynamic = js.native

given Crypto = new Crypto:

  private def toBytes(buf: js.Dynamic): Array[Byte] =
    val u = buf.asInstanceOf[Uint8Array]
    val out = new Array[Byte](u.length)
    var i = 0
    while i < u.length do { out(i) = u(i).toByte; i += 1 }
    out

  private def ofBytes(a: Array[Byte]): Uint8Array =
    val u = new Uint8Array(a.length)
    var i = 0
    while i < a.length do { u(i) = (a(i) & 0xff).toShort; i += 1 }
    u

  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    toBytes(NodeCrypto.createHmac("sha256", ofBytes(key))
      .update(ofBytes(data)).digest())

  def sha256(data: Array[Byte]): Array[Byte] =
    toBytes(NodeCrypto.createHash("sha256").update(ofBytes(data)).digest())

  def signRsaSha256(key: Crypto.Handle, data: Array[Byte]): Array[Byte] =
    // a broken invariant, not hostile input: no RSA key can even be
    // CONSTRUCTED on this platform (rsaPublicKey answers None), so a
    // handle reaching here was smuggled
    throw UnsupportedOperationException("RSA signing is a JVM ability (security-node)")

  def verifyRsaSha256(key: Crypto.Handle, data: Array[Byte],
                      sig: Array[Byte]): Boolean =
    false   // a refusal: no verifiable key exists on this platform

  def pbkdf2(password: Array[Char], salt: Array[Byte],
             iterations: Int, bits: Int): Array[Byte] =
    // node takes the password as a string; the JVM side's
    // clear-after-use has no equivalent here, which is the platform's
    // cost, not the model's
    toBytes(NodeCrypto.pbkdf2Sync(String(password), ofBytes(salt),
      iterations, bits / 8, "sha256"))

  def randomBytes(n: Int): Array[Byte] =
    toBytes(NodeCrypto.randomBytes(n))

  def rsaPublicKey(modulus: BigInt, exponent: BigInt): Option[Crypto.Handle] =
    None   // RS256 is JVM until a JWK-native verify arrives

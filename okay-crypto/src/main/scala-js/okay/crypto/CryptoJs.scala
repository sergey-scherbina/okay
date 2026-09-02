package okay.crypto

import scala.scalajs.js
import scala.scalajs.js.typedarray.*

/** the node:crypto leg — reached through the global require, so it
 * needs no module-kind ceremony (the okay-pg pattern, now shared) */
/** the shape of node:crypto this leg uses — stated once, at the
 * module boundary (the global require answers untyped; this project
 * has no module kind for a JSImport), instead of cast per result */
@js.native
private trait NodeHash extends js.Object:
  def update(data: Int8Array): NodeHash = js.native
  def digest(): Uint8Array = js.native

@js.native
private trait NodeCrypto extends js.Object:
  def createHmac(alg: String, key: Int8Array): NodeHash = js.native
  def createHash(alg: String): NodeHash = js.native
  def pbkdf2Sync(password: String, salt: Int8Array, iterations: Int, keylen: Int, digest: String): Uint8Array = js.native
  def randomBytes(n: Int): Uint8Array = js.native

given Crypto = new Crypto:
  private val crypto: NodeCrypto = js.Dynamic.global.require("crypto").asInstanceOf[NodeCrypto]

  private def bytesOf(u: Uint8Array): Array[Byte] =
    val out = new Array[Byte](u.length)
    var i = 0
    while i < u.length do { out(i) = (u(i).toInt & 0xff).toByte; i += 1 }
    out

  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val h = crypto.createHmac("sha256", byteArray2Int8Array(key))
    h.update(byteArray2Int8Array(data)): Unit
    bytesOf(h.digest())
  def sha256(data: Array[Byte]): Array[Byte] =
    val h = crypto.createHash("sha256")
    h.update(byteArray2Int8Array(data)): Unit
    bytesOf(h.digest())
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte] =
    bytesOf(crypto.pbkdf2Sync(new String(password), byteArray2Int8Array(salt),
      iterations, bits / 8, "sha256"))
  def randomBytes(n: Int): Array[Byte] =
    bytesOf(crypto.randomBytes(n))

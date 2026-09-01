package okay.crypto

import scala.scalajs.js
import scala.scalajs.js.typedarray.*

/** the node:crypto leg — reached through the global require, so it
 * needs no module-kind ceremony (the okay-pg pattern, now shared) */
given Crypto = new Crypto:
  private val crypto = js.Dynamic.global.require("crypto")

  private def bytesOf(d: js.Dynamic): Array[Byte] =
    val u = d.asInstanceOf[Uint8Array]
    val out = new Array[Byte](u.length)
    var i = 0
    while i < u.length do { out(i) = (u(i).toInt & 0xff).toByte; i += 1 }
    out

  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val h = crypto.createHmac("sha256", byteArray2Int8Array(key))
    h.update(byteArray2Int8Array(data))
    bytesOf(h.digest())
  def sha256(data: Array[Byte]): Array[Byte] =
    val h = crypto.createHash("sha256")
    h.update(byteArray2Int8Array(data))
    bytesOf(h.digest())
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte] =
    bytesOf(crypto.pbkdf2Sync(new String(password), byteArray2Int8Array(salt),
      iterations, bits / 8, "sha256"))
  def randomBytes(n: Int): Array[Byte] =
    bytesOf(crypto.randomBytes(n))

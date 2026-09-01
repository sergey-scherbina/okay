package okay.pg

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import javax.crypto.{Mac, SecretKeyFactory}

/**
 * SCRAM-SHA-256, the client side (RFC 5802 with the SHA-256
 * parameters of RFC 7677) — what a modern Postgres demands over
 * TCP. Small enough to own: three HMACs, one PBKDF2, one SHA-256,
 * all from the platform's crypto (the specs/tls.md rule: platform
 * primitives, never our own).
 *
 * The dance: client-first (bare, with our nonce) → server-first
 * (its nonce appended to ours, the salt, the iteration count) →
 * client-final (channel-binding stub + proof) → server-final (the
 * server SIGNATURE, which we VERIFY — mutual authentication is the
 * half of SCRAM most clients skip; refusing to skip it is why this
 * file exists rather than a md5 fallback).
 */
final class Scram(user: String, password: String, nonce: String):

  private val gs2Header = "n,,"
  private val clientFirstBare = s"n=$user,r=$nonce"

  /** the SASLInitialResponse payload */
  def clientFirst: Array[Byte] = (gs2Header + clientFirstBare).getBytes(UTF_8)

  private var serverSignature: Array[Byte] = null

  /** consumes server-first, answers client-final; throws loudly on
   * a malformed challenge or a server nonce that does not extend
   * ours (a challenge replayed from another session) */
  def clientFinal(serverFirst: Array[Byte]): Array[Byte] =
    val msg = new String(serverFirst, UTF_8)
    val combined = msg.split(",").find(_.startsWith("r=")).map(_.drop(2))
      .getOrElse(throw PgError("SCRAM server-first carries no nonce"))
    if !combined.startsWith(nonce) then
      throw PgError("SCRAM server nonce does not extend ours — a replayed challenge")
    val salt = Base64.getDecoder.decode(
      msg.split(",").find(_.startsWith("s=")).map(_.drop(2))
        .getOrElse(throw PgError("SCRAM server-first carries no salt")))
    val iterations = msg.split(",").find(_.startsWith("i=")).map(_.drop(2).toInt)
      .getOrElse(throw PgError("SCRAM server-first carries no iteration count"))

    val salted = hi(password, salt, iterations)
    val clientKey = hmac(salted, "Client Key")
    val storedKey = sha256(clientKey)
    val withoutProof = s"c=${Base64.getEncoder.encodeToString(gs2Header.getBytes(UTF_8))},r=$combined"
    val authMessage = s"$clientFirstBare,$msg,$withoutProof"
    val clientSignature = hmac(storedKey, authMessage)
    val proof = clientKey.zip(clientSignature).map(_ ^ _).map(_.toByte)
    serverSignature = hmac(hmac(salted, "Server Key"), authMessage)
    s"$withoutProof,p=${Base64.getEncoder.encodeToString(proof)}".getBytes(UTF_8)

  /** the mutual half: the server proves it holds the key too */
  def verifyServerFinal(serverFinal: Array[Byte]): Unit =
    val msg = new String(serverFinal, UTF_8)
    val v = msg.split(",").find(_.startsWith("v=")).map(_.drop(2))
      .getOrElse(throw PgError("SCRAM server-final carries no signature"))
    if !java.security.MessageDigest.isEqual(
      Base64.getDecoder.decode(v), serverSignature)
    then throw PgError("SCRAM server signature does not verify — not the server the password knows")

  private def hi(password: String, salt: Array[Byte], iterations: Int): Array[Byte] =
    val spec = PBEKeySpec(password.toCharArray, salt, iterations, 256)
    SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256").generateSecret(spec).getEncoded

  private def hmac(key: Array[Byte], msg: String): Array[Byte] =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(msg.getBytes(UTF_8))

  private def sha256(bs: Array[Byte]): Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-256").digest(bs)

object Scram:
  /** a printable nonce from the platform's secure randomness */
  def nonce(): String =
    val bs = new Array[Byte](18)
    java.security.SecureRandom().nextBytes(bs)
    Base64.getEncoder.encodeToString(bs)

/** a backend refusal or a protocol violation, named */
final case class PgError(message: String) extends RuntimeException(message)

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
 *
 * The dance is PHASE OBJECTS (pg-scram-typestate; the wire-typestate
 * family, phase objects where PState's Cont bridge buys nothing):
 * each step answers the only legal next phase, and a step out of
 * order does not EXIST as a method — `ClientFirst` has no
 * `serverFinal`, `ClientFinal` has no `serverFirst`. What used to be
 * ordering-by-convention over a private var is now the type's shape.
 * The one-object `Scram` class remains as the convenience adapter
 * (nothing broken; usable without the phases) — and even there the
 * old silent NPE on a misordered server is a NAMED PgError now.
 */
object Scram:

  /** phase 1: holds our nonce; the only step is receiving the
   * server's challenge */
  final class ClientFirst private[Scram] (user: String, password: String,
                                          nonce: String):
    private[Scram] val bare = s"n=$user,r=$nonce"

    /** the SASLInitialResponse payload */
    def message: Array[Byte] = (gs2Header + bare).getBytes(UTF_8)

    /** consume server-first, move to the proof phase; throws loudly
     * on a malformed challenge or a server nonce that does not
     * extend ours (a challenge replayed from another session) */
    def serverFirst(challenge: Array[Byte]): ClientFinal =
      val msg = new String(challenge, UTF_8)
      val combined = field(msg, "r=")
        .getOrElse(throw PgError("SCRAM server-first carries no nonce"))
      if !combined.startsWith(nonce) then
        throw PgError("SCRAM server nonce does not extend ours — a replayed challenge")
      val salt = Base64.getDecoder.decode(field(msg, "s=")
        .getOrElse(throw PgError("SCRAM server-first carries no salt")))
      val iterations = field(msg, "i=").map(_.toInt)
        .getOrElse(throw PgError("SCRAM server-first carries no iteration count"))

      val salted = hi(password, salt, iterations)
      val clientKey = hmac(salted, "Client Key")
      val storedKey = sha256(clientKey)
      val withoutProof =
        s"c=${Base64.getEncoder.encodeToString(gs2Header.getBytes(UTF_8))},r=$combined"
      val authMessage = s"$bare,$msg,$withoutProof"
      val clientSignature = hmac(storedKey, authMessage)
      val proof = clientKey.zip(clientSignature).map(_ ^ _).map(_.toByte)
      ClientFinal(
        s"$withoutProof,p=${Base64.getEncoder.encodeToString(proof)}".getBytes(UTF_8),
        hmac(hmac(salted, "Server Key"), authMessage))

  /** phase 2: holds the proof and the expected server signature; the
   * only step is verifying server-final */
  final class ClientFinal private[Scram] (val message: Array[Byte],
                                          serverSignature: Array[Byte]):
    /** the mutual half: the server proves it holds the key too */
    def serverFinal(fin: Array[Byte]): Unit =
      val msg = new String(fin, UTF_8)
      val v = field(msg, "v=")
        .getOrElse(throw PgError("SCRAM server-final carries no signature"))
      if !java.security.MessageDigest.isEqual(
        Base64.getDecoder.decode(v), serverSignature)
      then throw PgError(
        "SCRAM server signature does not verify — not the server the password knows")

  /** the entry: phase 1, with our nonce */
  def start(user: String, password: String, nonce: String = Scram.nonce()): ClientFirst =
    ClientFirst(user, password, nonce)

  /** a printable nonce from the platform's secure randomness */
  def nonce(): String =
    val bs = new Array[Byte](18)
    java.security.SecureRandom().nextBytes(bs)
    Base64.getEncoder.encodeToString(bs)

  private val gs2Header = "n,,"

  private def field(msg: String, prefix: String): Option[String] =
    msg.split(",").find(_.startsWith(prefix)).map(_.drop(prefix.length))

  private def hi(password: String, salt: Array[Byte], iterations: Int): Array[Byte] =
    val spec = PBEKeySpec(password.toCharArray, salt, iterations, 256)
    SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256").generateSecret(spec).getEncoded

  private def hmac(key: Array[Byte], msg: String): Array[Byte] =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(msg.getBytes(UTF_8))

  private def sha256(bs: Array[Byte]): Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-256").digest(bs)

/**
 * The one-object convenience over the phases — same API as before
 * the phases existed, same bytes; a misordered call is a NAMED
 * refusal where it used to be an accidental NPE.
 */
final class Scram(user: String, password: String, nonce: String):
  private var phase: AnyRef = Scram.start(user, password, nonce)

  /** the SASLInitialResponse payload */
  def clientFirst: Array[Byte] = phase match
    case p: Scram.ClientFirst => p.message
    case _ => throw PgError("SCRAM client-first requested after the handshake advanced")

  /** consumes server-first, answers client-final */
  def clientFinal(serverFirst: Array[Byte]): Array[Byte] = phase match
    case p: Scram.ClientFirst =>
      val next = p.serverFirst(serverFirst)
      phase = next
      next.message
    case _ => throw PgError("SCRAM server-first out of order")

  /** the mutual half: the server proves it holds the key too */
  def verifyServerFinal(serverFinal: Array[Byte]): Unit = phase match
    case p: Scram.ClientFinal => p.serverFinal(serverFinal)
    case _ => throw PgError("SCRAM server-final before server-first")

/** a backend refusal or a protocol violation, named */
final case class PgError(message: String) extends RuntimeException(message)

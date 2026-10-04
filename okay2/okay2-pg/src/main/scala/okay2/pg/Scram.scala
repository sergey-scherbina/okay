package okay2.pg

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import okay2.crypto.Crypto

/**
 * SCRAM-SHA-256, the client side (okay-pg's Scram.scala; RFC 5802 with
 * the SHA-256 parameters of RFC 7677) — what a modern Postgres demands
 * over TCP. The primitives come from the per-platform `okay2.crypto.Crypto`.
 *
 * The dance: client-first (bare, with our nonce) -> server-first (its
 * nonce appended to ours, the salt, the iteration count) -> client-final
 * (channel-binding stub + proof) -> server-final (the server SIGNATURE,
 * which we VERIFY — mutual authentication).
 *
 * The dance is PHASE OBJECTS: each step answers the only legal next
 * phase, and a step out of order does not EXIST as a method —
 * `ClientFirst` has no `serverFinal`, `ClientFinal` has no `serverFirst`.
 */
object Scram {

  /** phase 1: holds our nonce; the only step is receiving the server's
   * challenge */
  final class ClientFirst private[Scram] (user: String, password: String, nonce: String)(implicit c: Crypto) {
    private[Scram] val bare = s"n=$user,r=$nonce"

    /** the SASLInitialResponse payload */
    def message: Array[Byte] = (gs2Header + bare).getBytes(UTF_8)

    /** consume server-first, move to the proof phase; throws loudly on a
     * malformed challenge or a server nonce that does not extend ours */
    def serverFirst(challenge: Array[Byte]): ClientFinal = {
      val msg = new String(challenge, UTF_8)
      val combined = field(msg, "r=").getOrElse(throw PgError("SCRAM server-first carries no nonce"))
      if (!combined.startsWith(nonce))
        throw PgError("SCRAM server nonce does not extend ours — a replayed challenge")
      val salt = Base64.getDecoder.decode(field(msg, "s=")
        .getOrElse(throw PgError("SCRAM server-first carries no salt")))
      val iterations = field(msg, "i=").map(_.toInt)
        .getOrElse(throw PgError("SCRAM server-first carries no iteration count"))

      val salted = c.pbkdf2(password.toCharArray, salt, iterations, 256)
      val clientKey = hmac(salted, "Client Key")
      val storedKey = c.sha256(clientKey)
      val withoutProof = s"c=${Base64.getEncoder.encodeToString(gs2Header.getBytes(UTF_8))},r=$combined"
      val authMessage = s"$bare,$msg,$withoutProof"
      val clientSignature = hmac(storedKey, authMessage)
      val proof = clientKey.zip(clientSignature).map { case (a, b) => (a ^ b).toByte }
      new ClientFinal(
        s"$withoutProof,p=${Base64.getEncoder.encodeToString(proof)}".getBytes(UTF_8),
        hmac(hmac(salted, "Server Key"), authMessage))
    }

    private def hmac(key: Array[Byte], msg: String): Array[Byte] = c.hmacSha256(key, msg.getBytes(UTF_8))
  }

  /** phase 2: holds the proof and the expected server signature; the only
   * step is verifying server-final */
  final class ClientFinal private[Scram] (val message: Array[Byte], serverSignature: Array[Byte]) {
    /** the mutual half: the server proves it holds the key too */
    def serverFinal(fin: Array[Byte]): Unit = {
      val msg = new String(fin, UTF_8)
      val v = field(msg, "v=").getOrElse(throw PgError("SCRAM server-final carries no signature"))
      if (!constantTimeEquals(Base64.getDecoder.decode(v), serverSignature))
        throw PgError("SCRAM server signature does not verify — not the server the password knows")
    }
  }

  /** the entry: phase 1, with our nonce */
  def start(user: String, password: String, nonce: String)(implicit c: Crypto): ClientFirst =
    new ClientFirst(user, password, nonce)

  def start(user: String, password: String)(implicit c: Crypto): ClientFirst =
    start(user, password, nonce())

  /** a printable nonce from the platform's randomness */
  def nonce()(implicit c: Crypto): String = Base64.getEncoder.encodeToString(c.randomBytes(18))

  private val gs2Header = "n,,"

  private def field(msg: String, prefix: String): Option[String] =
    msg.split(",").find(_.startsWith(prefix)).map(_.drop(prefix.length))

  /** hand-rolled because MessageDigest.isEqual does not exist on JS; the
   * shape is the standard xor-fold */
  private[pg] def constantTimeEquals(a: Array[Byte], b: Array[Byte]): Boolean =
    if (a.length != b.length) false
    else {
      var acc = 0
      var i = 0
      while (i < a.length) { acc |= a(i) ^ b(i); i += 1 }
      acc == 0
    }
}

/**
 * The one-object convenience over the phases — a misordered call is a
 * NAMED refusal.
 */
final class Scram(user: String, password: String, nonce: String)(implicit c: Crypto) {
  private var first: Option[Scram.ClientFirst] = Some(Scram.start(user, password, nonce))
  private var fin: Option[Scram.ClientFinal] = None

  /** the SASLInitialResponse payload */
  def clientFirst: Array[Byte] = first match {
    case Some(p) => p.message
    case None => throw PgError("SCRAM client-first requested after the handshake advanced")
  }

  /** consumes server-first, answers client-final */
  def clientFinal(serverFirst: Array[Byte]): Array[Byte] = first match {
    case Some(p) =>
      val next = p.serverFirst(serverFirst)
      first = None
      fin = Some(next)
      next.message
    case None => throw PgError("SCRAM server-first out of order")
  }

  /** the mutual half: the server proves it holds the key too */
  def verifyServerFinal(serverFinal: Array[Byte]): Unit = fin match {
    case Some(p) => p.serverFinal(serverFinal)
    case None => throw PgError("SCRAM server-final before server-first")
  }
}

/** a backend refusal or a protocol violation, named; `code` is the
 * server's SQLSTATE for a backend error, empty for a protocol one */
final case class PgError(message: String, code: String = "") extends RuntimeException(message)

package okay.demo

import okay.security.{Claims, Crypto, Jwt, Keys, Verified}
import okay.security.given
import java.util.concurrent.atomic.AtomicReference

/**
 * Real login for the marketplace identity (demo-sessions,
 * specs/demo-chat.md): confirm-and-sign, not trust-the-field. A
 * `/match` message asserting "email x@y" used to be believed
 * outright; now a SESSION — an ES256 JWT bound to a confirmed
 * email — is what a turn's identity rides, and the same session
 * presented from any channel resolves to the same identity (the
 * cross-channel promise, kept by the token rather than by a
 * chat-typed claim).
 *
 * `start` mints a one-time code; this stack has no email transport
 * yet (specs/security.md), so the code rides the response the demo
 * hands back rather than an inbox — a deployment with real mail
 * delivery replaces that one return value with silence, nothing
 * else changes. `confirm` spends the code once and signs the token.
 */
object Login:

  // an ES256 key pair, one per process: the demo is a single
  // instance, so a restart simply signs out every session — stated,
  // not hidden. JVM only, as ES256 already is (specs/security.md).
  private val pair =
    val g = java.security.KeyPairGenerator.getInstance("EC")
    g.initialize(java.security.spec.ECGenParameterSpec("secp256r1"))
    g.generateKeyPair()
  private val key = Keys.ecPair(pair.getPublic, pair.getPrivate)

  private val ttlSec = 24L * 3600
  private val codeTtlMs = 10L * 60 * 1000

  /** sign a session token for a CONFIRMED email */
  def issue(email: String, now: Long = System.currentTimeMillis()): String =
    val sec = now / 1000
    Jwt.sign(Claims(subject = Some(email), expires = Some(sec + ttlSec), issuedAt = Some(sec)), key)

  /** the token's email, if the signature and the clock both hold */
  def verify(token: String, now: Long = System.currentTimeMillis()): Option[String] =
    Jwt.verify(token, _ => Some(key), audience = None, now = now / 1000) match
      case Verified.Ok(p) => Some(p.id)
      case Verified.No(_) => None

  // ---- confirm-and-sign: a one-time code per email ----------------

  private val pending = AtomicReference(Map.empty[String, (String, Long)])

  /** mint a fresh 6-digit code for `email`, replacing any earlier one */
  def start(email: String, now: Long = System.currentTimeMillis())(using c: okay.crypto.Crypto): String =
    val n = java.math.BigInteger(1, c.randomBytes(4)).intValue
    val code = f"${Math.floorMod(n, 1000000)}%06d"
    pending.updateAndGet(_ + (email -> (code, now + codeTtlMs)))
    code

  /** spend the code: right email, right code, not expired, ONCE */
  def confirm(email: String, code: String, now: Long = System.currentTimeMillis()): Boolean =
    val cur = pending.get()
    cur.get(email) match
      case Some((c, exp)) if c == code && now <= exp =>
        pending.compareAndSet(cur, cur - email) || confirm(email, code, now)
      case _ => false

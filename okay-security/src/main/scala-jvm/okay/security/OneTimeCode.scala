package okay.security

import java.util.concurrent.atomic.AtomicReference

/**
 * Confirm-and-sign (specs/security.md, security-sessions): mint a
 * fresh 6-digit code for a key (an email, typically), spend it once
 * before the deadline. Extracted 2026-09-02 from okay-demo's Login
 * — this stack has no email transport of its own, so a caller that
 * still lacks one hands the code back through whatever channel it
 * has (a response field, a console line); a deployment with real
 * mail delivery replaces that one return value with silence, nothing
 * else here changes.
 */
final class OneTimeCode(ttlMs: Long = 10L * 60 * 1000):
  private val pending = AtomicReference(Map.empty[String, (String, Long)])

  /** mint a fresh code for `key`, replacing any earlier one */
  def start(key: String, now: Long = System.currentTimeMillis())(using c: Crypto): String =
    val n = java.math.BigInteger(1, c.randomBytes(4)).intValue
    val code = f"${Math.floorMod(n, 1000000)}%06d"
    pending.updateAndGet(_ + (key -> (code, now + ttlMs)))
    code

  /** spend the code: right key, right code, not expired, ONCE */
  def confirm(key: String, code: String, now: Long = System.currentTimeMillis()): Boolean =
    val cur = pending.get()
    cur.get(key) match
      case Some((c, exp)) if c == code && now <= exp =>
        pending.compareAndSet(cur, cur - key) || confirm(key, code, now)
      case _ => false

package okay.demo

import okay.security.{OneTimeCode, SessionIssuer, Verified}

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
 * A thin wrapper over okay-security's `SessionIssuer`/`OneTimeCode`
 * (specs/security.md, security-sessions — extracted 2026-09-02): the
 * demo owns none of the ES256 or code-confirm machinery itself
 * anymore, only the choice to compose them this way.
 *
 * `start` mints a one-time code; this stack has no email transport
 * yet (specs/security.md), so the code rides the response the demo
 * hands back rather than an inbox — a deployment with real mail
 * delivery replaces that one return value with silence, nothing
 * else changes. `confirm` spends the code once and signs the token.
 */
object Login:

  private val sessions = SessionIssuer()
  private val codes = OneTimeCode()

  /** sign a session token for a CONFIRMED email */
  def issue(email: String, now: Long = System.currentTimeMillis()): String =
    sessions.issue(email, now = now)

  /** the token's email, if the signature and the clock both hold */
  def verify(token: String, now: Long = System.currentTimeMillis()): Option[String] =
    sessions.verify(token, now = now) match
      case Verified.Ok(p) => Some(p.id)
      case Verified.No(_) => None

  /** mint a fresh 6-digit code for `email`, replacing any earlier one */
  def start(email: String, now: Long = System.currentTimeMillis())(using c: okay.security.Crypto): String =
    codes.start(email, now = now)

  /** spend the code: right email, right code, not expired, ONCE */
  def confirm(email: String, code: String, now: Long = System.currentTimeMillis()): Boolean =
    codes.confirm(email, code, now = now)

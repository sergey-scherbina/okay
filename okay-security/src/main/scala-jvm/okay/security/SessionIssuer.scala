package okay.security

/**
 * An in-process ES256 session credential (specs/security.md,
 * security-sessions): one keypair per instance, one instance per
 * process being the common case — a restart signs out every session
 * it issued, stated not hidden. `issue`/`verify` a bearer token
 * carrying an arbitrary subject and optional scopes. Extracted
 * 2026-09-02 from okay-demo's Login and okay-admin's Admin.Issuer —
 * the identical four-line keypair generator and `Jwt.sign`/`verify`
 * wrapping, independently duplicated once already.
 *
 * A deployment wanting persistent or shared credentials builds its
 * OWN `Jwt.Key` from a real store instead and calls `Jwt.sign`/
 * `Jwt.verify` directly — this class is the zero-config default a
 * small service reaches for first, not the only door.
 */
final class SessionIssuer(ttlSec: Long = 24L * 3600):
  private val pair =
    val g = java.security.KeyPairGenerator.getInstance("EC")
    g.initialize(java.security.spec.ECGenParameterSpec("secp256r1"))
    g.generateKeyPair()
  private val key = Keys.ecPair(pair.getPublic, pair.getPrivate)

  /** a bearer token for `subject`, carrying `scopes` (empty for a
   * plain login session; non-empty for e.g. an admin-scoped token) */
  def issue(subject: String, scopes: Set[String] = Set.empty,
           now: Long = System.currentTimeMillis()): String =
    val sec = now / 1000
    Jwt.sign(Claims(subject = Some(subject), scopes = scopes,
      issuedAt = Some(sec), expires = Some(sec + ttlSec)), key)

  def verify(token: String, now: Long = System.currentTimeMillis()): Verified =
    Jwt.verify(token, _ => Some(key), audience = None, now = now / 1000)

package okay.security

/**
 * Passwords and API keys, the zero-dependency way: PBKDF2 (the JDK's
 * own KDF) with the STORED FORM CARRYING ITS PARAMETERS —
 * `pbkdf2$<iterations>$<salt>$<hash>` — so iterations can rise, or a
 * better KDF arrive (the argon2 satellite), and old entries verify
 * while new ones upgrade: migration hash by hash, never a flag day.
 * Every comparison is constant-time.
 */
object Password {

  private val Iterations = 210_000       // OWASP's floor for PBKDF2-SHA256
  private val SaltBytes = 16
  private val HashBits = 256

  private val enc = java.util.Base64.getEncoder
  private val dec = java.util.Base64.getDecoder

  def hash(password: Array[Char])(using c: Crypto): String =
    val salt = c.randomBytes(SaltBytes)
    val h = c.pbkdf2(password, salt, Iterations, HashBits)
    s"pbkdf2$$$Iterations$$${enc.encodeToString(salt)}$$${enc.encodeToString(h)}"

  /** verify against a stored form; garbage stored forms refuse */
  def verify(password: Array[Char], stored: String)(using c: Crypto): Boolean =
    stored.split('$') match
      case Array("pbkdf2", iters, salt64, hash64) =>
        (iters.toIntOption,
          decode(salt64), decode(hash64)) match
          case (Some(n), Some(salt), Some(expected)) if n > 0 && n <= 10_000_000 =>
            Crypto.constantTimeEquals(
              c.pbkdf2(password, salt, n, expected.length * 8), expected)
          case _ => false
      case _ => false

  private def decode(s: String): Option[Array[Byte]] =
    try Some(dec.decode(s)) catch case _: IllegalArgumentException => None
}

/**
 * API keys: the caller gets the KEY, the database keeps only its
 * SHA-256 digest — so what a leaked table holds cannot be presented.
 */
object ApiKey {

  private val enc = java.util.Base64.getUrlEncoder.withoutPadding

  /** (the key to hand out once, the digest to store) */
  def issue()(using c: Crypto): (String, String) =
    val key = "ok_" + enc.encodeToString(c.randomBytes(24))
    (key, digest(key))

  def digest(key: String)(using c: Crypto): String =
    enc.encodeToString(c.sha256(key.getBytes("UTF-8")))

  def verify(presented: String, stored: String)(using c: Crypto): Boolean =
    Crypto.constantTimeEquals(
      c.sha256(presented.getBytes("UTF-8")),
      try java.util.Base64.getUrlDecoder.decode(stored)
      catch case _: IllegalArgumentException => Array.emptyByteArray)
}

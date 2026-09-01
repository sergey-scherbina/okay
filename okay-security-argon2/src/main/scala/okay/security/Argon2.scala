package okay.security

import org.bouncycastle.crypto.generators.Argon2BytesGenerator
import org.bouncycastle.crypto.params.Argon2Parameters

/**
 * Argon2id — the one satellite that buys a dependency, because a
 * memory-hard KDF cannot be had from the JDK. The stored form is the
 * PHC string every other implementation reads —
 * `$argon2id$v=19$m=65536,t=3,p=1$<salt>$<hash>` (base64, no pad) —
 * which keeps the house rule (parameters ride the stored form, so
 * raising them is a hash-by-hash migration) AND makes the store
 * portable beyond this library.
 *
 * `verify` is total against a HOSTILE stored form: garbage refuses,
 * and absurd parameters refuse BEFORE allocating — a row claiming
 * gigabytes of memory is an attack on the verifier, not a password.
 */
object Argon2 {

  private val HashBytes = 32
  private val SaltBytes = 16

  // the ceilings a stored form may ask of the verifier
  private val MaxMemoryKb = 1 << 20      // 1 GiB
  private val MaxIterations = 64
  private val MaxParallelism = 16

  private val enc = java.util.Base64.getEncoder.withoutPadding
  private val dec = java.util.Base64.getDecoder

  /** Argon2id with OWASP-shaped defaults (64 MiB, t=3, p=1) */
  def hash(password: Array[Char], memoryKb: Int = 65536,
           iterations: Int = 3, parallelism: Int = 1)(using c: Crypto): String =
    require(memoryKb >= 8 * parallelism && memoryKb <= MaxMemoryKb
      && iterations >= 1 && iterations <= MaxIterations
      && parallelism >= 1 && parallelism <= MaxParallelism, "argon2 parameters out of range")
    val salt = c.randomBytes(SaltBytes)
    val h = derive(password, salt, memoryKb, iterations, parallelism)
    s"$$argon2id$$v=19$$m=$memoryKb,t=$iterations,p=$parallelism" +
      s"$$${enc.encodeToString(salt)}$$${enc.encodeToString(h)}"

  /** verify against a PHC stored form; anything else refuses */
  def verify(password: Array[Char], stored: String): Boolean =
    stored.split('$') match
      case Array("", "argon2id", "v=19", params, salt64, hash64) =>
        (numbers(params), decode(salt64), decode(hash64)) match
          case (Some((m, t, p)), Some(salt), Some(expect))
            if m >= 8 * p && m <= MaxMemoryKb
              && t >= 1 && t <= MaxIterations
              && p >= 1 && p <= MaxParallelism
              && expect.nonEmpty && salt.nonEmpty =>
            Crypto.constantTimeEquals(
              deriveN(password, salt, m, t, p, expect.length), expect)
          case _ => false
      case _ => false

  /** the migration door: one call reads a MIXED store — `$argon2id$`
   * rows verify here, `pbkdf2$` rows go to Password */
  def verifyAny(password: Array[Char], stored: String)(using Crypto): Boolean =
    if stored.startsWith("$argon2id$") then verify(password, stored)
    else Password.verify(password, stored)

  private def derive(password: Array[Char], salt: Array[Byte],
                     m: Int, t: Int, p: Int): Array[Byte] =
    deriveN(password, salt, m, t, p, HashBytes)

  private def deriveN(password: Array[Char], salt: Array[Byte],
                      m: Int, t: Int, p: Int, n: Int): Array[Byte] =
    val params = Argon2Parameters.Builder(Argon2Parameters.ARGON2_id)
      .withVersion(Argon2Parameters.ARGON2_VERSION_13)
      .withMemoryAsKB(m).withIterations(t).withParallelism(p)
      .withSalt(salt).build()
    val gen = Argon2BytesGenerator()
    gen.init(params)
    val out = new Array[Byte](n)
    gen.generateBytes(password, out)
    out

  private def numbers(s: String): Option[(Int, Int, Int)] = s.split(',') match
    case Array(m, t, p) =>
      for
        mv <- prefixed(m, "m="); tv <- prefixed(t, "t="); pv <- prefixed(p, "p=")
      yield (mv, tv, pv)
    case _ => None

  private def prefixed(s: String, pre: String): Option[Int] =
    if s.startsWith(pre) then s.drop(pre.length).toIntOption else None

  private def decode(s: String): Option[Array[Byte]] =
    try Some(dec.decode(s)) catch case _: IllegalArgumentException => None
}

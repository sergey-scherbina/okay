package okay.crypto

/**
 * `Keccak` over BouncyCastle's `KeccakDigest(256)` (JVM only), behind an
 * OPTIONAL dependency: `import okay.crypto.BouncyCastleKeccak.given`.
 * Without the jar the first use is refused by name ([[missing]]).
 */
object BouncyCastleKeccak extends Keccak:
  given bouncyCastle: Keccak = this

  def name = "bouncycastle"

  def missing(className: String = "org.bouncycastle.crypto.digests.KeccakDigest"): Option[String] =
    try { Class.forName(className, false, getClass.getClassLoader); None }
    catch case _: ClassNotFoundException => Some(
      s"okay.crypto.BouncyCastleKeccak needs BouncyCastle, an optional dependency of okay-crypto ($className is not on the classpath): " +
        "add org.bouncycastle:bcprov-jdk18on:1.78.1 — or use okay.crypto.Keccak.Okay, the default, which needs nothing")

  private lazy val ready: Unit = missing().foreach(why => throw IllegalStateException(why))

  def hash256(data: Array[Byte]): Array[Byte] =
    ready
    val d = org.bouncycastle.crypto.digests.KeccakDigest(256)
    d.update(data, 0, data.length)
    val out = new Array[Byte](32)
    d.doFinal(out, 0): Unit
    out

/** the implementations by NAME, for a config value or a flag */
object Keccaks:
  def byName(name: String): Either[String, Keccak] = name match
    case "okay" => Right(Keccak.Okay)
    case "bouncycastle" => Right(BouncyCastleKeccak)
    case other => Left(s"unknown Keccak implementation '$other' (okay, bouncycastle)")

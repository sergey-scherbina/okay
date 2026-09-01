package okay.pg

/**
 * The three primitives SCRAM needs and nothing more, as a
 * per-platform given — LOCAL to okay-pg only because the full
 * okay-security seam's module drags okayHttp (its JWKS road) and
 * that path cycles back here through the build graph; the moment a
 * security-crypto-split module exists (filed), this trait retires
 * into it. Platform primitives, never our own (the tls.md rule).
 */
trait PgCrypto:
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte]
  def sha256(data: Array[Byte]): Array[Byte]
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte]
  def randomBytes(n: Int): Array[Byte]

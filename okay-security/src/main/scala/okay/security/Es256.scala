package okay.security

/**
 * The ES256 signature dance — pure bytes, no crypto. JOSE carries an
 * ECDSA signature as R||S: two 32-byte big-endian integers, 64 bytes,
 * no framing. JCA and node speak DER: SEQUENCE(INTEGER r, INTEGER s),
 * where an INTEGER sheds leading zero bytes and grows a 0x00 pad when
 * its high bit is set. The two shapes disagree on almost every real
 * signature (half of all r values have the high bit set), which is
 * why this conversion is a task and not a footnote.
 *
 * Both directions are TOTAL: the signature segment of a JWT is
 * attacker-supplied, so anything not exactly the expected shape is a
 * None, never a throw.
 */
object Es256 {

  private val N = 32   // P-256 coordinate width in bytes

  /** raw R||S (exactly 64 bytes) -> DER SEQUENCE(INTEGER, INTEGER) */
  def joseToDer(raw: Array[Byte]): Option[Array[Byte]] =
    if raw.length != 2 * N then None
    else
      val r = integer(raw, 0)
      val s = integer(raw, N)
      val body = r ++ s
      // total length stays well under 128, so short-form lengths only
      Some(Array[Byte](0x30, body.length.toByte) ++ body)

  /** one coordinate as a DER INTEGER: strip leading zeros (keeping
   * one for the value 0), pad 0x00 back when the high bit is set */
  private def integer(raw: Array[Byte], from: Int): Array[Byte] =
    var i = from
    while i < from + N - 1 && raw(i) == 0 do i += 1
    val mag = raw.slice(i, from + N)
    val body = if (mag(0) & 0x80) != 0 then Array[Byte](0) ++ mag else mag
    Array[Byte](0x02, body.length.toByte) ++ body

  /** DER SEQUENCE(INTEGER r, INTEGER s) -> raw R||S (64 bytes) */
  def derToJose(der: Array[Byte]): Option[Array[Byte]] =
    for
      _ <- Option.when(der.length >= 2 && der(0) == 0x30
             && (der(1) & 0xff) == der.length - 2
             && (der(1) & 0xff) < 128)(())     // short form only — DER for P-256 never needs more
      (r, afterR) <- intAt(der, 2)
      (s, afterS) <- intAt(der, afterR)
      _ <- Option.when(afterS == der.length)(())   // no trailing garbage
    yield r ++ s

  /** parse one INTEGER at `at`, left-padded to 32 bytes; None when it
   * is not an integer, runs past the end, or cannot fit 32 bytes */
  private def intAt(der: Array[Byte], at: Int): Option[(Array[Byte], Int)] =
    if at + 2 > der.length || der(at) != 0x02 then None
    else
      val len = der(at + 1) & 0xff
      if len == 0 || len >= 128 || at + 2 + len > der.length then None
      else
        val body = der.slice(at + 2, at + 2 + len)
        // a 33-byte body must be the 0x00 pad over a high-bit value;
        // anything longer cannot be a P-256 coordinate
        val mag =
          if body.length == N + 1 && body(0) == 0 && (body(1) & 0x80) != 0 then Some(body.tail)
          else if body.length <= N then Some(body)
          else None
        mag.map(m => (Array.fill[Byte](N - m.length)(0) ++ m, at + 2 + len))
}

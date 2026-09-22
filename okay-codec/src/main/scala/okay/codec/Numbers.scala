package okay.codec

/**
 * A wire number into a field that must HOLD it, or a refusal —
 * sint-decode-truncates (specs/codecs.md "Integers that do not fit").
 * Every door (the fold decoders, Validate, and the staged codecs, whose
 * generated code runs in the CALLER's package — hence public) used to
 * `.toInt`/`.toLong` here, so 3000000000 read as 2147483647, 2^32 as 0
 * and 1.5 as 1, each a `Right` nobody could tell from a real value.
 */
object Numbers:
  /** a JSON number into an Int: integral and within Int */
  def int(d: Double): Either[String, Int] =
    if !d.isWhole then Left(s"expected an integer, got $d")
    else if d < Int.MinValue || d > Int.MaxValue then Left(s"integer $d out of range for an Int")
    else Right(d.toInt)

  /** a CBOR integer (already a Long) into an Int */
  def int(n: Long): Either[String, Int] =
    if n < Int.MinValue || n > Int.MaxValue then Left(s"integer $n out of range for an Int")
    else Right(n.toInt)

  /**
   * A JSON number into a Long: INTEGRAL is required; the range is not
   * checked, deliberately. Past 2^53 a JSON number has already been
   * rounded by the parser (`JNum` is a Double), and `Long.MaxValue`
   * itself round-trips today only because the Double saturates back to
   * it — refusing |d| >= 2^63 would break that, and refusing past 2^53
   * would break every large Long already written as a number. A value
   * that needs all 64 bits exactly wants `BigInt` (a digit string).
   */
  def long(d: Double): Either[String, Long] =
    if d.isWhole then Right(d.toLong) else Left(s"expected an integer, got $d")

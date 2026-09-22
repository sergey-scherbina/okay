package okay.codec

/**
 * What a JSON door accepts for `Schema.SBigInt` (schema-bigint,
 * specs/codecs.md "Big integers") — written once, because `Json.decode`,
 * `JsonStrict` and `Validate` must accept the SAME set or the lossless
 * and the strict door disagree on one document:
 *
 *  - a string of `-?[0-9]+` — what `Json.encode` writes;
 *  - a JSON number that is integral and within ±2^53, the range where a
 *    Double is exact, so nothing was lost before we saw it; a larger
 *    one has ALREADY been rounded by the time it is a `JNum`, and is
 *    refused with the reason rather than returned wrong.
 */
private[codec] object BigInts:
  /** 2^53: every integer up to here is exactly a Double */
  private val exact = 9007199254740992.0

  def fromDigits(s: String): Either[String, BigInt] =
    val digits = if s.startsWith("-") then s.drop(1) else s
    if digits.nonEmpty && digits.forall(c => c >= '0' && c <= '9') then Right(BigInt(s))
    else Left(s"expected an integer as a string of digits, got \"$s\"")

  def fromNumber(d: Double): Either[String, BigInt] =
    if d.isWhole && math.abs(d) <= exact then Right(BigInt(d.toLong))
    else if d.isWhole then Left(s"integer $d is past ±2^53, where a JSON number is already rounded; send it as a string of digits")
    else Left(s"expected an integer, got $d")

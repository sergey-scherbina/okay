package okay2.codec

/** a BigInt travels as a string of digits; a JSON number is accepted
 * only while a double still holds it exactly (|n| <= 2^53) */
private[codec] object BigInts {
  private val exact = 9007199254740992.0

  def fromDigits(s: String): Either[String, BigInt] = {
    val digits = if (s.startsWith("-")) s.drop(1) else s
    if (digits.nonEmpty && digits.forall(c => c >= '0' && c <= '9')) Right(BigInt(s))
    else Left(s"""expected an integer as a string of digits, got "$s"""")
  }

  def fromNumber(d: Double): Either[String, BigInt] =
    if (d.isWhole && math.abs(d) <= exact) Right(BigInt(d.toLong))
    else if (d.isWhole) Left(s"integer $d is past ±2^53, where a JSON number is already rounded; send it as a string of digits")
    else Left(s"expected an integer, got $d")
}

package okay.semantic

/** One division policy for all semantic execution paths. */
object DecimalMath:
  def divide(numerator: BigDecimal, denominator: BigDecimal): Option[BigDecimal] =
    if denominator == 0 then None
    else
      val exact = scala.util.Try(numerator.bigDecimal.divide(denominator.bigDecimal)).toOption
      Some(BigDecimal(exact.getOrElse(numerator.bigDecimal.divide(denominator.bigDecimal,java.math.MathContext.DECIMAL128))))

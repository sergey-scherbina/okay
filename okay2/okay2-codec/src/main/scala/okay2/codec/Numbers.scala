package okay2.codec

/** a JSON number's reading as an integral type: whole, and in range */
object Numbers {
  def int(d: Double): Either[String, Int] =
    if (!d.isWhole) Left(s"expected an integer, got $d")
    else if (d < Int.MinValue || d > Int.MaxValue) Left(s"integer $d out of range for an Int")
    else Right(d.toInt)

  def int(n: Long): Either[String, Int] =
    if (n < Int.MinValue || n > Int.MaxValue) Left(s"integer $n out of range for an Int")
    else Right(n.toInt)

  def long(d: Double): Either[String, Long] =
    if (d.isWhole) Right(d.toLong) else Left(s"expected an integer, got $d")
}

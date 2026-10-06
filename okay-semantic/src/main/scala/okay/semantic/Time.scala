package okay.semantic

/** An exact fixed-duration bucket, expressed in epoch microseconds. */
final class FixedBucket private (val widthMicros: Long, val anchorMicros: Long) extends Serializable:
  def start(micros: Long): BigInt =
    val shifted = BigInt(micros) - anchorMicros
    val width = BigInt(widthMicros)
    val remainder = ((shifted % width) + width) % width
    shifted - remainder + anchorMicros
object FixedBucket:
  def build(widthMicros: Long, anchorMicros: Long = 0L): Either[String, FixedBucket] =
    if widthMicros <= 0 then Left("time bucket: width must be positive")
    else Right(new FixedBucket(widthMicros, anchorMicros))

enum Grain:
  case Hour, Day, Week, Month, Quarter, Year
enum TimeTransform:
  case Fixed(widthMicros: Long, anchorMicros: Long)
  case Civil(grain: Grain, zone: String)
/** Civil-calendar semantics are supplied by the platform, not guessed from a timezone offset. */
trait Calendar:
  def start(micros: Long, grain: Grain, zone: String): Either[String, Long]
object Time:
  def dimension[A](id: String, description: String, read: A => Option[Long], bucket: FixedBucket): Dimension[A] =
    Dimension(id, description, Kind.Number, a => read(a).fold[Value](Value.Null)(t => Value.Number(BigDecimal(bucket.start(t)))), Some(TimeTransform.Fixed(bucket.widthMicros, bucket.anchorMicros)))
  def window(dimension: String, fromMicros: Long, untilMicros: Long): Either[String, Vector[Filter]] =
    if fromMicros >= untilMicros then Left("time window: start must precede exclusive end")
    else Right(Vector(Filter(dimension, Value.Number(BigDecimal(fromMicros)), Comparison.Ge),
      Filter(dimension, Value.Number(BigDecimal(untilMicros)), Comparison.Lt)))

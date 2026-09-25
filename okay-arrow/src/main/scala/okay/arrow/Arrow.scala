package okay.arrow

/**
 * okay-arrow (specs/okay-arrow.md): Arrow IPC streams behind ONE facade,
 * with two implementations chosen by a given:
 *
 * {{{
 * summon[ArrowCodec]                     // OkayArrow: ours, every platform, no dependency
 * import okay.arrow.ApacheArrow.given    // Arrow Java (JVM), an OPTIONAL dependency you add
 * }}}
 *
 * The model is the five columns okay's wire carries (int64, float64,
 * utf8, bool, null), every one nullable, and the schema's metadata.
 */
trait ArrowCodec:
  def name: String
  /** a table as one IPC stream: schema, one record batch, end of stream */
  def write(t: Table): Array[Byte]
  /** an IPC stream as a table (several batches concatenated); anything
   * outside the model, or a stream cut short, is refused by name */
  def read(bytes: Array[Byte]): Table

object ArrowCodec:
  /** THE DEFAULT: ours — the wire's five columns on every platform */
  given own: ArrowCodec = OkayArrow

  /** whether these bytes are an Arrow stream: its first message's
   * continuation marker, which begins no JSON text and no CBOR item of
   * the okay wire */
  def isStream(bytes: Array[Byte]): Boolean =
    bytes.length >= 4 && bytes(0) == -1 && bytes(1) == -1 && bytes(2) == -1 && bytes(3) == -1

/** one column; `valid(i)` false is a null at row i (the value there is
 * ignored). Arrays, not boxed cells: a frame is often large. */
enum Column:
  case Int64(values: Array[Long], valid: Array[Boolean])
  case Float64(values: Array[Double], valid: Array[Boolean])
  case Utf8(values: Array[String], valid: Array[Boolean])
  case Bool(values: Array[Boolean], valid: Array[Boolean])
  /** Arrow's null type: every row null, no buffers */
  case Nulls(rows: Int)

  def length: Int = this match
    case Int64(v, _) => v.length
    case Float64(v, _) => v.length
    case Utf8(v, _) => v.length
    case Bool(v, _) => v.length
    case Nulls(n) => n

object Column:
  /** one column out of batches' parts of ONE kind, in order (at least one
   * part); parts of different kinds are refused by name */
  def concat(parts: Vector[Column]): Column =
    require(parts.nonEmpty, "concat needs at least one part")
    def valid = parts.iterator.map {
      case Int64(_, ok) => ok
      case Float64(_, ok) => ok
      case Utf8(_, ok) => ok
      case Bool(_, ok) => ok
      case Nulls(n) => Array.fill(n)(false)
    }.foldLeft(Array.emptyBooleanArray)(_ ++ _)
    def mixed = throw IllegalArgumentException(s"a column changed kind between batches: ${parts.map(_.getClass.getSimpleName).distinct.mkString(", ")}")
    if parts.length == 1 then parts.head
    else parts.head match
      case Int64(_, _) => Int64(parts.map { case Int64(v, _) => v; case _ => mixed }.foldLeft(Array.emptyLongArray)(_ ++ _), valid)
      case Float64(_, _) => Float64(parts.map { case Float64(v, _) => v; case _ => mixed }.foldLeft(Array.emptyDoubleArray)(_ ++ _), valid)
      case Utf8(_, _) => Utf8(parts.map { case Utf8(v, _) => v; case _ => mixed }.foldLeft(Array.empty[String])(_ ++ _), valid)
      case Bool(_, _) => Bool(parts.map { case Bool(v, _) => v; case _ => mixed }.foldLeft(Array.emptyBooleanArray)(_ ++ _), valid)
      case Nulls(_) => Nulls(parts.map { case Nulls(n) => n; case _ => mixed }.sum)

/** a table: named columns of one length, and the schema's metadata */
final case class Table(cols: Vector[(String, Column)], metadata: Vector[(String, String)]):
  def rows: Int = cols.headOption.fold(0)(_._2.length)

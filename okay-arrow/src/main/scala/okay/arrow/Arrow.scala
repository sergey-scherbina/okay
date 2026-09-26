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
 * The model (`Table`, `Column`) is okay's columnar format: the Arrow types
 * okay's data takes — ints of every width, floats, bool, text, bytes,
 * decimals, dates, timestamps, durations, lists and structs, every column
 * nullable — and the schema's metadata.
 */
trait ArrowCodec:
  def name: String
  /** a table as one IPC stream: schema, one record batch, end of stream */
  def write(t: Table): Array[Byte]
  /** an IPC stream as a table (several batches concatenated, dictionaries
   * decoded); anything outside the model, or a stream cut short, is
   * refused by name. A compressed body is read by whichever
   * `okay.compress.Compression` is in scope — ours by default,
   * `Aircompressor.given` for the library's (compress-crypto-facades) */
  def read(bytes: Array[Byte])(using okay.compress.Compression): Table
  /** a table as an IPC FILE: the stream, a footer, random access by batch */
  def writeFile(t: Table, compression: Option[okay.compress.Codec] = None): Array[Byte]
  /** every record batch of an IPC file, as one table */
  def readFile(bytes: Array[Byte])(using okay.compress.Compression): Table
  /** how many record batches an IPC file holds */
  def fileBatches(bytes: Array[Byte]): Int
  /** the i-th record batch of an IPC file, found from its footer */
  def readFileBatch(bytes: Array[Byte], i: Int)(using okay.compress.Compression): Table

object ArrowCodec:
  /** THE DEFAULT: ours, on every platform */
  given own: ArrowCodec = OkayArrow

  /** whether these bytes are an Arrow stream: its first message's
   * continuation marker, which begins no JSON text and no CBOR item of
   * the okay wire */
  def isStream(bytes: Array[Byte]): Boolean =
    bytes.length >= 4 && bytes(0) == -1 && bytes(1) == -1 && bytes(2) == -1 && bytes(3) == -1

  /** typed rows through either implementation (`Rows`, stage 5) */
  extension (codec: ArrowCodec)
    def encode[A](rows: Seq[A])(using okay.codec.Schema[A]): Array[Byte] = codec.write(Rows.table(rows))
    def decode[A](bytes: Array[Byte])(using okay.codec.Schema[A], okay.compress.Compression): Either[String, Vector[A]] = Rows.rows[A](codec.read(bytes))

/** the unit of a timestamp or a duration */
enum TimeUnit:
  case Second, Milli, Micro, Nano

/**
 * One column. Values sit in primitive arrays, one slot per row; `valid(i)`
 * false is a null at row i, and the value in that slot is ignored. A
 * nested column (a list, a struct) holds its child columns.
 */
enum Column:
  case Int64(values: Array[Long], valid: Array[Boolean])
  case Float64(values: Array[Double], valid: Array[Boolean])
  case Utf8(values: Array[String], valid: Array[Boolean])
  case Bool(values: Array[Boolean], valid: Array[Boolean])
  /** Arrow's null type: every row null, no buffers */
  case Nulls(rows: Int)
  /** an int of 8, 16 or 32 bits, signed or not, or an UNSIGNED 64-bit one
   * (whose values are the Long bit patterns) */
  case Ints(bits: Int, signed: Boolean, values: Array[Long], valid: Array[Boolean])
  case Float32(values: Array[Float], valid: Array[Boolean])
  case Binary(values: Array[Array[Byte]], valid: Array[Boolean])
  case FixedBinary(width: Int, values: Array[Array[Byte]], valid: Array[Boolean])
  /** decimal128: the UNSCALED value, and the precision and scale that read it */
  case Decimal(precision: Int, scale: Int, unscaled: Array[BigInt], valid: Array[Boolean])
  /** days since the epoch (date32) */
  case Date32(days: Array[Int], valid: Array[Boolean])
  /** milliseconds since the epoch (date64) */
  case Date64(millis: Array[Long], valid: Array[Boolean])
  /** since the epoch, in `unit`; `zone` None is a wall-clock (naive) time */
  case Timestamp(unit: TimeUnit, zone: Option[String], values: Array[Long], valid: Array[Boolean])
  case Duration(unit: TimeUnit, values: Array[Long], valid: Array[Boolean])
  /** row i is `child`'s rows [offsets(i), offsets(i + 1)) */
  case ListOf(offsets: Array[Int], child: Column, valid: Array[Boolean])
  /** row i is the i-th row of every field */
  case Struct(fields: Vector[(String, Column)], valid: Array[Boolean])
  /** DICTIONARY-ENCODED (stage 8): row i is `dictionary`'s row
   * `indices(i)` — int32 indices into a dictionary column of any kind,
   * kept as they came so its values, their order and the unused ones
   * survive (an R factor's levels, a pandas category's categories) */
  case Dictionary(indices: Array[Int], dictionary: Column, ordered: Boolean, valid: Array[Boolean])

  /** the column this one reads as: a `Dictionary`'s values gathered by its
   * indices, anything else itself — what a consumer that has no use for
   * the encoding reads (stage 8) */
  def decoded: Column = this match
    case Dictionary(idx, dict, _, ok) => dict.take(Array.tabulate(idx.length)(i => if ok(i) then idx(i) else 0), ok)
    case other => other

  def length: Int = this match
    case Nulls(n) => n
    case ListOf(offsets, _, _) => offsets.length - 1
    case other => other.validity.length

  /** the validity of every row (all false for the null type) */
  def validity: Array[Boolean] = this match
    case Int64(_, ok) => ok
    case Float64(_, ok) => ok
    case Utf8(_, ok) => ok
    case Bool(_, ok) => ok
    case Nulls(n) => Array.fill(n)(false)
    case Ints(_, _, _, ok) => ok
    case Float32(_, ok) => ok
    case Binary(_, ok) => ok
    case FixedBinary(_, _, ok) => ok
    case Decimal(_, _, _, ok) => ok
    case Date32(_, ok) => ok
    case Date64(_, ok) => ok
    case Timestamp(_, _, _, ok) => ok
    case Duration(_, _, ok) => ok
    case ListOf(_, _, ok) => ok
    case Struct(_, ok) => ok
    case Dictionary(_, _, _, ok) => ok

  /** the rows at `at`, in that order (how a dictionary is decoded); a row
   * whose `keep` is false is null */
  def take(at: Array[Int], keep: Array[Boolean]): Column =
    def ok(src: Array[Boolean]) = Array.tabulate(at.length)(i => keep(i) && src(at(i)))
    def pick[A: scala.reflect.ClassTag](v: Array[A]) = Array.tabulate(at.length)(i => v(at(i)))
    this match
      case Int64(v, o) => Int64(pick(v), ok(o))
      case Float64(v, o) => Float64(pick(v), ok(o))
      case Utf8(v, o) => Utf8(pick(v), ok(o))
      case Bool(v, o) => Bool(pick(v), ok(o))
      case Nulls(_) => Nulls(at.length)
      case Ints(b, s, v, o) => Ints(b, s, pick(v), ok(o))
      case Float32(v, o) => Float32(pick(v), ok(o))
      case Binary(v, o) => Binary(pick(v), ok(o))
      case FixedBinary(w, v, o) => FixedBinary(w, pick(v), ok(o))
      case Decimal(p, s, v, o) => Decimal(p, s, pick(v), ok(o))
      case Date32(v, o) => Date32(pick(v), ok(o))
      case Date64(v, o) => Date64(pick(v), ok(o))
      case Timestamp(u, z, v, o) => Timestamp(u, z, pick(v), ok(o))
      case Duration(u, v, o) => Duration(u, pick(v), ok(o))
      case ListOf(offs, child, o) =>
        val rows = at.map(i => offs(i) until offs(i + 1))
        val out = new Array[Int](at.length + 1)
        var i = 0
        while i < at.length do { out(i + 1) = out(i) + rows(i).length; i += 1 }
        ListOf(out, child.take(rows.flatMap(r => r).toArray, Array.fill(out(at.length))(true)), ok(o))
      case Struct(fs, o) => Struct(fs.map((n, c) => n -> c.take(at, keep)), ok(o))
      case Dictionary(idx, dict, ord, o) => Dictionary(pick(idx), dict, ord, ok(o))

object Column:
  /** how deep a column's TYPE may nest (lists and structs, a leaf is 0) —
   * Arrow's own reference limit: C++ `IpcReadOptions::max_recursion_depth`,
   * `kMaxNestingDepth` = 64. Every walk over a type here recurses once
   * per level, so this is the bound that makes them safe
   * (stack-safety-arrow) */
  val MaxNesting: Int = 64

  /** how many list/struct levels the column's type has (a leaf is 0),
   * counted on an explicit stack: this is the check, so it cannot be one
   * of the walks it guards */
  def nesting(c: Column): Int =
    val todo = scala.collection.mutable.Stack[(Column, Int)]((c, 0))
    var deepest = 0
    while todo.nonEmpty do
      val (x, d) = todo.pop()
      if d > deepest then deepest = d
      x match
        case ListOf(_, child, _) => todo.push((child, d + 1))
        case Struct(fs, _) => fs.foreach((_, f) => todo.push((f, d + 1)))
        case _ => ()
    deepest

  /** one column out of batches' parts of ONE kind, in order (at least one
   * part); parts of different kinds are refused by name */
  def concat(parts: Vector[Column]): Column =
    require(parts.nonEmpty, "concat needs at least one part")
    if parts.length == 1 then parts.head
    else
      val kinds = parts.map(kind).distinct
      if kinds.length > 1 then
        throw IllegalArgumentException(s"a column changed kind between batches: ${kinds.mkString(", ")}")
      parts.reduceLeft(append)

  /** the column's type, as a person reads it: `int32`, `list<utf8>`, `struct<a: int64>` */
  def describe(c: Column): String = kind(c)

  private def kind(c: Column): String = c match
    case Column.Ints(b, s, _, _) => s"${if s then "int" else "uint"}$b"
    case Column.Decimal(p, s, _, _) => s"decimal($p, $s)"
    case Column.Timestamp(u, z, _, _) => s"timestamp($u, ${z.getOrElse("naive")})"
    case Column.Duration(u, _, _) => s"duration($u)"
    case Column.FixedBinary(w, _, _) => s"fixed_size_binary($w)"
    case Column.ListOf(_, child, _) => s"list<${kind(child)}>"
    case Column.Struct(fs, _) => fs.map((n, c) => s"$n: ${kind(c)}").mkString("struct<", ", ", ">")
    case Column.Dictionary(_, dict, _, _) => s"dictionary<${kind(dict)}>"
    case other => other.getClass.getSimpleName

  /** two dictionaries with the same values in the same order */
  private def same(a: Column, b: Column): Boolean =
    a.length == b.length && ((a, b) match
      case (Column.Utf8(x, xo), Column.Utf8(y, yo)) => x.sameElements(y) && xo.sameElements(yo)
      case (Column.Int64(x, xo), Column.Int64(y, yo)) => x.sameElements(y) && xo.sameElements(yo)
      case (Column.Ints(xb, xs, x, xo), Column.Ints(yb, ys, y, yo)) => xb == yb && xs == ys && x.sameElements(y) && xo.sameElements(yo)
      case _ => false)

  private def append(a: Column, b: Column): Column =
    val ok = a.validity ++ b.validity
    (a, b) match
      case (Column.Int64(x, _), Column.Int64(y, _)) => Column.Int64(x ++ y, ok)
      case (Column.Float64(x, _), Column.Float64(y, _)) => Column.Float64(x ++ y, ok)
      case (Column.Utf8(x, _), Column.Utf8(y, _)) => Column.Utf8(x ++ y, ok)
      case (Column.Bool(x, _), Column.Bool(y, _)) => Column.Bool(x ++ y, ok)
      case (Column.Nulls(x), Column.Nulls(y)) => Column.Nulls(x + y)
      case (Column.Ints(bits, s, x, _), Column.Ints(_, _, y, _)) => Column.Ints(bits, s, x ++ y, ok)
      case (Column.Float32(x, _), Column.Float32(y, _)) => Column.Float32(x ++ y, ok)
      case (Column.Binary(x, _), Column.Binary(y, _)) => Column.Binary(x ++ y, ok)
      case (Column.FixedBinary(w, x, _), Column.FixedBinary(_, y, _)) => Column.FixedBinary(w, x ++ y, ok)
      case (Column.Decimal(p, s, x, _), Column.Decimal(_, _, y, _)) => Column.Decimal(p, s, x ++ y, ok)
      case (Column.Date32(x, _), Column.Date32(y, _)) => Column.Date32(x ++ y, ok)
      case (Column.Date64(x, _), Column.Date64(y, _)) => Column.Date64(x ++ y, ok)
      case (Column.Timestamp(u, z, x, _), Column.Timestamp(_, _, y, _)) => Column.Timestamp(u, z, x ++ y, ok)
      case (Column.Duration(u, x, _), Column.Duration(_, y, _)) => Column.Duration(u, x ++ y, ok)
      case (Column.ListOf(xo, xc, _), Column.ListOf(yo, yc, _)) =>
        // offsets that start at 0 and end at the child's length (what a
        // read produces): the second part's shift by the first's child
        Column.ListOf(xo ++ yo.tail.map(_ - yo.head + xo.last), append(xc, yc), ok)
      case (Column.Struct(xf, _), Column.Struct(yf, _)) =>
        Column.Struct(xf.zip(yf).map { case ((n, x), (_, y)) => n -> append(x, y) }, ok)
      // one dictionary: the indices concatenate; a replacement one is
      // appended and the later indices shifted past it (stage 8)
      case (Column.Dictionary(xi, xd, ord, _), Column.Dictionary(yi, yd, _, _)) =>
        if same(xd, yd) then Column.Dictionary(xi ++ yi, xd, ord, ok)
        else Column.Dictionary(xi ++ yi.map(_ + xd.length), append(xd, yd), ord, ok)
      case _ => throw IllegalArgumentException(s"a column changed kind between batches: ${kind(a)}, ${kind(b)}")

/** a table: named columns of one length, and the schema's metadata */
final case class Table(cols: Vector[(String, Column)], metadata: Vector[(String, String)]):
  // every column that is written, or handed out by a reader, passes here
  cols.foreach { (name, c) =>
    val n = Column.nesting(c)
    if n > Column.MaxNesting then
      throw IllegalArgumentException(s"column '$name' nests $n levels; Arrow's limit is ${Column.MaxNesting}")
  }
  def rows: Int = cols.headOption.fold(0)(_._2.length)

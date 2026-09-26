package okay.arrow

/** test tables and a cell-by-cell comparison, shared by every platform's
 * suites and both implementations' */
object Tables:

  /** a column as comparable cells: None for a null, a kind tag first so a
   * column that changed kind cannot compare equal */
  def cells(c: Column): Vector[Option[Any]] =
    def each[A](v: Array[A], ok: Array[Boolean])(f: A => Any): Vector[Option[Any]] =
      v.indices.map(i => Option.when(ok(i))(f(v(i)))).toVector
    c match
      case d: Column.Dictionary => ("dictionary" +: d.dictionary.length +: cells(d.decoded)).map(Some(_))
      case Column.Int64(v, ok) => each(v, ok)(x => x)
      case Column.Float64(v, ok) => each(v, ok)(java.lang.Double.doubleToRawLongBits(_))
      case Column.Utf8(v, ok) => each(v, ok)(x => x)
      case Column.Bool(v, ok) => each(v, ok)(x => x)
      case Column.Nulls(n) => Vector.fill(n)(None)
      case Column.Ints(_, _, v, ok) => each(v, ok)(x => x)
      case Column.Float32(v, ok) => each(v, ok)(java.lang.Float.floatToRawIntBits(_))
      case Column.Binary(v, ok) => each(v, ok)(_.toVector)
      case Column.FixedBinary(_, v, ok) => each(v, ok)(_.toVector)
      case Column.Decimal(_, _, v, ok) => each(v, ok)(x => x)
      case Column.Date32(v, ok) => each(v, ok)(x => x)
      case Column.Date64(v, ok) => each(v, ok)(x => x)
      case Column.Timestamp(_, _, v, ok) => each(v, ok)(x => x)
      case Column.Duration(_, v, ok) => each(v, ok)(x => x)
      case Column.ListOf(offs, child, ok) =>
        val inner = cells(child)
        (0 until offs.length - 1).map(i => Option.when(ok(i))(inner.slice(offs(i), offs(i + 1)))).toVector
      case Column.Struct(fs, ok) =>
        val inner = fs.map((n, f) => n -> cells(f))
        ok.indices.map(i => Option.when(ok(i))(inner.map((n, v) => n -> v(i)))).toVector

  /** the column's declared type, beside its cells */
  def kind(c: Column): String = c match
    case Column.Ints(b, s, _, _) => s"Ints($b,$s)"
    case Column.Decimal(p, s, _, _) => s"Decimal($p,$s)"
    case Column.Timestamp(u, z, _, _) => s"Timestamp($u,$z)"
    case Column.Duration(u, _, _) => s"Duration($u)"
    case Column.FixedBinary(w, _, _) => s"FixedBinary($w)"
    case Column.ListOf(_, ch, _) => s"ListOf(${kind(ch)})"
    case Column.Struct(fs, _) => fs.map((n, f) => s"$n:${kind(f)}").mkString("Struct(", ",", ")")
    case other => other.getClass.getSimpleName

  def same(a: Table, b: Table): Option[String] =
    if b.metadata != a.metadata then Some(s"metadata ${b.metadata} != ${a.metadata}")
    else if b.cols.map(_._1) != a.cols.map(_._1) then Some(s"names ${b.cols.map(_._1)} != ${a.cols.map(_._1)}")
    else a.cols.zip(b.cols).collectFirst {
      case ((n, x), (_, y)) if kind(x) != kind(y) => s"column $n: ${kind(y)} != ${kind(x)}"
      case ((n, x), (_, y)) if cells(x) != cells(y) => s"column $n: ${cells(y)} != ${cells(x)}"
    }

  private val ok4 = Array(true, true, true, false)

  /** every kind of column, nulls in each, four rows */
  def everything: Table = Table(Vector(
    "i64" -> Column.Int64(Array(1L, -2L, Long.MaxValue, 0L), ok4),
    "f64" -> Column.Float64(Array(0.5, Double.NaN, -0.0, 1e300), Array(true, true, false, true)),
    "text" -> Column.Utf8(Array("kyiv", "", "чай ☕ 𝄞", "x"), ok4),
    "flag" -> Column.Bool(Array(true, false, true, true), Array(true, true, false, true)),
    "none" -> Column.Nulls(4),
    "i8" -> Column.Ints(8, true, Array(-128L, 127L, 0L, 5L), ok4),
    "u8" -> Column.Ints(8, false, Array(255L, 0L, 1L, 2L), ok4),
    "i16" -> Column.Ints(16, true, Array(-32768L, 32767L, 1L, 0L), ok4),
    "u16" -> Column.Ints(16, false, Array(65535L, 0L, 1L, 0L), ok4),
    "i32" -> Column.Ints(32, true, Array(Int.MinValue.toLong, Int.MaxValue.toLong, 0L, 1L), ok4),
    "u32" -> Column.Ints(32, false, Array(4294967295L, 0L, 1L, 0L), ok4),
    "u64" -> Column.Ints(64, false, Array(-1L, 0L, Long.MaxValue, 0L), ok4),
    "f32" -> Column.Float32(Array(1.5f, Float.NaN, -0.0f, 3e38f), ok4),
    "bytes" -> Column.Binary(Array(Array[Byte](1, 2, 3), Array.emptyByteArray, Array[Byte](-1), Array.emptyByteArray), ok4),
    "fixed" -> Column.FixedBinary(2, Array(Array[Byte](1, 2), Array[Byte](3, 4), Array[Byte](5, 6), Array[Byte](0, 0)), ok4),
    "money" -> Column.Decimal(10, 2, Array(BigInt(12345), BigInt(-1), BigInt("99999999"), BigInt(0)), ok4),
    "big" -> Column.Decimal(38, 0, Array(BigInt("99999999999999999999999999999999999999"),
      BigInt("-99999999999999999999999999999999999999"), BigInt(0), BigInt(0)), ok4),
    "day" -> Column.Date32(Array(0, 19999, -1, 0), ok4),
    "ms" -> Column.Date64(Array(0L, 1727222400000L, -86400000L, 0L), ok4),
    "at" -> Column.Timestamp(TimeUnit.Micro, Some("Europe/Kyiv"), Array(1L, 1727222400000000L, -5L, 0L), ok4),
    "naive" -> Column.Timestamp(TimeUnit.Nano, None, Array(1L, 2L, 3L, 0L), ok4),
    "took" -> Column.Duration(TimeUnit.Milli, Array(1500L, 0L, -3L, 0L), ok4),
    "tags" -> Column.ListOf(Array(0, 2, 2, 3, 3),
      Column.Utf8(Array("a", "b", "c"), Array(true, false, true)), ok4),
    "nested" -> Column.ListOf(Array(0, 1, 3, 3, 3),
      Column.ListOf(Array(0, 2, 2, 3), Column.Int64(Array(7L, 8L, 9L), Array(true, true, true)), Array(true, false, true)), ok4),
    "who" -> Column.Struct(Vector(
      "name" -> Column.Utf8(Array("ann", "bo", "", "z"), Array(true, true, false, true)),
      "age" -> Column.Ints(32, true, Array(30L, 40L, 0L, 1L), ok4),
      "tags" -> Column.ListOf(Array(0, 1, 1, 1, 1), Column.Int64(Array(5L), Array(true)), Array(true, true, true, true))),
      Array(true, false, true, true))),
    Vector("okay" -> """{"id":7}""", "source" -> "Tables.everything"))

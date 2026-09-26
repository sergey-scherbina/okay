package okay.parquet

import okay.arrow.{Column, Table, TimeUnit}
import okay.compress.Compression
import okay.parquet.Thrift.{Struct, Value}

/**
 * OUR PARQUET (specs/parquet.md): the footer and pages read and written
 * over okay-arrow's `Table`, flat schemas, no dependency beyond okay.
 * The field numbers below are parquet.thrift's; each struct is read and
 * written as the file format defines it, so the comments name the
 * thrift fields rather than restate the format.
 */
object OkayParquet extends ParquetCodec:
  def name = "okay"

  private val Magic = "PAR1".getBytes("US-ASCII")

  // physical types
  private val TBoolean = 0
  private val TInt32 = 1
  private val TInt64 = 2
  private val TInt96 = 3
  private val TFloat = 4
  private val TDouble = 5
  private val TByteArray = 6
  private val TFixed = 7

  // encodings
  private val Plain = 0
  private val PlainDictionary = 2
  private val RleEncoding = 3
  private val RleDictionary = 8

  // page types
  private val DataPage = 0
  private val DictionaryPage = 2
  private val DataPageV2 = 3

  /** what a leaf decodes to */
  private enum Kind:
    case KBool
    case KInt32(bits: Int, signed: Boolean)
    case KDate
    case KInt64(signed: Boolean)
    case KTimestamp(unit: TimeUnit, utc: Boolean)
    case KInt96
    case KFloat
    case KDouble
    case KString
    case KBinary
    case KFixed(width: Int)
    case KDecimal(precision: Int, scale: Int)
  import Kind.*

  private final case class Leaf(name: String, physical: Int, width: Int, optional: Boolean, kind: Kind)

  // ------------------------------------------------------------ the footer

  /** the footer as this codec reads it, kept in `Footer.parsed` */
  private final case class Parsed(f: Struct, leaves: Vector[Leaf])

  def footer(in: ReadAt): Footer =
    val (f, leaves) = meta(in)
    Footer(leaves.map(l => l.name -> describe(l.kind)), f.structs(4).map(_.int(3).getOrElse(0L)),
      f.structs(5).map(kv => kv.str(1).getOrElse("") -> kv.str(2).getOrElse("")), f.str(6))(Parsed(f, leaves))

  private def meta(in: ReadAt): (Struct, Vector[Leaf]) =
    val size = in.size
    if size < 12 then throw Refused(s"not a Parquet file: $size bytes")
    val tail = in.read(size - 8, 8)
    if !java.util.Arrays.equals(tail.drop(4), Magic) then throw Refused("not a Parquet file: no PAR1 at its end")
    val len = (tail(0) & 0xff) | (tail(1) & 0xff) << 8 | (tail(2) & 0xff) << 16 | (tail(3) & 0xff) << 24
    if len <= 0 || len > size - 12 then throw Refused(s"a Parquet footer of $len bytes in a file of $size")
    val (f, _) = Thrift.read(in.read(size - 8 - len, len))
    (f, leaves(f.structs(2)))

  private def leaves(schema: Vector[Struct]): Vector[Leaf] =
    if schema.isEmpty then throw Refused("a Parquet footer with no schema")
    val children = schema.head.int(5).getOrElse(0L).toInt
    schema.tail.take(children).map { e =>
      val name = e.str(4).getOrElse("")
      if e.int(5).exists(_ > 0) then
        throw Refused(s"column '$name' is a group: nested Parquet schemas are not read yet (specs/parquet.md)")
      if e.int(3).contains(2L) then
        throw Refused(s"column '$name' is REPEATED: nested Parquet schemas are not read yet (specs/parquet.md)")
      if schema.length - 1 != children then
        throw Refused("a Parquet schema with nested groups: not read yet (specs/parquet.md)")
      val physical = e.int(1).getOrElse(throw Refused(s"column '$name' has no type")).toInt
      Leaf(name, physical, e.int(2).getOrElse(0L).toInt, !e.int(3).contains(0L), kindOf(name, physical, e))
    }

  private def kindOf(name: String, physical: Int, e: Struct): Kind =
    val logical = e.struct(10)
    val converted = e.int(6).map(_.toInt)
    def decimal: Option[Kind] =
      logical.flatMap(_.struct(5)).map(d => KDecimal(d.int(2).getOrElse(0L).toInt, d.int(1).getOrElse(0L).toInt))
        .orElse(if converted.contains(5) then Some(KDecimal(e.int(8).getOrElse(0L).toInt, e.int(7).getOrElse(0L).toInt)) else None)
    def integer: Option[(Int, Boolean)] =
      logical.flatMap(_.struct(10)).map(i => (i.int(1).getOrElse(32L).toInt, i.bool(2).getOrElse(true)))
        .orElse(converted.collect {
          case 11 => (8, false); case 12 => (16, false); case 13 => (32, false); case 14 => (64, false)
          case 15 => (8, true); case 16 => (16, true); case 17 => (32, true); case 18 => (64, true) })
    physical match
      case TBoolean => KBool
      case TInt32 =>
        decimal.orElse(
          if logical.exists(_.struct(6).isDefined) || converted.contains(6) then Some(KDate) else None)
          .getOrElse { val (b, s) = integer.getOrElse((32, true)); KInt32(b, s) }
      case TInt64 =>
        val ts = logical.flatMap(_.struct(8)).map { t =>
          val u = t.struct(2)
          val unit =
            if u.exists(_.struct(1).isDefined) then TimeUnit.Milli
            else if u.exists(_.struct(2).isDefined) then TimeUnit.Micro
            else TimeUnit.Nano
          KTimestamp(unit, t.bool(1).getOrElse(false))
        }.orElse(converted.collect {
          case 9 => KTimestamp(TimeUnit.Milli, true)
          case 10 => KTimestamp(TimeUnit.Micro, true) })
        ts.orElse(decimal).getOrElse(KInt64(integer.forall(_._2)))
      case TInt96 => KInt96
      case TFloat => KFloat
      case TDouble => KDouble
      case TByteArray =>
        val text = logical.exists(l => l.struct(1).isDefined || l.struct(4).isDefined || l.struct(12).isDefined) ||
          converted.exists(c => c == 0 || c == 4 || c == 19)
        if text then KString else decimal.getOrElse(KBinary)
      case TFixed => decimal.getOrElse(KFixed(e.int(2).getOrElse(0L).toInt))
      case other => throw Refused(s"column '$name' has physical type $other")

  private def describe(k: Kind): String = Column.describe(empty(k, 0))

  /** a column of `n` nulls of this kind */
  private def empty(k: Kind, n: Int): Column =
    val no = new Array[Boolean](n)
    k match
      case KBool => Column.Bool(new Array[Boolean](n), no)
      case KInt32(b, s) => Column.Ints(b, s, new Array[Long](n), no)
      case KDate => Column.Date32(new Array[Int](n), no)
      case KInt64(true) => Column.Int64(new Array[Long](n), no)
      case KInt64(false) => Column.Ints(64, false, new Array[Long](n), no)
      case KTimestamp(u, utc) => Column.Timestamp(u, if utc then Some("UTC") else None, new Array[Long](n), no)
      case KInt96 => Column.Timestamp(TimeUnit.Nano, Some("UTC"), new Array[Long](n), no)
      case KFloat => Column.Float32(new Array[Float](n), no)
      case KDouble => Column.Float64(new Array[Double](n), no)
      case KString => Column.Utf8(Array.fill(n)(""), no)
      case KBinary => Column.Binary(Array.fill(n)(Array.emptyByteArray), no)
      case KFixed(w) => Column.FixedBinary(w, Array.fill(n)(new Array[Byte](w)), no)
      case KDecimal(p, s) => Column.Decimal(p, s, Array.fill(n)(BigInt(0)), no)

  // ------------------------------------------------------------- reading

  def group(in: ReadAt, footer: Footer, g: Int, columns: Option[Set[String]] = None)
           (using Compression): Table =
    val (f, leaves) = footer.parsed match
      case Parsed(f, leaves) => (f, leaves)
      case _ => meta(in)
    val groups = f.structs(4)
    if g < 0 || g >= groups.length then throw Refused(s"row group $g of ${groups.length}")
    val rg = groups(g)
    val rows = rg.int(3).getOrElse(0L)
    if rows > Int.MaxValue then throw Refused(s"a row group of $rows rows")
    val chunks = rg.structs(1)
    if chunks.length != leaves.length then throw Refused(s"row group $g has ${chunks.length} column chunks for ${leaves.length} columns")
    val cols = leaves.zip(chunks).filter((l, _) => columns.forall(_.contains(l.name))).map { (l, c) =>
      l.name -> chunk(in, l, c, rows.toInt)
    }
    Table(cols, f.structs(5).map(kv => kv.str(1).getOrElse("") -> kv.str(2).getOrElse("")))

  private def chunk(in: ReadAt, leaf: Leaf, c: Struct, rows: Int)(using z: Compression): Column =
    val m = c.struct(3).getOrElse(throw Refused(s"column '${leaf.name}': a chunk without metadata"))
    val codec = m.int(4).getOrElse(0L).toInt
    val total = m.int(5).getOrElse(0L)
    val data = m.int(9).getOrElse(throw Refused(s"column '${leaf.name}': no data page offset"))
    val start = m.int(11).filter(d => d > 0 && d < data).getOrElse(data)
    val size = m.int(7).getOrElse(0L)
    if size > Int.MaxValue then throw Refused(s"column '${leaf.name}': a chunk of $size bytes")
    val bytes = in.read(start, size.toInt)
    var at = 0
    var seen = 0L
    var dict: Option[Column] = None
    val parts = Vector.newBuilder[Column]
    while seen < total do
      if at >= bytes.length then throw Refused(s"column '${leaf.name}': its pages end before its $total values")
      val (h, body) = Thrift.read(bytes, at)
      val csize = h.int(3).getOrElse(0L).toInt
      val usize = h.int(2).getOrElse(0L).toInt
      if body + csize > bytes.length then throw Refused(s"column '${leaf.name}': a page past its chunk")
      h.int(1).getOrElse(-1L).toInt match
        case DictionaryPage =>
          val d = h.struct(7).getOrElse(throw Refused(s"column '${leaf.name}': a dictionary page without its header"))
          val page = inflate(leaf, codec, bytes, body, csize, usize)
          dict = Some(plain(leaf, page, 0, page.length, d.int(1).getOrElse(0L).toInt))
        case DataPage =>
          val d = h.struct(5).getOrElse(throw Refused(s"column '${leaf.name}': a data page without its header"))
          val n = d.int(1).getOrElse(0L).toInt
          val page = inflate(leaf, codec, bytes, body, csize, usize)
          var p = 0
          val valid =
            if !leaf.optional then Array.fill(n)(true)
            else
              val len = (page(0) & 0xff) | (page(1) & 0xff) << 8 | (page(2) & 0xff) << 16 | (page(3) & 0xff) << 24
              val levels = Rle.decode(page, 4, 4 + len, 1, n)
              p = 4 + len
              levels.map(_ == 1)
          parts += values(leaf, d.int(2).getOrElse(0L).toInt, page, p, page.length, valid, dict)
          seen += n
        case DataPageV2 =>
          val d = h.struct(8).getOrElse(throw Refused(s"column '${leaf.name}': a v2 data page without its header"))
          val n = d.int(1).getOrElse(0L).toInt
          val defLen = d.int(5).getOrElse(0L).toInt
          val repLen = d.int(6).getOrElse(0L).toInt
          if repLen != 0 then throw Refused(s"column '${leaf.name}': repetition levels in a flat column")
          val valid =
            if !leaf.optional || defLen == 0 then Array.fill(n)(true)
            else Rle.decode(bytes, body + repLen, body + repLen + defLen, 1, n).map(_ == 1)
          val from = body + repLen + defLen
          val compressed = d.bool(7).getOrElse(true)
          val page =
            if compressed then inflate(leaf, codec, bytes, from, csize - repLen - defLen, usize - repLen - defLen)
            else java.util.Arrays.copyOfRange(bytes, from, body + csize)
          parts += values(leaf, d.int(4).getOrElse(0L).toInt, page, 0, page.length, valid, dict)
          seen += n
        case _ => ()                      // an index page, or one this reader need not read
      at = body + csize
    val all = parts.result()
    if all.isEmpty then empty(leaf.kind, rows) else Column.concat(all)

  private def inflate(leaf: Leaf, codec: Int, bytes: Array[Byte], from: Int, len: Int, usize: Int)
                     (using z: Compression): Array[Byte] =
    val raw = java.util.Arrays.copyOfRange(bytes, from, from + len)
    val out = codec match
      case 0 => raw
      case 1 => z.snappy.decompress(raw)
      case 6 => z.zstd.decompress(raw)
      case other =>
        val named = Vector("UNCOMPRESSED", "SNAPPY", "GZIP", "LZO", "BROTLI", "LZ4", "ZSTD", "LZ4_RAW").lift(other).getOrElse(other.toString)
        throw Refused(s"column '${leaf.name}' is compressed with $named: read are UNCOMPRESSED, SNAPPY and ZSTD (specs/parquet.md)")
    if out.length != usize then throw Refused(s"column '${leaf.name}': a page of ${out.length} bytes declaring $usize")
    out

  /** a page's values placed at its valid rows */
  private def values(leaf: Leaf, encoding: Int, page: Array[Byte], from: Int, until: Int,
                     valid: Array[Boolean], dict: Option[Column]): Column =
    val n = valid.length
    val present = valid.count(identity)
    val dense: Column = encoding match
      case Plain => plain(leaf, page, from, until, present)
      case PlainDictionary | RleDictionary =>
        val d = dict.getOrElse(throw Refused(s"column '${leaf.name}': dictionary indices before its dictionary"))
        val width = page(from) & 0xff
        val idx = Rle.decode(page, from + 1, until, width, present)
        if idx.exists(i => i < 0 || i >= d.length) then throw Refused(s"column '${leaf.name}': an index outside its dictionary")
        d.take(idx, Array.fill(present)(true))
      // booleans RLE-encoded (pyarrow's data page v2): a 4-byte length,
      // then the hybrid at width one
      case RleEncoding if leaf.kind == KBool =>
        if until - from < 4 then throw Refused(s"column '${leaf.name}': RLE booleans cut short")
        val len = (page(from) & 0xff) | (page(from + 1) & 0xff) << 8 | (page(from + 2) & 0xff) << 16 | (page(from + 3) & 0xff) << 24
        val bits = Rle.decode(page, from + 4, math.min(until, from + 4 + len), 1, present)
        Column.Bool(bits.map(_ == 1), Array.fill(present)(true))
      case other =>
        val named = Map(5 -> "DELTA_BINARY_PACKED", 6 -> "DELTA_LENGTH_BYTE_ARRAY", 7 -> "DELTA_BYTE_ARRAY",
          9 -> "BYTE_STREAM_SPLIT", 3 -> "RLE (outside booleans)").getOrElse(other, other.toString)
        throw Refused(s"column '${leaf.name}' is encoded $named: read are PLAIN and the dictionary encodings (specs/parquet.md)")
    if present == n then dense
    else if present == 0 then empty(leaf.kind, n)
    else
      val at = new Array[Int](n)
      var k = 0
      var i = 0
      while i < n do { at(i) = if valid(i) then k else 0; if valid(i) then k += 1; i += 1 }
      dense.take(at, valid)

  /** `n` PLAIN values, all present */
  private def plain(leaf: Leaf, b: Array[Byte], from: Int, until: Int, n: Int): Column =
    val ok = Array.fill(n)(true)
    var at = from
    def need(k: Int): Unit =
      if at + k > until then throw Refused(s"column '${leaf.name}': its values end early")
    def i32(): Int =
      need(4)
      val v = (b(at) & 0xff) | (b(at + 1) & 0xff) << 8 | (b(at + 2) & 0xff) << 16 | (b(at + 3) & 0xff) << 24
      at += 4
      v
    def i64(): Long =
      need(8)
      var v = 0L
      var k = 0
      while k < 8 do { v |= (b(at + k) & 0xffL) << (8 * k); k += 1 }
      at += 8
      v
    def bytes(k: Int): Array[Byte] =
      need(k)
      val out = java.util.Arrays.copyOfRange(b, at, at + k)
      at += k
      out
    def unscaled(raw: Array[Byte]): BigInt = if raw.isEmpty then BigInt(0) else BigInt(raw)
    leaf.kind match
      case KBool =>
        need((n + 7) / 8)
        val v = Array.tabulate(n)(i => ((b(at + (i >>> 3)) >>> (i & 7)) & 1) == 1)
        Column.Bool(v, ok)
      case KInt32(bits, signed) =>
        Column.Ints(bits, signed, Array.fill(n) { val v = i32(); if signed then v.toLong else v & 0xffffffffL }, ok)
      case KDate => Column.Date32(Array.fill(n)(i32()), ok)
      case KInt64(true) => Column.Int64(Array.fill(n)(i64()), ok)
      case KInt64(false) => Column.Ints(64, false, Array.fill(n)(i64()), ok)
      case KTimestamp(u, utc) => Column.Timestamp(u, if utc then Some("UTC") else None, Array.fill(n)(i64()), ok)
      case KInt96 =>
        Column.Timestamp(TimeUnit.Nano, Some("UTC"), Array.fill(n) {
          val nanos = i64()
          val day = i32()
          (day - 2440588L) * 86400000000000L + nanos
        }, ok)
      case KFloat => Column.Float32(Array.fill(n)(java.lang.Float.intBitsToFloat(i32())), ok)
      case KDouble => Column.Float64(Array.fill(n)(java.lang.Double.longBitsToDouble(i64())), ok)
      case KString => Column.Utf8(Array.fill(n)(String(bytes(i32()), "UTF-8")), ok)
      case KBinary => Column.Binary(Array.fill(n)(bytes(i32())), ok)
      case KFixed(w) => Column.FixedBinary(w, Array.fill(n)(bytes(w)), ok)
      case KDecimal(p, s) =>
        val v = leaf.physical match
          case TInt32 => Array.fill(n)(BigInt(i32()))
          case TInt64 => Array.fill(n)(BigInt(i64()))
          case TFixed => Array.fill(n)(unscaled(bytes(leaf.width)))
          case _ => Array.fill(n)(unscaled(bytes(i32())))
        Column.Decimal(p, s, v, ok)

  // ------------------------------------------------------------- writing

  /** rows per data page */
  val PageRows: Int = 64 * 1024

  def writer(out: Array[Byte] => Unit, compress: Compress = Compress.Snappy,
             metadata: Vector[(String, String)] = Vector.empty)
            (using z: Compression): ParquetWriter = new ParquetWriter:
    private var at = 0L
    private var schema: Option[Vector[(String, Column)]] = None
    private val groups = Vector.newBuilder[Struct]
    private var rows = 0L
    private var closed = false

    private def emit(b: Array[Byte]): Unit = { out(b); at += b.length }
    emit(Magic)

    def append(t: Table): Unit =
      if closed then throw IllegalStateException("a Parquet writer appended to after close")
      schema match
        case None => schema = Some(t.cols)
        case Some(first) =>
          val a = first.map((n, c) => n -> Column.describe(c))
          val b = t.cols.map((n, c) => n -> Column.describe(c))
          if a != b then throw Refused(s"a row group of columns $b after $a: every group of a file has one schema")
      val chunks = t.cols.map((name, c) => column(name, c))
      groups += Struct.of(
        1 -> Value.L(12, chunks.map(Value.S(_))),
        2 -> Value.I(chunks.map(_.struct(3).flatMap(_.int(6)).getOrElse(0L)).sum),
        3 -> Value.I(t.rows.toLong))
      rows += t.rows

    private def column(name: String, c: Column): Struct =
      val (physical, _, _) = typeOf(name, c)
      val start = at
      var uncompressed = 0L
      var from = 0
      while from < c.length || (from == 0 && c.length == 0) do
        val n = math.min(PageRows, c.length - from)
        val page = dataPage(name, Slice(c, from, n))
        val body = compress match
          case Compress.None => page
          case Compress.Snappy => z.snappy.compress(page)
          case Compress.Zstd => z.zstd.compress(page)
        val header = Thrift.write(Struct.of(
          1 -> Value.I32(DataPage),
          2 -> Value.I32(page.length),
          3 -> Value.I32(body.length),
          5 -> Value.S(Struct.of(
            1 -> Value.I32(n),
            2 -> Value.I32(Plain),
            3 -> Value.I32(RleEncoding),
            4 -> Value.I32(RleEncoding)))))
        emit(header)
        emit(body)
        uncompressed += header.length + page.length
        from += math.max(n, 1)
      Struct.of(
        2 -> Value.I(start),
        3 -> Value.S(Struct.of(
          1 -> Value.I32(physical),
          2 -> Value.L(5, Vector(Value.I32(Plain), Value.I32(RleEncoding))),
          3 -> Value.L(8, Vector(Value.Bin(name.getBytes("UTF-8")))),
          4 -> Value.I32(compress match { case Compress.None => 0; case Compress.Snappy => 1; case Compress.Zstd => 6 }),
          5 -> Value.I(c.length.toLong),
          6 -> Value.I(uncompressed),
          7 -> Value.I(at - start),
          9 -> Value.I(start))))

    def close(): Unit =
      if !closed then
        closed = true
        val cols = schema.getOrElse(Vector.empty)
        val elements = Value.S(Struct.of(4 -> Value.Bin("schema".getBytes("UTF-8")), 5 -> Value.I32(cols.length))) +:
          cols.map((name, c) => Value.S(element(name, c)))
        val kv = metadata.map((k, v) => Value.S(Struct.of(1 -> Value.Bin(k.getBytes("UTF-8")), 2 -> Value.Bin(v.getBytes("UTF-8")))))
        val footer = Thrift.write(Struct(Map(
          1 -> Value.I32(1),
          2 -> Value.L(12, elements),
          3 -> Value.I(rows),
          4 -> Value.L(12, groups.result().map(Value.S(_))),
          6 -> Value.Bin("okay-parquet".getBytes("UTF-8"))) ++
          (if kv.isEmpty then Map.empty else Map(5 -> Value.L(12, kv)))))
        emit(footer)
        emit(Array((footer.length & 0xff).toByte, ((footer.length >>> 8) & 0xff).toByte,
          ((footer.length >>> 16) & 0xff).toByte, ((footer.length >>> 24) & 0xff).toByte))
        emit(Magic)

  /** (physical type, converted type, logical type) of a column we write */
  private def typeOf(name: String, c: Column): (Int, Option[Int], Option[Struct]) =
    def integer(bits: Int, signed: Boolean) =
      Some(Struct.of(10 -> Value.S(Struct.of(1 -> Value.I8(bits.toByte), 2 -> Value.Bool(signed)))))
    c match
      case _: Column.Bool => (TBoolean, None, None)
      case _: Column.Int64 => (TInt64, None, None)
      case Column.Ints(bits, signed, _, _) if bits <= 32 =>
        val conv = (bits, signed) match
          case (8, true) => 15; case (16, true) => 16; case (32, true) => 17
          case (8, false) => 11; case (16, false) => 12; case _ => 13
        (TInt32, Some(conv), integer(bits, signed))
      case Column.Ints(64, signed, _, _) => (TInt64, Some(if signed then 18 else 14), integer(64, signed))
      case _: Column.Float32 => (TFloat, None, None)
      case _: Column.Float64 => (TDouble, None, None)
      case _: Column.Utf8 => (TByteArray, Some(0), Some(Struct.of(1 -> Value.S(Struct.of()))))
      case _: Column.Binary => (TByteArray, None, None)
      case _: Column.FixedBinary => (TFixed, None, None)
      case _: Column.Date32 => (TInt32, Some(6), Some(Struct.of(6 -> Value.S(Struct.of()))))
      case Column.Timestamp(unit, zone, _, _) =>
        val u = unit match
          case TimeUnit.Milli => 1
          case TimeUnit.Micro => 2
          case TimeUnit.Nano => 3
          case TimeUnit.Second =>
            throw Refused(s"column '$name' is a timestamp in seconds: Parquet has millis, micros and nanos")
        val conv = if zone.isDefined && u == 1 then Some(9) else if zone.isDefined && u == 2 then Some(10) else None
        (TInt64, conv, Some(Struct.of(8 -> Value.S(Struct.of(
          1 -> Value.Bool(zone.isDefined),
          2 -> Value.S(Struct.of(u -> Value.S(Struct.of()))))))))
      case other =>
        throw Refused(s"column '$name' is ${Column.describe(other)}: not written yet (specs/parquet.md)")

  private def element(name: String, c: Column): Struct =
    val (physical, conv, logical) = typeOf(name, c)
    val fields = Map[Int, Value](
      1 -> Value.I32(physical),
      3 -> Value.I32(1),                   // OPTIONAL
      4 -> Value.Bin(name.getBytes("UTF-8"))) ++
      conv.map(6 -> Value.I32(_)) ++
      logical.map(10 -> Value.S(_)) ++
      (c match { case Column.FixedBinary(w, _, _) => Map(2 -> Value.I32(w)); case _ => Map.empty })
    Struct(fields)

  /** one v1 data page, uncompressed: definition levels, then the present
   * values PLAIN */
  private def dataPage(name: String, c: Column): Array[Byte] =
    val out = java.io.ByteArrayOutputStream()
    val valid = c.validity
    val levels = Rle.encode(valid.map(v => if v then 1 else 0), 1)
    def le32(v: Int): Unit = { out.write(v & 0xff); out.write((v >>> 8) & 0xff); out.write((v >>> 16) & 0xff); out.write((v >>> 24) & 0xff) }
    def le64(v: Long): Unit = { var k = 0; while k < 8 do { out.write(((v >>> (8 * k)) & 0xff).toInt); k += 1 } }
    le32(levels.length)
    out.write(levels)
    def each(f: Int => Unit): Unit =
      var i = 0
      while i < valid.length do { if valid(i) then f(i); i += 1 }
    c match
      case Column.Bool(v, _) =>
        val present = valid.indices.filter(valid).map(v).toArray
        val bits = new Array[Byte]((present.length + 7) / 8)
        for (x, i) <- present.zipWithIndex if x do bits(i >>> 3) = (bits(i >>> 3) | (1 << (i & 7))).toByte
        out.write(bits)
      case Column.Int64(v, _) => each(i => le64(v(i)))
      case Column.Ints(bits, _, v, _) => if bits <= 32 then each(i => le32(v(i).toInt)) else each(i => le64(v(i)))
      case Column.Float32(v, _) => each(i => le32(java.lang.Float.floatToRawIntBits(v(i))))
      case Column.Float64(v, _) => each(i => le64(java.lang.Double.doubleToRawLongBits(v(i))))
      case Column.Utf8(v, _) => each { i => val b = v(i).getBytes("UTF-8"); le32(b.length); out.write(b) }
      case Column.Binary(v, _) => each { i => le32(v(i).length); out.write(v(i)) }
      case Column.FixedBinary(w, v, _) => each { i =>
        if v(i).length != w then throw Refused(s"column '$name': a value of ${v(i).length} bytes in a fixed($w)")
        out.write(v(i)) }
      case Column.Date32(v, _) => each(i => le32(v(i)))
      case Column.Timestamp(_, _, v, _) => each(i => le64(v(i)))
      case other => throw Refused(s"column '$name' is ${Column.describe(other)}: not written yet (specs/parquet.md)")
    out.toByteArray

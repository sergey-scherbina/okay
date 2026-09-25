package okay.codec

import java.nio.charset.StandardCharsets.UTF_8

/**
 * Arrow IPC STREAMS, written and read by hand (py-arrow, specs/py-arrow.md)
 * for the five columns a foreign frame carries: int64, float64, utf8,
 * bool and the null type, every one nullable. A stream is a schema
 * message, record batches, and an end-of-stream marker; each message is
 * `FF FF FF FF`, its metadata's length, a FlatBuffers `Message`, and a
 * body of 8-byte-aligned buffers (the Arrow columnar format, version 5).
 *
 * Why not Arrow Java: it brings its own off-heap memory (netty or
 * unsafe) and `--add-opens` for what is, here, four primitive layouts
 * and a string one. okay writes its own CBOR for the same reason, and
 * pyarrow checks every byte this writes (TestArrowPy).
 *
 * A stream this does not understand — another column type, a compressed
 * body, a dictionary — is refused by name, and so is one cut short.
 */
object ArrowIpc:

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

  /** a table: named columns of one length, and the schema's metadata */
  final case class Table(cols: Vector[(String, Column)], metadata: Vector[(String, String)]):
    def rows: Int = cols.headOption.fold(0)(_._2.length)

  /** whether these bytes are an Arrow stream: its first message's
   * continuation marker, which begins no JSON text and no CBOR item of
   * the okay wire */
  def isStream(bytes: Array[Byte]): Boolean =
    bytes.length >= 4 && bytes(0) == -1 && bytes(1) == -1 && bytes(2) == -1 && bytes(3) == -1

  // ---- writing -------------------------------------------------------------

  def write(t: Table): Array[Byte] =
    val n = t.rows
    t.cols.find(_._2.length != n).foreach { (name, c) =>
      throw IllegalArgumentException(s"column '$name' has ${c.length} rows, the first has $n")
    }
    val out = Bytes()
    message(out, HeaderSchema, schema(t), Array.emptyByteArray)
    val (batch, body) = recordBatch(t)
    message(out, HeaderRecordBatch, batch, body)
    out.i32(-1); out.i32(0)                      // end of stream
    out.result()

  /** one encapsulated message: marker, metadata length (padded so the
   * body starts 8-aligned), the FlatBuffer, the body */
  private def message(out: Bytes, headerType: Int, header: Fb.Table, body: Array[Byte]): Unit =
    val msg = Fb.Table(Vector(
      Some(Fb.I16(MetadataV5)),
      Some(Fb.U8(headerType)),
      Some(header),
      Some(Fb.I64(body.length.toLong))))
    val fb = Fb.finish(msg)
    val padded = (fb.length + 8 + 7) / 8 * 8 - 8
    out.i32(-1); out.i32(padded)
    out.bytes(fb); out.zeros(padded - fb.length)
    out.bytes(body)

  private def schema(t: Table): Fb.Table =
    def field(name: String, c: Column): Fb.Table =
      val (typeId, tpe) = c match
        case Column.Int64(_, _) => (TypeInt, Fb.Table(Vector(Some(Fb.I32(64)), Some(Fb.Bool(true)))))
        case Column.Float64(_, _) => (TypeFloat, Fb.Table(Vector(Some(Fb.I16(PrecisionDouble)))))
        case Column.Utf8(_, _) => (TypeUtf8, Fb.Table(Vector.empty))
        case Column.Bool(_, _) => (TypeBool, Fb.Table(Vector.empty))
        case Column.Nulls(_) => (TypeNull, Fb.Table(Vector.empty))
      Fb.Table(Vector(
        Some(Fb.Str(name)), Some(Fb.Bool(true)), Some(Fb.U8(typeId)), Some(tpe),
        None, Some(Fb.Tables(Vector.empty))))
    val meta = t.metadata.map((k, v) => Fb.Table(Vector(Some(Fb.Str(k)), Some(Fb.Str(v)))))
    Fb.Table(Vector(
      Some(Fb.I16(0)),                                      // little-endian
      Some(Fb.Tables(t.cols.map(field))),
      if meta.isEmpty then None else Some(Fb.Tables(meta))))

  private def recordBatch(t: Table): (Fb.Table, Array[Byte]) =
    val body = Bytes()
    val nodes = Bytes()
    val buffers = Bytes()
    def buffer(write: Bytes => Unit): Unit =
      val start = body.size
      write(body)
      val len = body.size - start
      body.zeros((8 - len % 8) % 8)
      buffers.i64(start.toLong); buffers.i64(len.toLong)
    def validity(valid: Array[Boolean]): Int =
      val nulls = valid.count(!_)
      if nulls == 0 then buffer(_ => ())           // absent: every row valid
      else buffer(b => b.bytes(bitmap(valid)))
      nulls
    for (_, c) <- t.cols do
      val nulls = c match
        case Column.Int64(v, ok) =>
          val k = validity(ok); buffer(b => v.foreach(b.i64)); k
        case Column.Float64(v, ok) =>
          val k = validity(ok); buffer(b => v.foreach(x => b.i64(java.lang.Double.doubleToRawLongBits(x)))); k
        case Column.Bool(v, ok) =>
          val k = validity(ok); buffer(b => b.bytes(bitmap(v))); k
        case Column.Utf8(v, ok) =>
          val k = validity(ok)
          val encoded = v.indices.map(i => if ok(i) && v(i) != null then v(i).getBytes(UTF_8) else Array.emptyByteArray)
          buffer { b =>
            var at = 0
            b.i32(0)
            encoded.foreach { e => at += e.length; b.i32(at) }
          }
          buffer(b => encoded.foreach(b.bytes))
          k
        case Column.Nulls(n) => n
      nodes.i64(c.length.toLong); nodes.i64(nulls.toLong)
    val batch = Fb.Table(Vector(
      Some(Fb.I64(t.rows.toLong)),
      Some(Fb.Structs(nodes.result(), 16)),
      Some(Fb.Structs(buffers.result(), 16))))
    (batch, body.result())

  private def bitmap(bits: Array[Boolean]): Array[Byte] =
    val out = new Array[Byte]((bits.length + 7) / 8)
    var i = 0
    while i < bits.length do
      if bits(i) then out(i >> 3) = (out(i >> 3) | (1 << (i & 7))).toByte
      i += 1
    out

  // ---- reading -------------------------------------------------------------

  def read(bytes: Array[Byte]): Table =
    try readStream(bytes)
    catch case _: IndexOutOfBoundsException | _: NegativeArraySizeException =>
      refuse("an offset points outside the stream (cut short, or not Arrow)")

  private def readStream(bytes: Array[Byte]): Table =
    val in = In(bytes)
    var fields = Vector.empty[(String, Int, Fb.At)]
    var metadata = Vector.empty[(String, String)]
    var batches = Vector.empty[Vector[Column]]
    var ended = false
    var seenSchema = false
    while !ended do
      if in.remaining < 4 then refuse("the stream ended before its end-of-stream marker (cut short?)")
      var len = in.i32()
      if len == -1 then
        if in.remaining < 4 then refuse("the stream ended inside a message prefix (cut short?)")
        len = in.i32()
      if len == 0 then ended = true
      else
        if len < 0 || len > in.remaining then refuse(s"a message's metadata claims $len bytes; ${in.remaining} remain (cut short?)")
        val meta = in.slice(len)
        val msg = Fb.root(meta)
        val version = msg.i16(0, 0)
        if version != MetadataV5 then refuse(s"metadata version $version; this reads version 5 (V5 = $MetadataV5)")
        val bodyLen = msg.i64(3, 0L)
        if bodyLen < 0 || bodyLen > in.remaining then refuse(s"a message's body claims $bodyLen bytes; ${in.remaining} remain (cut short?)")
        val body = in.slice(bodyLen.toInt)
        msg.u8(1, 0) match
          case HeaderSchema =>
            val s = msg.table(2).getOrElse(refuse("a schema message without its schema"))
            fields = s.tables(1).map { f =>
              val name = f.str(0).getOrElse("")
              if f.table(4).isDefined then refuse(s"column '$name' is dictionary-encoded; decode it before sending")
              if f.tables(5).nonEmpty then refuse(s"column '$name' has child fields; this reads flat columns only")
              (name, f.u8(2, 0), f.table(3).getOrElse(Fb.At.empty))
            }
            metadata = s.tables(2).map(kv => (kv.str(0).getOrElse(""), kv.str(1).getOrElse("")))
            seenSchema = true
          case HeaderRecordBatch =>
            if !seenSchema then refuse("a record batch before the schema")
            val rb = msg.table(2).getOrElse(refuse("a record batch message without its batch"))
            if rb.table(3).isDefined then refuse("the record batch is compressed; send it uncompressed")
            batches :+= batch(rb, body, fields)
          case HeaderDictionary => refuse("a dictionary batch; decode dictionaries before sending")
          case other => refuse(s"message header type $other; this reads schemas and record batches")
    if !seenSchema then refuse("a stream without a schema")
    val cols = fields.indices.map { j =>
      val (name, typeId, _) = fields(j)
      (name, concat(typeId, batches.map(_(j)), name))
    }.toVector
    Table(cols, metadata)

  private def batch(rb: Fb.At, body: Array[Byte], fields: Vector[(String, Int, Fb.At)]): Vector[Column] =
    val rows = rb.i64(0, 0L)
    if rows < 0 || rows > Int.MaxValue then refuse(s"a batch of $rows rows")
    val nodes = rb.structs(1, 16)
    val bufs = rb.structs(2, 16)
    if nodes.length != fields.length then refuse(s"${nodes.length} field nodes for ${fields.length} fields")
    var b = 0
    def next(): Array[Byte] =
      if b >= bufs.length then refuse("fewer buffers than the columns need")
      val off = Fb.i64le(bufs(b), 0); val len = Fb.i64le(bufs(b), 8)
      b += 1
      if off < 0 || len < 0 || off + len > body.length then refuse(s"a buffer [$off, +$len) outside a body of ${body.length} bytes (cut short?)")
      java.util.Arrays.copyOfRange(body, off.toInt, (off + len).toInt)
    fields.indices.map { j =>
      val (name, typeId, tpe) = fields(j)
      val n = Fb.i64le(nodes(j), 0).toInt
      val nulls = Fb.i64le(nodes(j), 8)
      def valid(): Array[Boolean] =
        val bits = next()
        if bits.isEmpty then
          if nulls != 0 then refuse(s"column '$name' has $nulls nulls and no validity buffer")
          Array.fill(n)(true)
        else unpack(bits, n, name)
      def need(bytes: Array[Byte], len: Long): Unit =
        if bytes.length < len then refuse(s"column '$name': a buffer of ${bytes.length} bytes for $len (cut short?)")
      typeId match
        case TypeNull => Column.Nulls(n)
        case TypeInt =>
          val width = tpe.i32(0, 0)
          if width != 64 || !tpe.bool(1, false) then refuse(s"column '$name' is int$width${if tpe.bool(1, false) then "" else " unsigned"}; cast it to int64")
          val ok = valid(); val d = next(); need(d, 8L * n)
          Column.Int64(Array.tabulate(n)(i => Fb.i64le(d, 8 * i)), ok)
        case TypeFloat =>
          val p = tpe.i16(0, 0)
          if p != PrecisionDouble then refuse(s"column '$name' is a float of precision $p; cast it to float64")
          val ok = valid(); val d = next(); need(d, 8L * n)
          Column.Float64(Array.tabulate(n)(i => java.lang.Double.longBitsToDouble(Fb.i64le(d, 8 * i))), ok)
        case TypeBool =>
          val ok = valid(); val d = next(); need(d, (n + 7) / 8)
          Column.Bool(unpack(d, n, name), ok)
        case TypeUtf8 =>
          val ok = valid(); val offs = next(); val data = next()
          need(offs, 4L * (n + 1))
          Column.Utf8(Array.tabulate(n) { i =>
            val a = Fb.i32le(offs, 4 * i); val z = Fb.i32le(offs, 4 * (i + 1))
            if a < 0 || z < a || z > data.length then refuse(s"column '$name': string $i spans [$a, $z) of ${data.length} bytes")
            if ok(i) then String(data, a, z - a, UTF_8) else ""
          }, ok)
        case other => refuse(s"column '$name' has Arrow type ${typeName(other)}; this reads int64, float64, utf8, bool and null")
    }.toVector

  private def unpack(bits: Array[Byte], n: Int, name: String): Array[Boolean] =
    if bits.length < (n + 7) / 8 then refuse(s"column '$name': a bitmap of ${bits.length} bytes for $n rows (cut short?)")
    Array.tabulate(n)(i => (bits(i >> 3) >> (i & 7) & 1) == 1)

  /** one column out of the batches' parts, in order */
  private def concat(typeId: Int, parts: Vector[Column], name: String): Column =
    def bools(f: Column => Array[Boolean]) = parts.map(f).foldLeft(Array.emptyBooleanArray)(_ ++ _)
    typeId match
      case TypeNull => Column.Nulls(parts.map(_.length).sum)
      case _ if parts.isEmpty => typeId match
        case TypeInt => Column.Int64(Array.emptyLongArray, Array.emptyBooleanArray)
        case TypeFloat => Column.Float64(Array.emptyDoubleArray, Array.emptyBooleanArray)
        case TypeBool => Column.Bool(Array.emptyBooleanArray, Array.emptyBooleanArray)
        case _ => Column.Utf8(Array.empty[String], Array.emptyBooleanArray)
      case _ if parts.length == 1 => parts.head
      case _ => parts.head match
        case Column.Int64(_, _) => Column.Int64(parts.collect { case Column.Int64(v, _) => v }.foldLeft(Array.emptyLongArray)(_ ++ _),
          bools { case Column.Int64(_, ok) => ok; case _ => refuse(s"column '$name' changed type between batches") })
        case Column.Float64(_, _) => Column.Float64(parts.collect { case Column.Float64(v, _) => v }.foldLeft(Array.emptyDoubleArray)(_ ++ _),
          bools { case Column.Float64(_, ok) => ok; case _ => refuse(s"column '$name' changed type between batches") })
        case Column.Bool(_, _) => Column.Bool(bools { case Column.Bool(v, _) => v; case _ => refuse(s"column '$name' changed type between batches") },
          bools { case Column.Bool(_, ok) => ok; case _ => refuse(s"column '$name' changed type between batches") })
        case Column.Utf8(_, _) => Column.Utf8(parts.collect { case Column.Utf8(v, _) => v }.foldLeft(Array.empty[String])(_ ++ _),
          bools { case Column.Utf8(_, ok) => ok; case _ => refuse(s"column '$name' changed type between batches") })
        case Column.Nulls(_) => Column.Nulls(parts.map(_.length).sum)

  private def refuse(why: String): Nothing = throw IllegalStateException(s"not an Arrow stream this reads: $why")

  private def typeName(id: Int): String =
    Vector("NONE", "Null", "Int", "FloatingPoint", "Binary", "Utf8", "Bool", "Decimal", "Date", "Time",
      "Timestamp", "Interval", "List", "Struct", "Union", "FixedSizeBinary", "FixedSizeList", "Map",
      "Duration", "LargeBinary", "LargeUtf8", "LargeList", "RunEndEncoded", "BinaryView", "Utf8View",
      "ListView", "LargeListView").lift(id).getOrElse(s"#$id")

  // Message.fbs / Schema.fbs constants
  private val MetadataV5 = 4
  private val HeaderSchema = 1
  private val HeaderDictionary = 2
  private val HeaderRecordBatch = 3
  private val TypeNull = 1
  private val TypeInt = 2
  private val TypeFloat = 3
  private val TypeUtf8 = 5
  private val TypeBool = 6
  private val PrecisionDouble = 2

  // ---- bytes, little-endian ------------------------------------------------

  private final class Bytes:
    private var buf = new Array[Byte](256)
    private var n = 0
    def size: Int = n
    private def room(k: Int): Unit =
      if n + k > buf.length then buf = java.util.Arrays.copyOf(buf, math.max(buf.length * 2, n + k))
    def u8(v: Int): Unit = { room(1); buf(n) = v.toByte; n += 1 }
    def i16(v: Int): Unit = { u8(v); u8(v >> 8) }
    def i32(v: Int): Unit = { room(4); var i = 0; while i < 4 do { buf(n + i) = (v >>> (8 * i)).toByte; i += 1 }; n += 4 }
    def i64(v: Long): Unit = { room(8); var i = 0; while i < 8 do { buf(n + i) = (v >>> (8 * i)).toByte; i += 1 }; n += 8 }
    def bytes(b: Array[Byte]): Unit = { room(b.length); System.arraycopy(b, 0, buf, n, b.length); n += b.length }
    def zeros(k: Int): Unit = { room(k); n += k }
    def pokeByte(at: Int, v: Byte): Unit = buf(at) = v
    def patch32(at: Int, v: Int): Unit = { var i = 0; while i < 4 do { buf(at + i) = (v >>> (8 * i)).toByte; i += 1 } }
    def result(): Array[Byte] = java.util.Arrays.copyOf(buf, n)

  private final class In(b: Array[Byte]):
    private var at = 0
    def remaining: Int = b.length - at
    def i32(): Int = { val v = Fb.i32le(b, at); at += 4; v }
    def slice(k: Int): Array[Byte] = { val s = java.util.Arrays.copyOfRange(b, at, at + k); at += k; s }

  // ---- FlatBuffers, the little this needs ----------------------------------

  /**
   * FlatBuffers written FRONT to back: a table's vtable, then the table,
   * then what its offsets point at, so every uoffset is forward (as the
   * format requires) and a table's soffset to its vtable is positive.
   * Scalars sit at their natural alignment from an 8-aligned table start;
   * vectors of 16-byte structs start at a position 8-aligned, as the
   * Arrow C++ verifier checks.
   */
  private object Fb:
    sealed trait Node
    final case class U8(v: Int) extends Node
    final case class Bool(v: Boolean) extends Node
    final case class I16(v: Int) extends Node
    final case class I32(v: Int) extends Node
    final case class I64(v: Long) extends Node
    final case class Str(v: String) extends Node
    final case class Table(fields: Vector[Option[Node]]) extends Node
    final case class Tables(items: Vector[Table]) extends Node
    /** a vector of fixed-size structs, pre-encoded little-endian */
    final case class Structs(bytes: Array[Byte], size: Int) extends Node

    private def scalarSize(n: Node): Int = n match
      case U8(_) | Bool(_) => 1
      case I16(_) => 2
      case I32(_) => 4
      case I64(_) => 8
      case _ => 4                                     // a uoffset

    def finish(root: Table): Array[Byte] =
      val out = Bytes()
      out.i32(0)                                      // the root uoffset, patched
      val at = table(out, root)
      out.patch32(0, at)
      out.result()

    private def align(out: Bytes, k: Int): Unit = out.zeros((k - out.size % k) % k)

    private def table(out: Bytes, t: Table): Int =
      // the layout: soffset at 0, then fields largest first, each aligned
      val present = t.fields.zipWithIndex.collect { case (Some(n), i) => (n, i) }
      val order = present.sortBy((n, _) => -scalarSize(n))
      var size = 4
      val offsetOf = scala.collection.mutable.Map.empty[Int, Int]
      for (n, i) <- order do
        val s = scalarSize(n)
        size = (size + s - 1) / s * s
        offsetOf(i) = size
        size += s
      size = (size + 3) / 4 * 4
      // the vtable, then the table 8-aligned after it
      align(out, 2)
      val vt = out.size
      out.i16(4 + 2 * t.fields.length); out.i16(size)
      t.fields.indices.foreach(i => out.i16(offsetOf.getOrElse(i, 0)))
      align(out, 8)
      val tbl = out.size
      out.i32(tbl - vt)
      out.zeros(size - 4)
      val patches = Vector.newBuilder[(Int, Node)]
      for (n, i) <- present do
        val at = tbl + offsetOf(i)
        n match
          case U8(v) => poke(out, at, v.toLong, 1)
          case Bool(v) => poke(out, at, if v then 1L else 0L, 1)
          case I16(v) => poke(out, at, v.toLong, 2)
          case I32(v) => poke(out, at, v.toLong, 4)
          case I64(v) => poke(out, at, v, 8)
          case other => patches += ((at, other))
      for (at, child) <- patches.result() do
        val target = child match
          case s: Str => string(out, s.v)
          case t: Table => table(out, t)
          case ts: Tables => tables(out, ts.items)
          case st: Structs => structs(out, st)
          case other => throw IllegalStateException(s"not an offset node: $other")
        out.patch32(at, target - at)
      tbl

    private def poke(out: Bytes, at: Int, v: Long, k: Int): Unit =
      var i = 0
      while i < k do { out.pokeByte(at + i, (v >>> (8 * i)).toByte); i += 1 }

    private def string(out: Bytes, s: String): Int =
      val b = s.getBytes(UTF_8)
      align(out, 4)
      val at = out.size
      out.i32(b.length); out.bytes(b); out.u8(0)
      at

    private def tables(out: Bytes, items: Vector[Table]): Int =
      align(out, 4)
      val at = out.size
      out.i32(items.length)
      val slots = items.indices.map(_ => { val s = out.size; out.i32(0); s })
      items.zip(slots).foreach { (t, slot) => out.patch32(slot, table(out, t) - slot) }
      at

    private def structs(out: Bytes, st: Structs): Int =
      // the elements 8-aligned: the length prefix at 4 mod 8
      out.zeros(((4 - out.size % 8) + 8) % 8)
      val at = out.size
      out.i32(st.bytes.length / st.size)
      out.bytes(st.bytes)
      at

    // ---- reading ----

    /** a table at a position of a buffer */
    final class At(b: Array[Byte], pos: Int):
      private val vt = if b.isEmpty then 0 else pos - i32le(b, pos)
      private val vtLen = if b.isEmpty then 0 else u16le(b, vt)
      private def field(i: Int): Int =
        if b.isEmpty || 4 + 2 * i >= vtLen then 0 else u16le(b, vt + 4 + 2 * i)
      def u8(i: Int, dflt: Int): Int = { val o = field(i); if o == 0 then dflt else b(pos + o) & 0xff }
      def bool(i: Int, dflt: Boolean): Boolean = { val o = field(i); if o == 0 then dflt else b(pos + o) != 0 }
      def i16(i: Int, dflt: Int): Int = { val o = field(i); if o == 0 then dflt else (u16le(b, pos + o).toShort: Int) }
      def i32(i: Int, dflt: Int): Int = { val o = field(i); if o == 0 then dflt else i32le(b, pos + o) }
      def i64(i: Int, dflt: Long): Long = { val o = field(i); if o == 0 then dflt else i64le(b, pos + o) }
      private def target(i: Int): Option[Int] =
        val o = field(i)
        if o == 0 then None else Some(pos + o + i32le(b, pos + o))
      def table(i: Int): Option[At] = target(i).map(At(b, _))
      def str(i: Int): Option[String] = target(i).map(p => String(b, p + 4, i32le(b, p), UTF_8))
      def tables(i: Int): Vector[At] = target(i).fold(Vector.empty) { p =>
        Vector.tabulate(i32le(b, p))(k => { val s = p + 4 + 4 * k; At(b, s + i32le(b, s)) })
      }
      def structs(i: Int, size: Int): Vector[Array[Byte]] = target(i).fold(Vector.empty) { p =>
        Vector.tabulate(i32le(b, p))(k => java.util.Arrays.copyOfRange(b, p + 4 + size * k, p + 4 + size * (k + 1)))
      }

    object At:
      val empty: At = At(Array.emptyByteArray, 0)

    def root(b: Array[Byte]): At =
      if b.length < 4 then refuse("a message's metadata is shorter than its root offset")
      try At(b, i32le(b, 0))
      catch case _: IndexOutOfBoundsException => refuse("a message's metadata points outside itself")

    def u16le(b: Array[Byte], at: Int): Int = (b(at) & 0xff) | (b(at + 1) & 0xff) << 8
    def i32le(b: Array[Byte], at: Int): Int =
      (b(at) & 0xff) | (b(at + 1) & 0xff) << 8 | (b(at + 2) & 0xff) << 16 | (b(at + 3) & 0xff) << 24
    def i64le(b: Array[Byte], at: Int): Long =
      (i32le(b, at).toLong & 0xffffffffL) | (i32le(b, at + 4).toLong << 32)

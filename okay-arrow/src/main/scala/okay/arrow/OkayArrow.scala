package okay.arrow

import java.nio.charset.StandardCharsets.UTF_8

/**
 * OURS (okay-arrow's default implementation): Arrow IPC STREAMS written and
 * read by hand, on the JVM, Scala.js and Scala Native, for every type of
 * the model (`Column`): ints of every width, floats (half precision read
 * as float32), bool, utf8 and binary and their large forms, fixed-size
 * binary, decimal128, dates, timestamps, durations, lists and large lists,
 * structs, the null type; dictionary-encoded columns are decoded on read.
 *
 * A stream is a schema message, dictionary batches, record batches and an
 * end-of-stream marker; each message is `FF FF FF FF`, its metadata's
 * length, a FlatBuffers `Message`, and a body of 8-byte-aligned buffers
 * (the Arrow columnar format, version 5). A column's field node and its
 * buffers come in PRE-ORDER: the column, then its children, depth first.
 *
 * Why not Arrow Java for this: it brings its own off-heap memory (netty or
 * unsafe) and `--add-opens`, and it runs on the JVM only. pyarrow and Arrow
 * Java check what this writes (TestArrowPy, TestArrowJavaInterop,
 * TestApacheArrow).
 *
 * What the model does not hold — decimal256, views, unions, maps, run-end
 * encoding, intervals, times of day, a compressed body — is refused by
 * name, and so is a stream cut short.
 */
object OkayArrow extends ArrowCodec:

  def name = "okay"

  // ---- writing -------------------------------------------------------------

  def write(t: Table): Array[Byte] =
    val n = t.rows
    t.cols.find(_._2.length != n).foreach { (name, c) =>
      throw IllegalArgumentException(s"column '$name' has ${c.length} rows, the first has $n")
    }
    // the plan first, so the stream is written ONCE into an array of its
    // exact size (arrow-ipc-fast: growing by doubling and copying out made
    // 129 MB of garbage for a 14 MB stream)
    val nodes = Bytes()
    val buffers = Bytes()
    val fills = Vector.newBuilder[(Int, (Array[Byte], Int) => Unit)]
    var bodyLen = 0L
    def add(len: Int, fill: (Array[Byte], Int) => Unit): Unit =
      buffers.i64(bodyLen); buffers.i64(len.toLong)
      fills += ((len, fill))
      bodyLen += pad8(len)
    def validity(ok: Array[Boolean]): Unit =
      if ok.forall(identity) then add(0, (_, _) => ())            // absent: every row valid
      else add((ok.length + 7) / 8, (out, at) => bits(ok, out, at))
    def offsets(lengths: Array[Int]): Array[Int] =
      val offs = new Array[Int](lengths.length + 1)
      var total = 0L
      var i = 0
      while i < lengths.length do
        total += lengths(i)
        if total > Int.MaxValue then throw IllegalArgumentException("a column past 2 GiB of values: Arrow's large types, not written here")
        offs(i + 1) = total.toInt
        i += 1
      offs
    def column(c: Column): Unit =
      val rows = c.length
      nodes.i64(rows.toLong)
      nodes.i64((c match { case Column.Nulls(k) => k; case other => other.validity.count(!_) }).toLong)
      c match
        case Column.Nulls(_) => ()
        case Column.Int64(v, ok) => validity(ok); add(8 * rows, (out, at) => putLongs(out, at, v))
        case Column.Float64(v, ok) => validity(ok); add(8 * rows, (out, at) => putDoubles(out, at, v))
        case Column.Float32(v, ok) => validity(ok); add(4 * rows, (out, at) => putFloats(out, at, v))
        case Column.Bool(v, ok) => validity(ok); add((rows + 7) / 8, (out, at) => bits(v, out, at))
        case Column.Ints(b, _, v, ok) =>
          if !Set(8, 16, 32, 64).contains(b) then throw IllegalArgumentException(s"an int of $b bits: Arrow's are 8, 16, 32 and 64")
          validity(ok); add(b / 8 * rows, (out, at) => putWidth(out, at, v, b / 8))
        case Column.Date32(v, ok) => validity(ok); add(4 * rows, (out, at) => putInts(out, at, v))
        case Column.Date64(v, ok) => validity(ok); add(8 * rows, (out, at) => putLongs(out, at, v))
        case Column.Timestamp(_, _, v, ok) => validity(ok); add(8 * rows, (out, at) => putLongs(out, at, v))
        case Column.Duration(_, v, ok) => validity(ok); add(8 * rows, (out, at) => putLongs(out, at, v))
        case Column.Utf8(v, ok) =>
          validity(ok)
          // each string's UTF-8 length counted without encoding; the bytes
          // are then encoded straight into the stream
          val offs = offsets(Array.tabulate(rows)(i => if ok(i) && v(i) != null then Utf8.length(v(i)) else 0))
          add(4 * (rows + 1), (out, at) => putInts(out, at, offs))
          add(offs(rows), (out, at) =>
            var j = 0
            while j < rows do
              if ok(j) && v(j) != null then Utf8.encode(v(j), out, at + offs(j))
              j += 1)
        case Column.Binary(v, ok) =>
          validity(ok)
          val offs = offsets(Array.tabulate(rows)(i => if ok(i) && v(i) != null then v(i).length else 0))
          add(4 * (rows + 1), (out, at) => putInts(out, at, offs))
          add(offs(rows), (out, at) =>
            var j = 0
            while j < rows do
              if ok(j) && v(j) != null then System.arraycopy(v(j), 0, out, at + offs(j), v(j).length)
              j += 1)
        case Column.FixedBinary(w, v, ok) =>
          v.indices.find(i => ok(i) && (v(i) == null || v(i).length != w)).foreach { i =>
            throw IllegalArgumentException(s"a fixed_size_binary($w) value at row $i is ${Option(v(i)).fold(0)(_.length)} bytes")
          }
          validity(ok)
          add(w * rows, (out, at) =>
            var j = 0
            while j < rows do
              if ok(j) then System.arraycopy(v(j), 0, out, at + w * j, w)
              j += 1)
        case Column.Decimal(p, _, v, ok) =>
          if p < 1 || p > 38 then throw IllegalArgumentException(s"a decimal of precision $p: decimal128 holds 1 to 38 digits")
          validity(ok)
          add(16 * rows, (out, at) =>
            var j = 0
            while j < rows do
              if ok(j) then putDecimal(out, at + 16 * j, v(j))
              j += 1)
        case Column.ListOf(offs, child, ok) =>
          validity(ok); add(4 * (rows + 1), (out, at) => putInts(out, at, offs))
          column(child)
        case Column.Struct(fs, ok) =>
          fs.find(_._2.length != rows).foreach { (name, f) =>
            throw IllegalArgumentException(s"struct field '$name' has ${f.length} rows, the struct has $rows")
          }
          validity(ok)
          fs.foreach((_, f) => column(f))
    t.cols.foreach((_, c) => column(c))
    if bodyLen > Int.MaxValue - 1024 then throw IllegalArgumentException(s"a batch of $bodyLen bytes: past what one JVM array holds")
    val schemaMsg = messageHead(HeaderSchema, schema(t), 0L)
    val batch = Fb.Table(Vector(
      Some(Fb.I64(n.toLong)),
      Some(Fb.Structs(nodes.result(), 16)),
      Some(Fb.Structs(buffers.result(), 16))))
    val batchHead = messageHead(HeaderRecordBatch, batch, bodyLen)
    val out = new Array[Byte](schemaMsg.length + batchHead.length + bodyLen.toInt + 8)
    System.arraycopy(schemaMsg, 0, out, 0, schemaMsg.length)
    System.arraycopy(batchHead, 0, out, schemaMsg.length, batchHead.length)
    var at = schemaMsg.length + batchHead.length
    for (len, fill) <- fills.result() do
      fill(out, at)
      at += pad8(len)
    putInt(out, at, -1)                           // end of stream: marker, then 0
    out

  private def pad8(len: Int): Int = (len + 7) & ~7

  /** a message's prefix and metadata, padded so a body after it starts
   * 8-aligned (the body itself is written in place by `write`) */
  private def messageHead(headerType: Int, header: Fb.Table, bodyLen: Long): Array[Byte] =
    val fb = Fb.finish(Fb.Table(Vector(
      Some(Fb.I16(MetadataV5)), Some(Fb.U8(headerType)), Some(header), Some(Fb.I64(bodyLen)))))
    val padded = (fb.length + 8 + 7) / 8 * 8 - 8
    val out = new Array[Byte](8 + padded)
    putInt(out, 0, -1); putInt(out, 4, padded)
    System.arraycopy(fb, 0, out, 8, fb.length)
    out

  private def field(name: String, c: Column): Fb.Table =
    def t(fields: Option[Fb.Node]*) = Fb.Table(fields.toVector)
    val (typeId, tpe, children) = c match
      case Column.Int64(_, _) => (TypeInt, t(Some(Fb.I32(64)), Some(Fb.Bool(true))), Vector.empty)
      case Column.Ints(b, s, _, _) => (TypeInt, t(Some(Fb.I32(b)), Some(Fb.Bool(s))), Vector.empty)
      case Column.Float64(_, _) => (TypeFloat, t(Some(Fb.I16(PrecisionDouble))), Vector.empty)
      case Column.Float32(_, _) => (TypeFloat, t(Some(Fb.I16(PrecisionSingle))), Vector.empty)
      case Column.Utf8(_, _) => (TypeUtf8, t(), Vector.empty)
      case Column.Binary(_, _) => (TypeBinary, t(), Vector.empty)
      case Column.Bool(_, _) => (TypeBool, t(), Vector.empty)
      case Column.Nulls(_) => (TypeNull, t(), Vector.empty)
      case Column.FixedBinary(w, _, _) => (TypeFixedBinary, t(Some(Fb.I32(w))), Vector.empty)
      case Column.Decimal(p, s, _, _) => (TypeDecimal, t(Some(Fb.I32(p)), Some(Fb.I32(s)), Some(Fb.I32(128))), Vector.empty)
      case Column.Date32(_, _) => (TypeDate, t(Some(Fb.I16(0))), Vector.empty)
      case Column.Date64(_, _) => (TypeDate, t(Some(Fb.I16(1))), Vector.empty)
      case Column.Timestamp(u, z, _, _) => (TypeTimestamp, t(Some(Fb.I16(u.ordinal)), z.map(Fb.Str(_))), Vector.empty)
      case Column.Duration(u, _, _) => (TypeDuration, t(Some(Fb.I16(u.ordinal))), Vector.empty)
      case Column.ListOf(_, child, _) => (TypeList, t(), Vector(field("item", child)))
      case Column.Struct(fs, _) => (TypeStruct, t(), fs.map(field))
    Fb.Table(Vector(
      Some(Fb.Str(name)), Some(Fb.Bool(true)), Some(Fb.U8(typeId)), Some(tpe),
      None, Some(Fb.Tables(children))))

  private def schema(t: Table): Fb.Table =
    val meta = t.metadata.map((k, v) => Fb.Table(Vector(Some(Fb.Str(k)), Some(Fb.Str(v)))))
    Fb.Table(Vector(
      Some(Fb.I16(0)),                                      // little-endian
      Some(Fb.Tables(t.cols.map(field))),
      if meta.isEmpty then None else Some(Fb.Tables(meta))))

  /** a bitmap, LSB first, written at `at` */
  private def bits(v: Array[Boolean], out: Array[Byte], at: Int): Unit =
    var i = 0
    while i < v.length do
      if v(i) then out(at + (i >> 3)) = (out(at + (i >> 3)) | (1 << (i & 7))).toByte
      i += 1

  // bulk little-endian copies: a view buffer's put/get is one intrinsic
  // copy on the JVM, where a loop of shifts was one store per byte
  private def le(out: Array[Byte], at: Int, len: Int): java.nio.ByteBuffer =
    java.nio.ByteBuffer.wrap(out, at, len).order(java.nio.ByteOrder.LITTLE_ENDIAN)
  private def putLongs(out: Array[Byte], at: Int, v: Array[Long]): Unit = { val _ = le(out, at, 8 * v.length).asLongBuffer().put(v) }
  private def putDoubles(out: Array[Byte], at: Int, v: Array[Double]): Unit = { val _ = le(out, at, 8 * v.length).asDoubleBuffer().put(v) }
  private def putFloats(out: Array[Byte], at: Int, v: Array[Float]): Unit = { val _ = le(out, at, 4 * v.length).asFloatBuffer().put(v) }
  private def putInts(out: Array[Byte], at: Int, v: Array[Int]): Unit = { val _ = le(out, at, 4 * v.length).asIntBuffer().put(v) }
  private def putInt(out: Array[Byte], at: Int, v: Int): Unit =
    out(at) = v.toByte; out(at + 1) = (v >> 8).toByte; out(at + 2) = (v >> 16).toByte; out(at + 3) = (v >> 24).toByte
  /** ints of `width` bytes, the low bytes of each Long */
  private def putWidth(out: Array[Byte], at: Int, v: Array[Long], width: Int): Unit =
    if width == 8 then putLongs(out, at, v)
    else
      var i = 0
      while i < v.length do
        var k = 0
        while k < width do { out(at + width * i + k) = (v(i) >>> (8 * k)).toByte; k += 1 }
        i += 1
  /** an unscaled decimal128: 16 bytes of two's complement, little-endian */
  private def putDecimal(out: Array[Byte], at: Int, v: BigInt): Unit =
    val be = v.toByteArray
    if be.length > 16 then throw IllegalArgumentException(s"$v does not fit decimal128")
    val fill: Byte = if v.signum < 0 then -1 else 0
    var k = 0
    while k < 16 do
      out(at + k) = if k < be.length then be(be.length - 1 - k) else fill
      k += 1

  // ---- reading -------------------------------------------------------------

  def read(bytes: Array[Byte]): Table =
    try readStream(bytes)
    catch case _: IndexOutOfBoundsException | _: NegativeArraySizeException =>
      refuse("an offset points outside the stream (cut short, or not Arrow)")

  /** a field of the schema, as it was declared: its dictionary's id and
   * index type when it is dictionary-encoded */
  private final case class Field(name: String, typeId: Int, tpe: Fb.At, children: Vector[Field],
                                 dictionary: Option[(Long, Int, Boolean)])

  private def parseField(f: Fb.At): Field =
    val name = f.str(0).getOrElse("")
    val dict = f.table(4).map { d =>
      val idx = d.table(1)
      (d.i64(0, 0L), idx.fold(32)(_.i32(0, 32)), idx.fold(true)(_.bool(1, true)))
    }
    Field(name, f.u8(2, 0), f.table(3).getOrElse(Fb.At.empty), f.tables(5).map(parseField), dict)

  private def readStream(bytes: Array[Byte]): Table =
    val in = In(bytes)
    var fields = Vector.empty[Field]
    var metadata = Vector.empty[(String, String)]
    var batches = Vector.empty[Vector[Column]]
    var dictionaries = Map.empty[Long, Column]
    var ended = false
    var seenSchema = false
    // every dictionary-encoded field, at any depth, by its dictionary id
    def dictFields(fs: Vector[Field]): Vector[Field] =
      fs.flatMap(f => (if f.dictionary.isDefined then Vector(f) else Vector.empty) ++ dictFields(f.children))
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
        val body = in.at
        in.skip(bodyLen.toInt)
        msg.u8(1, 0) match
          case HeaderSchema =>
            val s = msg.table(2).getOrElse(refuse("a schema message without its schema"))
            fields = s.tables(1).map(parseField)
            metadata = s.tables(2).map(kv => (kv.str(0).getOrElse(""), kv.str(1).getOrElse("")))
            seenSchema = true
          case HeaderRecordBatch =>
            if !seenSchema then refuse("a record batch before the schema")
            val rb = msg.table(2).getOrElse(refuse("a record batch message without its batch"))
            batches :+= Batch(rb, bytes, body, bodyLen.toInt, dictionaries).columns(fields)
          case HeaderDictionary =>
            if !seenSchema then refuse("a dictionary batch before the schema")
            val db = msg.table(2).getOrElse(refuse("a dictionary message without its batch"))
            val id = db.i64(0, 0L)
            val f = dictFields(fields).find(_.dictionary.exists(_._1 == id))
              .getOrElse(refuse(s"a dictionary batch for id $id, which no field names"))
            val rb = db.table(1).getOrElse(refuse(s"dictionary $id without its data"))
            val values = Batch(rb, bytes, body, bodyLen.toInt, dictionaries).columns(Vector(f.copy(dictionary = None))).head
            dictionaries = dictionaries.updated(id,
              if db.bool(2, false) then dictionaries.get(id).fold(values)(prev => Column.concat(Vector(prev, values)))
              else values)
          case other => refuse(s"message header type $other; this reads schemas, dictionaries and record batches")
    if !seenSchema then refuse("a stream without a schema")
    val cols = fields.indices.map { j =>
      val f = fields(j)
      val parts = batches.map(_(j))
      (f.name, if parts.nonEmpty then concat(parts, f.name) else empty(f))
    }.toVector
    Table(cols, metadata)

  /** one record batch's body, read IN PLACE: a buffer is a position and a
   * length in `bytes`, copied once into its column */
  private final class Batch(rb: Fb.At, bytes: Array[Byte], body: Int, bodyLen: Int, dictionaries: Map[Long, Column]):
    private val rows = rb.i64(0, 0L)
    if rows < 0 || rows > Int.MaxValue then refuse(s"a batch of $rows rows")
    if rb.table(3).isDefined then refuse("the record batch is compressed; LZ4 and ZSTD bodies are not read yet (specs/okay-arrow.md)")
    private val nodes = rb.structs(1, 16)
    private val bufs = rb.structs(2, 16)
    private var node = 0
    private var buf = 0

    def columns(fields: Vector[Field]): Vector[Column] = fields.map(column)

    /** the next buffer: its position in `bytes` and its length */
    private def next(): (Int, Int) =
      if buf >= bufs.length then refuse("fewer buffers than the columns need")
      val off = Fb.i64le(bufs(buf), 0); val len = Fb.i64le(bufs(buf), 8)
      buf += 1
      if off < 0 || len < 0 || off + len > bodyLen then refuse(s"a buffer [$off, +$len) outside a body of $bodyLen bytes (cut short?)")
      (body + off.toInt, len.toInt)

    private def column(f: Field): Column =
      if node >= nodes.length then refuse("fewer field nodes than the columns need")
      val n = Fb.i64le(nodes(node), 0).toInt
      val nulls = Fb.i64le(nodes(node), 8)
      node += 1
      val name = f.name
      def valid(): Array[Boolean] =
        val (at, len) = next()
        if len == 0 then
          if nulls != 0 then refuse(s"column '$name' has $nulls nulls and no validity buffer")
          Array.fill(n)(true)
        else unpack(bytes, at, len, n, name)
      def data(width: Int): Int =
        val (at, len) = next()
        if len < width.toLong * n then refuse(s"column '$name': a buffer of $len bytes for ${width.toLong * n} (cut short?)")
        at
      /** offsets of `width` bytes (4, or 8 for the large forms), as Ints */
      def offsets(width: Int): Array[Int] =
        val (at, len) = next()
        if len < width.toLong * (n + 1) then refuse(s"column '$name': offsets of $len bytes for ${n + 1} (cut short?)")
        val offs = new Array[Int](n + 1)
        if width == 4 then le(bytes, at, 4 * (n + 1)).asIntBuffer().get(offs)
        else
          var i = 0
          while i <= n do
            val o = Fb.i64le(bytes, at + 8 * i)
            if o < 0 || o > Int.MaxValue then refuse(s"column '$name': a large offset $o past what one array holds")
            offs(i) = o.toInt
            i += 1
        var i = 0
        while i < n do
          if offs(i + 1) < offs(i) then refuse(s"column '$name': offsets fall at row $i")
          i += 1
        offs
      def values(offs: Array[Int], len: Int): Unit =
        if offs(n) > len then refuse(s"column '$name': values end at ${offs(n)} of $len bytes (cut short?)")
      f.dictionary match
        case Some((id, bits, signed)) =>
          // the indices, then the dictionary's values gathered by them
          val ok = valid()
          val idx = widthInts(bytes, data(bits / 8), n, bits / 8, signed)
          val dict = dictionaries.getOrElse(id, refuse(s"column '$name' uses dictionary $id before its batch"))
          val at = Array.tabulate(n) { i =>
            val k = idx(i)
            if ok(i) && (k < 0 || k >= dict.length) then refuse(s"column '$name': index $k at row $i outside a dictionary of ${dict.length}")
            if ok(i) then k.toInt else 0
          }
          dict.take(at, ok)
        case None => f.typeId match
          case TypeNull => Column.Nulls(n)
          case TypeInt =>
            val bits = f.tpe.i32(0, 0)
            val signed = f.tpe.bool(1, false)
            if !Set(8, 16, 32, 64).contains(bits) then refuse(s"column '$name' is an int of $bits bits")
            val ok = valid(); val at = data(bits / 8)
            if bits == 64 && signed then
              val v = new Array[Long](n); le(bytes, at, 8 * n).asLongBuffer().get(v); Column.Int64(v, ok)
            else Column.Ints(bits, signed, widthInts(bytes, at, n, bits / 8, signed), ok)
          case TypeFloat => f.tpe.i16(0, 0) match
            case PrecisionDouble =>
              val ok = valid(); val at = data(8)
              val v = new Array[Double](n); le(bytes, at, 8 * n).asDoubleBuffer().get(v); Column.Float64(v, ok)
            case PrecisionSingle =>
              val ok = valid(); val at = data(4)
              val v = new Array[Float](n); le(bytes, at, 4 * n).asFloatBuffer().get(v); Column.Float32(v, ok)
            case _ =>                                               // half: widened to float32
              val ok = valid(); val at = data(2)
              Column.Float32(Array.tabulate(n)(i => halfToFloat(Fb.u16le(bytes, at + 2 * i))), ok)
          case TypeBool =>
            val ok = valid(); val (at, len) = next()
            Column.Bool(unpack(bytes, at, len, n, name), ok)
          case TypeUtf8 | TypeLargeUtf8 =>
            val ok = valid(); val offs = offsets(if f.typeId == TypeUtf8 then 4 else 8); val (dat, dlen) = next()
            values(offs, dlen)
            Column.Utf8(Array.tabulate(n)(i => if ok(i) then String(bytes, dat + offs(i), offs(i + 1) - offs(i), UTF_8) else ""), ok)
          case TypeBinary | TypeLargeBinary =>
            val ok = valid(); val offs = offsets(if f.typeId == TypeBinary then 4 else 8); val (dat, dlen) = next()
            values(offs, dlen)
            Column.Binary(Array.tabulate(n)(i => java.util.Arrays.copyOfRange(bytes, dat + offs(i), dat + offs(i + 1))), ok)
          case TypeFixedBinary =>
            val w = f.tpe.i32(0, 0)
            if w < 0 then refuse(s"column '$name': a fixed_size_binary of width $w")
            val ok = valid(); val at = data(w)
            Column.FixedBinary(w, Array.tabulate(n)(i => java.util.Arrays.copyOfRange(bytes, at + w * i, at + w * (i + 1))), ok)
          case TypeDecimal =>
            val width = f.tpe.i32(2, 128)
            if width != 128 then refuse(s"column '$name' is a decimal$width; the model holds decimal128")
            val ok = valid(); val at = data(16)
            Column.Decimal(f.tpe.i32(0, 0), f.tpe.i32(1, 0),
              Array.tabulate(n)(i => if ok(i) then decimal(bytes, at + 16 * i) else BigInt(0)), ok)
          case TypeDate =>
            if f.tpe.i16(0, 1) == 0 then
              val ok = valid(); val at = data(4)
              val v = new Array[Int](n); le(bytes, at, 4 * n).asIntBuffer().get(v); Column.Date32(v, ok)
            else
              val ok = valid(); val at = data(8)
              val v = new Array[Long](n); le(bytes, at, 8 * n).asLongBuffer().get(v); Column.Date64(v, ok)
          case TypeTimestamp =>
            val unit = timeUnit(f.tpe.i16(0, 0), name)
            val ok = valid(); val at = data(8)
            val v = new Array[Long](n); le(bytes, at, 8 * n).asLongBuffer().get(v)
            Column.Timestamp(unit, f.tpe.str(1), v, ok)
          case TypeDuration =>
            val unit = timeUnit(f.tpe.i16(0, 1), name)
            val ok = valid(); val at = data(8)
            val v = new Array[Long](n); le(bytes, at, 8 * n).asLongBuffer().get(v)
            Column.Duration(unit, v, ok)
          case TypeList | TypeLargeList =>
            if f.children.length != 1 then refuse(s"list column '$name' with ${f.children.length} children")
            val ok = valid(); val offs = offsets(if f.typeId == TypeList then 4 else 8)
            val child = column(f.children.head)
            if offs(n) > child.length then refuse(s"list column '$name' ends at ${offs(n)} of ${child.length} child rows")
            normalised(offs, child, ok)
          case TypeStruct =>
            val ok = valid()
            val fs = f.children.map(c => c.name -> column(c))
            fs.find(_._2.length != n).foreach((cn, c) => refuse(s"struct column '$name': field '$cn' has ${c.length} rows for $n"))
            Column.Struct(fs, ok)
          case other => refuse(s"column '$name' has Arrow type ${typeName(other)}; the model does not hold it")

  /** a list whose offsets start at 0 and end at its child's length: what
   * the model's `concat` and `take` assume */
  private def normalised(offs: Array[Int], child: Column, ok: Array[Boolean]): Column =
    if offs(0) == 0 && offs(offs.length - 1) == child.length then Column.ListOf(offs, child, ok)
    else
      val from = offs(0)
      val used = Array.tabulate(offs(offs.length - 1) - from)(_ + from)
      Column.ListOf(offs.map(_ - from), child.take(used, Array.fill(used.length)(true)), ok)

  private def timeUnit(u: Int, name: String): TimeUnit =
    if u < 0 || u > 3 then refuse(s"column '$name': time unit $u") else TimeUnit.fromOrdinal(u)

  /** ints of `width` bytes, sign- or zero-extended to Long */
  private def widthInts(bytes: Array[Byte], at: Int, n: Int, width: Int, signed: Boolean): Array[Long] =
    val out = new Array[Long](n)
    if width == 8 then le(bytes, at, 8 * n).asLongBuffer().get(out)
    else
      var i = 0
      while i < n do
        var v = 0L
        var k = 0
        while k < width do { v |= (bytes(at + width * i + k) & 0xffL) << (8 * k); k += 1 }
        out(i) = if signed then (v << (64 - 8 * width)) >> (64 - 8 * width) else v
        i += 1
    out

  /** 16 bytes of two's complement, little-endian */
  private def decimal(bytes: Array[Byte], at: Int): BigInt =
    val be = Array.tabulate(16)(k => bytes(at + 15 - k))
    BigInt(be)

  /** IEEE 754 half precision to float, by hand: `Float.float16ToFloat` is JDK 20+ and JVM-only */
  private def halfToFloat(h: Int): Float =
    val sign = if (h & 0x8000) != 0 then -1f else 1f
    val exp = (h >> 10) & 0x1f
    val frac = h & 0x3ff
    if exp == 0 then sign * frac * math.pow(2, -24).toFloat
    else if exp == 31 then (if frac == 0 then sign * Float.PositiveInfinity else Float.NaN)
    else sign * (1f + frac / 1024f) * math.pow(2, exp - 15).toFloat

  private def unpack(bytes: Array[Byte], at: Int, len: Int, n: Int, name: String): Array[Boolean] =
    if len < (n + 7) / 8 then refuse(s"column '$name': a bitmap of $len bytes for $n rows (cut short?)")
    Array.tabulate(n)(i => (bytes(at + (i >> 3)) >> (i & 7) & 1) == 1)

  private def concat(parts: Vector[Column], name: String): Column =
    try Column.concat(parts)
    catch case e: IllegalArgumentException => refuse(s"column '$name': ${e.getMessage}")

  /** a field's column when the stream has no batch for it */
  private def empty(f: Field): Column =
    val none = Array.emptyBooleanArray
    f.typeId match
      case _ if f.dictionary.isDefined => Column.Utf8(Array.empty[String], none)
      case TypeNull => Column.Nulls(0)
      case TypeInt =>
        if f.tpe.i32(0, 0) == 64 && f.tpe.bool(1, false) then Column.Int64(Array.emptyLongArray, none)
        else Column.Ints(f.tpe.i32(0, 0), f.tpe.bool(1, false), Array.emptyLongArray, none)
      case TypeFloat => if f.tpe.i16(0, 0) == PrecisionDouble then Column.Float64(Array.emptyDoubleArray, none) else Column.Float32(Array.emptyFloatArray, none)
      case TypeBool => Column.Bool(none, none)
      case TypeUtf8 | TypeLargeUtf8 => Column.Utf8(Array.empty[String], none)
      case TypeBinary | TypeLargeBinary => Column.Binary(Array.empty[Array[Byte]], none)
      case TypeFixedBinary => Column.FixedBinary(f.tpe.i32(0, 0), Array.empty[Array[Byte]], none)
      case TypeDecimal => Column.Decimal(f.tpe.i32(0, 0), f.tpe.i32(1, 0), Array.empty[BigInt], none)
      case TypeDate => if f.tpe.i16(0, 1) == 0 then Column.Date32(Array.emptyIntArray, none) else Column.Date64(Array.emptyLongArray, none)
      case TypeTimestamp => Column.Timestamp(timeUnit(f.tpe.i16(0, 0), f.name), f.tpe.str(1), Array.emptyLongArray, none)
      case TypeDuration => Column.Duration(timeUnit(f.tpe.i16(0, 1), f.name), Array.emptyLongArray, none)
      case TypeList | TypeLargeList => Column.ListOf(Array(0), f.children.headOption.fold(Column.Nulls(0))(empty), none)
      case TypeStruct => Column.Struct(f.children.map(c => c.name -> empty(c)), none)
      case other => refuse(s"column '${f.name}' has Arrow type ${typeName(other)}; the model does not hold it")

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
  private val TypeBinary = 4
  private val TypeUtf8 = 5
  private val TypeBool = 6
  private val TypeDecimal = 7
  private val TypeDate = 8
  private val TypeTimestamp = 10
  private val TypeList = 12
  private val TypeStruct = 13
  private val TypeFixedBinary = 15
  private val TypeDuration = 18
  private val TypeLargeBinary = 19
  private val TypeLargeUtf8 = 20
  private val TypeLargeList = 21
  private val PrecisionSingle = 1
  private val PrecisionDouble = 2

  private object Utf8:
    def length(s: String): Int =
      var n = 0
      var i = 0
      while i < s.length do
        val c = s.charAt(i)
        if c < 0x80 then n += 1
        else if c < 0x800 then n += 2
        else if Character.isHighSurrogate(c) && i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1)) then
          n += 4; i += 1
        else if Character.isSurrogate(c) then n += 1
        else n += 3
        i += 1
      n
    def encode(s: String, out: Array[Byte], start: Int): Unit =
      var at = start
      var i = 0
      while i < s.length do
        val c = s.charAt(i)
        if c < 0x80 then { out(at) = c.toByte; at += 1 }
        else if c < 0x800 then
          out(at) = (0xc0 | (c >> 6)).toByte; out(at + 1) = (0x80 | (c & 0x3f)).toByte; at += 2
        else if Character.isHighSurrogate(c) && i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1)) then
          val cp = Character.toCodePoint(c, s.charAt(i + 1))
          out(at) = (0xf0 | (cp >> 18)).toByte; out(at + 1) = (0x80 | ((cp >> 12) & 0x3f)).toByte
          out(at + 2) = (0x80 | ((cp >> 6) & 0x3f)).toByte; out(at + 3) = (0x80 | (cp & 0x3f)).toByte
          at += 4; i += 1
        else if Character.isSurrogate(c) then { out(at) = '?'.toByte; at += 1 }
        else
          out(at) = (0xe0 | (c >> 12)).toByte; out(at + 1) = (0x80 | ((c >> 6) & 0x3f)).toByte
          out(at + 2) = (0x80 | (c & 0x3f)).toByte; at += 3
        i += 1


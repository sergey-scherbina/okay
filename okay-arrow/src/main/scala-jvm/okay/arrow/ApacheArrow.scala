package okay.arrow

import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import org.apache.arrow.memory.{ArrowBuf, BufferAllocator, RootAllocator}
import org.apache.arrow.vector.*
import org.apache.arrow.vector.complex.{LargeListVector, ListVector, StructVector}
import org.apache.arrow.vector.dictionary.DictionaryEncoder
import org.apache.arrow.vector.ipc.{ArrowStreamReader, ArrowStreamWriter}
import org.apache.arrow.vector.types.{DateUnit, FloatingPointPrecision, TimeUnit as JUnit}
import org.apache.arrow.vector.types.pojo.{ArrowType, Field, FieldType, Schema}

/**
 * The facade over the real thing: Apache Arrow Java (specs/okay-arrow.md).
 *
 * {{{
 * import okay.arrow.ApacheArrow.given
 * }}}
 *
 * Arrow Java is an OPTIONAL dependency of okay-arrow: a program that uses
 * this adds `org.apache.arrow:arrow-vector` and `arrow-memory-unsafe` (or
 * `arrow-memory-netty`) itself, and runs with the flags Arrow's memory
 * needs (`--add-opens=java.base/java.nio=ALL-UNNAMED`, and on JDK 24+
 * `--sun-misc-unsafe-memory-access=allow`). Without Arrow on the
 * classpath the first use is refused by name ([[ApacheArrow.missing]]).
 *
 * Every column of the model maps to Arrow Java's own vector for it, both
 * ways, through Arrow Java's API — never through okay's IPC — so each
 * implementation checks the other (TestApacheArrow). `toRoot` and
 * `fromRoot` move a table between the model and a `VectorSchemaRoot`, for
 * code that already lives in Arrow.
 */
object ApacheArrow extends ArrowCodec:

  def name = "apache"

  given codec: ArrowCodec = this

  /** why this cannot run here, or None: a class of Arrow Java's missing
   * from the classpath (the dependency is optional, so this is the
   * ordinary case for a program that did not add it) */
  def missing(className: String = "org.apache.arrow.vector.VectorSchemaRoot"): Option[String] =
    try { Class.forName(className, false, getClass.getClassLoader); None }
    catch case _: ClassNotFoundException => Some(
      s"okay.arrow.ApacheArrow needs Apache Arrow Java, an optional dependency of okay-arrow ($className is not on the classpath): " +
        "add org.apache.arrow:arrow-vector and arrow-memory-unsafe (or arrow-memory-netty), 19.0.0, and run with " +
        "--add-opens=java.base/java.nio=ALL-UNNAMED (and --sun-misc-unsafe-memory-access=allow on JDK 24+) — " +
        "or use okay.arrow.OkayArrow, the default, which needs none of it")

  private lazy val ready: Unit = missing().foreach(why => throw IllegalStateException(why))

  def write(t: Table): Array[Byte] =
    ready
    val alloc = RootAllocator()
    try
      val root = toRoot(t, alloc)
      try
        val out = java.io.ByteArrayOutputStream()
        val w = ArrowStreamWriter(root, null, java.nio.channels.Channels.newChannel(out))
        try { w.start(); w.writeBatch(); w.end() } finally w.close()
        out.toByteArray
      finally root.close()
    finally alloc.close()

  def read(bytes: Array[Byte]): Table =
    ready
    val alloc = RootAllocator()
    try
      val r = ArrowStreamReader(java.io.ByteArrayInputStream(bytes), alloc)
      try
        val root = r.getVectorSchemaRoot
        val schema = root.getSchema
        val dicts = r.getDictionaryVectors
        var parts = Vector.empty[Vector[Column]]
        while r.loadNextBatch() do
          parts :+= root.getFieldVectors.asScala.toVector.map { v =>
            Option(v.getField.getDictionary) match
              case Some(enc) =>
                val decoded = DictionaryEncoder.decode(v, dicts.get(java.lang.Long.valueOf(enc.getId)))
                try column(decoded) finally decoded.close()
              case None => column(v)
          }
        val names = schema.getFields.asScala.toVector.map(_.getName)
        val cols =
          if parts.isEmpty then root.getFieldVectors.asScala.toVector.map(v => column(v))   // no rows
          else names.indices.toVector.map(j => Column.concat(parts.map(_(j))))
        Table(names.zip(cols), metadata(schema))
      catch case e: java.io.IOException => throw IllegalStateException(s"not an Arrow stream Arrow Java reads: ${e.getMessage}", e)
      finally r.close()
    finally alloc.close()

  private def metadata(s: Schema): Vector[(String, String)] =
    Option(s.getCustomMetadata).fold(Vector.empty[(String, String)])(_.asScala.toVector)

  // ---- the model into Arrow Java ---------------------------------------------

  /** the model as Arrow Java's columns, in `alloc` (the caller closes the root) */
  def toRoot(t: Table, alloc: BufferAllocator): VectorSchemaRoot =
    ready
    val n = t.rows
    t.cols.find(_._2.length != n).foreach { (name, c) =>
      throw IllegalArgumentException(s"column '$name' has ${c.length} rows, the first has $n")
    }
    val vectors: Vector[FieldVector] = t.cols.map { (name, c) =>
      val v = field(name, c).createVector(alloc)
      fill(v, c)
      v
    }
    val schema = Schema(vectors.map(_.getField).asJava, t.metadata.toMap.asJava)
    VectorSchemaRoot(schema, vectors.asJava, n)

  private def unitOf(u: TimeUnit): JUnit = u match
    case TimeUnit.Second => JUnit.SECOND
    case TimeUnit.Milli => JUnit.MILLISECOND
    case TimeUnit.Micro => JUnit.MICROSECOND
    case TimeUnit.Nano => JUnit.NANOSECOND

  private def unitOf(u: JUnit): TimeUnit = u match
    case JUnit.SECOND => TimeUnit.Second
    case JUnit.MILLISECOND => TimeUnit.Milli
    case JUnit.MICROSECOND => TimeUnit.Micro
    case JUnit.NANOSECOND => TimeUnit.Nano

  /** the column's Arrow field, children included */
  private def field(name: String, c: Column): Field =
    def leaf(t: ArrowType) = Field(name, FieldType.nullable(t), null)
    c match
      case Column.Int64(_, _) => leaf(ArrowType.Int(64, true))
      case Column.Ints(b, s, _, _) => leaf(ArrowType.Int(b, s))
      case Column.Float64(_, _) => leaf(ArrowType.FloatingPoint(FloatingPointPrecision.DOUBLE))
      case Column.Float32(_, _) => leaf(ArrowType.FloatingPoint(FloatingPointPrecision.SINGLE))
      case Column.Utf8(_, _) => leaf(ArrowType.Utf8.INSTANCE)
      case Column.Binary(_, _) => leaf(ArrowType.Binary.INSTANCE)
      case Column.Bool(_, _) => leaf(ArrowType.Bool.INSTANCE)
      case Column.Nulls(_) => leaf(ArrowType.Null.INSTANCE)
      case Column.FixedBinary(w, _, _) => leaf(ArrowType.FixedSizeBinary(w))
      case Column.Decimal(p, s, _, _) => leaf(ArrowType.Decimal(p, s, 128))
      case Column.Date32(_, _) => leaf(ArrowType.Date(DateUnit.DAY))
      case Column.Date64(_, _) => leaf(ArrowType.Date(DateUnit.MILLISECOND))
      case Column.Timestamp(u, z, _, _) => leaf(ArrowType.Timestamp(unitOf(u), z.orNull))
      case Column.Duration(u, _, _) => leaf(ArrowType.Duration(unitOf(u)))
      case Column.ListOf(_, child, _) =>
        Field(name, FieldType.nullable(ArrowType.List.INSTANCE), java.util.List.of(field("item", child)))
      case Column.Struct(fs, _) =>
        Field(name, FieldType.nullable(ArrowType.Struct.INSTANCE), fs.map((n, f) => field(n, f)).asJava)

  /** a fixed-width vector written through its data buffer, `width` bytes a row */
  private def fixed(v: BaseFixedWidthVector, ok: Array[Boolean])(put: (ArrowBuf, Int) => Unit): Unit =
    val n = ok.length
    v.allocateNew(n)
    var i = 0
    while i < n do
      if ok(i) then { put(v.getDataBuffer, i); BitVectorHelper.setBit(v.getValidityBuffer, i.toLong) }
      i += 1
    v.setValueCount(n)

  private def fill(v: FieldVector, c: Column): Unit = (v, c) match
    case (x: BaseFixedWidthVector, Column.Int64(d, ok)) => fixed(x, ok)((b, i) => b.setLong(8L * i, d(i)))
    case (x: BaseFixedWidthVector, Column.Ints(bits, _, d, ok)) => fixed(x, ok) { (b, i) =>
      bits match
        case 8 => b.setByte(i.toLong, d(i).toInt)
        case 16 => b.setShort(2L * i, d(i).toShort)
        case 32 => b.setInt(4L * i, d(i).toInt)
        case _ => b.setLong(8L * i, d(i))
    }
    case (x: BaseFixedWidthVector, Column.Float64(d, ok)) => fixed(x, ok)((b, i) => b.setDouble(8L * i, d(i)))
    case (x: BaseFixedWidthVector, Column.Float32(d, ok)) => fixed(x, ok)((b, i) => b.setFloat(4L * i, d(i)))
    case (x: BaseFixedWidthVector, Column.Date32(d, ok)) => fixed(x, ok)((b, i) => b.setInt(4L * i, d(i)))
    case (x: BaseFixedWidthVector, Column.Date64(d, ok)) => fixed(x, ok)((b, i) => b.setLong(8L * i, d(i)))
    case (x: BaseFixedWidthVector, Column.Timestamp(_, _, d, ok)) => fixed(x, ok)((b, i) => b.setLong(8L * i, d(i)))
    case (x: BaseFixedWidthVector, Column.Duration(_, d, ok)) => fixed(x, ok)((b, i) => b.setLong(8L * i, d(i)))
    case (x: FixedSizeBinaryVector, Column.FixedBinary(w, d, ok)) => fixed(x, ok)((b, i) => b.setBytes(w.toLong * i, d(i)))
    case (x: DecimalVector, Column.Decimal(_, _, d, ok)) => fixed(x, ok) { (b, i) =>
      val be = d(i).toByteArray
      if be.length > 16 then throw IllegalArgumentException(s"${d(i)} does not fit decimal128")
      val fillByte: Byte = if d(i).signum < 0 then -1 else 0
      b.setBytes(16L * i, Array.tabulate(16)(k => if k < be.length then be(be.length - 1 - k) else fillByte))
    }
    case (x: BitVector, Column.Bool(d, ok)) =>
      x.allocateNew(ok.length)
      var i = 0
      while i < ok.length do { if ok(i) then x.set(i, if d(i) then 1 else 0) else x.setNull(i); i += 1 }
      x.setValueCount(ok.length)
    case (x: VarCharVector, Column.Utf8(d, ok)) =>
      x.allocateNew(ok.length)
      var i = 0
      while i < ok.length do { if ok(i) && d(i) != null then x.setSafe(i, d(i).getBytes(UTF_8)) else x.setNull(i); i += 1 }
      x.setValueCount(ok.length)
    case (x: VarBinaryVector, Column.Binary(d, ok)) =>
      x.allocateNew(ok.length)
      var i = 0
      while i < ok.length do { if ok(i) && d(i) != null then x.setSafe(i, d(i)) else x.setNull(i); i += 1 }
      x.setValueCount(ok.length)
    case (x: NullVector, Column.Nulls(n)) => x.setValueCount(n)
    case (x: ListVector, Column.ListOf(offs, child, ok)) =>
      val n = ok.length
      x.setInitialCapacity(n)
      x.allocateNew()
      fill(x.getDataVector, child)
      var i = 0
      while i < n do
        val start = x.startNewValue(i)
        x.endValue(i, offs(i + 1) - offs(i))
        if start != offs(i) then throw IllegalStateException(s"list offsets diverged at row $i")
        if !ok(i) then x.setNull(i)
        i += 1
      x.setLastSet(n - 1)
      x.setValueCount(n)
    case (x: StructVector, Column.Struct(fs, ok)) =>
      val n = ok.length
      x.setInitialCapacity(n)
      x.allocateNew()
      fs.foreach((name, f) => fill(x.getChild(name), f))
      var i = 0
      while i < n do { if ok(i) then x.setIndexDefined(i) else x.setNull(i); i += 1 }
      x.setValueCount(n)
    case (x, other) =>
      throw IllegalStateException(s"no Arrow Java vector for ${other.getClass.getSimpleName} (made ${x.getClass.getSimpleName})")

  // ---- Arrow Java into the model -------------------------------------------

  /** Arrow Java's columns as the model; a column outside it is refused by name */
  def fromRoot(root: VectorSchemaRoot): Table =
    ready
    Table(root.getFieldVectors.asScala.toVector.map(v => v.getName -> column(v)), metadata(root.getSchema))

  private def column(v: ValueVector): Column =
    val n = v.getValueCount
    def ok = Array.tabulate(n)(i => !v.isNull(i))
    def longs(width: Int, signed: Boolean, b: ArrowBuf) = Array.tabulate(n) { i =>
      width match
        case 1 => if signed then b.getByte(i.toLong).toLong else b.getByte(i.toLong) & 0xffL
        case 2 => if signed then b.getShort(2L * i).toLong else b.getShort(2L * i) & 0xffffL
        case 4 => if signed then b.getInt(4L * i).toLong else b.getInt(4L * i) & 0xffffffffL
        case _ => b.getLong(8L * i)
    }
    v match
      case x: BigIntVector => Column.Int64(longs(8, true, x.getDataBuffer), ok)
      case x: TinyIntVector => Column.Ints(8, true, longs(1, true, x.getDataBuffer), ok)
      case x: SmallIntVector => Column.Ints(16, true, longs(2, true, x.getDataBuffer), ok)
      case x: IntVector => Column.Ints(32, true, longs(4, true, x.getDataBuffer), ok)
      case x: UInt1Vector => Column.Ints(8, false, longs(1, false, x.getDataBuffer), ok)
      case x: UInt2Vector => Column.Ints(16, false, longs(2, false, x.getDataBuffer), ok)
      case x: UInt4Vector => Column.Ints(32, false, longs(4, false, x.getDataBuffer), ok)
      case x: UInt8Vector => Column.Ints(64, false, longs(8, false, x.getDataBuffer), ok)
      case x: Float8Vector => Column.Float64(Array.tabulate(n)(i => x.getDataBuffer.getDouble(8L * i)), ok)
      case x: Float4Vector => Column.Float32(Array.tabulate(n)(i => x.getDataBuffer.getFloat(4L * i)), ok)
      case x: Float2Vector => Column.Float32(Array.tabulate(n)(i => if x.isNull(i) then 0f else x.getValueAsFloat(i)), ok)
      case x: BitVector => Column.Bool(Array.tabulate(n)(i => !x.isNull(i) && x.get(i) == 1), ok)
      case x: VarCharVector => Column.Utf8(Array.tabulate(n)(i => if x.isNull(i) then "" else String(x.get(i), UTF_8)), ok)
      case x: LargeVarCharVector => Column.Utf8(Array.tabulate(n)(i => if x.isNull(i) then "" else String(x.get(i), UTF_8)), ok)
      case x: VarBinaryVector => Column.Binary(Array.tabulate(n)(i => if x.isNull(i) then Array.emptyByteArray else x.get(i)), ok)
      case x: LargeVarBinaryVector => Column.Binary(Array.tabulate(n)(i => if x.isNull(i) then Array.emptyByteArray else x.get(i)), ok)
      case x: FixedSizeBinaryVector =>
        val w = x.getByteWidth
        Column.FixedBinary(w, Array.tabulate(n)(i => if x.isNull(i) then new Array[Byte](w) else x.get(i)), ok)
      case x: DecimalVector =>
        Column.Decimal(x.getPrecision, x.getScale,
          Array.tabulate(n)(i => if x.isNull(i) then BigInt(0) else BigInt(x.getObject(i).unscaledValue)), ok)
      case x: DateDayVector => Column.Date32(Array.tabulate(n)(i => x.getDataBuffer.getInt(4L * i)), ok)
      case x: DateMilliVector => Column.Date64(longs(8, true, x.getDataBuffer), ok)
      case x: TimeStampVector =>
        x.getField.getType match
          case t: ArrowType.Timestamp => Column.Timestamp(unitOf(t.getUnit), Option(t.getTimezone), longs(8, true, x.getDataBuffer), ok)
          case other => refuse(v, s"a timestamp vector typed $other")
      case x: DurationVector =>
        x.getField.getType match
          case t: ArrowType.Duration => Column.Duration(unitOf(t.getUnit), longs(8, true, x.getDataBuffer), ok)
          case other => refuse(v, s"a duration vector typed $other")
      case x: ListVector =>
        val offs = Array.tabulate(n + 1)(i => x.getOffsetBuffer.getInt(4L * i))
        normalised(offs, column(x.getDataVector), ok)
      case x: LargeListVector =>
        val offs = Array.tabulate(n + 1)(i => x.getOffsetBuffer.getLong(8L * i).toInt)
        normalised(offs, column(x.getDataVector), ok)
      case x: StructVector =>
        Column.Struct(x.getChildrenFromFields.asScala.toVector.map(c => c.getName -> column(c)), ok)
      case _: NullVector => Column.Nulls(n)
      case other => refuse(other, other.getField.getType.toString)

  /** a list whose offsets start at 0 and end at its child's length */
  private def normalised(offs: Array[Int], child: Column, ok: Array[Boolean]): Column =
    if offs.isEmpty then Column.ListOf(Array(0), child, ok)
    else if offs(0) == 0 && offs.last == child.length then Column.ListOf(offs, child, ok)
    else
      val from = offs(0)
      val used = Array.tabulate(offs.last - from)(_ + from)
      Column.ListOf(offs.map(_ - from), child.take(used, Array.fill(used.length)(true)), ok)

  private def refuse(v: ValueVector, what: String): Nothing =
    throw IllegalStateException(s"column '${v.getName}' is Arrow $what; the model does not hold it")

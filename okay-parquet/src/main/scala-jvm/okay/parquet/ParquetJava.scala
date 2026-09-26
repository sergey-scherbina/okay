package okay.parquet

import okay.arrow.{Column, Table, TimeUnit}
import okay.compress.Compression
import org.apache.parquet.io.api.Binary
import org.apache.parquet.schema.{LogicalTypeAnnotation as LT, MessageType, PrimitiveType, Types}
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName as P
import scala.jdk.CollectionConverters.*

/**
 * PARQUET-JAVA BEHIND THE FACADE (specs/parquet.md, specs/own-or-standard.md):
 * `import okay.parquet.ParquetJava.given`, over an OPTIONAL dependency
 * (org.apache.parquet:parquet-hadoop with Hadoop's client API). Files and
 * streams only through parquet-java's own `InputFile`/`OutputFile` and a
 * `PlainParquetConfiguration`, so no Hadoop file system is touched. Without
 * the jars the first use is refused by name ([[ParquetJava.missing]]).
 *
 * The one difference that is the library's to make: its writer chooses its
 * own row groups (by size, 128 MB by default), so `append` writes rows and
 * does not force a group boundary.
 */
object ParquetJava extends ParquetCodec:
  given parquetJava: ParquetCodec = this

  def name = "parquet-java"

  /** why this cannot run here, or None */
  def missing(className: String = "org.apache.parquet.hadoop.ParquetFileReader"): Option[String] =
    try { Class.forName(className, false, getClass.getClassLoader); None }
    catch case _: ClassNotFoundException => Some(
      s"okay.parquet.ParquetJava needs org.apache.parquet:parquet-hadoop, an optional dependency of okay-parquet " +
        s"($className is not on the classpath): add org.apache.parquet:parquet-hadoop:1.16.0 and " +
        "org.apache.hadoop:hadoop-client-api:3.4.1 — or use okay.parquet.OkayParquet, the default, which needs nothing")

  private lazy val ready: Unit = missing().foreach(why => throw IllegalStateException(why))

  private def input(in: ReadAt): org.apache.parquet.io.InputFile = new org.apache.parquet.io.InputFile:
    def getLength: Long = in.size
    def newStream(): org.apache.parquet.io.SeekableInputStream = new org.apache.parquet.io.SeekableInputStream:
      private var pos = 0L
      def getPos: Long = pos
      def seek(p: Long): Unit = pos = p
      override def read(): Int =
        if pos >= in.size then -1 else { val b = in.read(pos, 1)(0) & 0xff; pos += 1; b }
      override def read(b: Array[Byte], off: Int, len: Int): Int =
        if pos >= in.size then -1
        else
          val n = math.min(len.toLong, in.size - pos).toInt
          System.arraycopy(in.read(pos, n), 0, b, off, n)
          pos += n
          n
      def readFully(b: Array[Byte]): Unit = readFully(b, 0, b.length)
      def readFully(b: Array[Byte], off: Int, len: Int): Unit =
        if pos + len > in.size then throw java.io.EOFException(s"a read of $len bytes at $pos past ${in.size}")
        System.arraycopy(in.read(pos, len), 0, b, off, len)
        pos += len
      def read(buf: java.nio.ByteBuffer): Int =
        val n = math.min(buf.remaining().toLong, in.size - pos).toInt
        if n <= 0 then -1 else { buf.put(in.read(pos, n)); pos += n; n }
      def readFully(buf: java.nio.ByteBuffer): Unit =
        val n = buf.remaining()
        if pos + n > in.size then throw java.io.EOFException(s"a read of $n bytes at $pos past ${in.size}")
        buf.put(in.read(pos, n))
        pos += n

  private def open(in: ReadAt) =
    ready
    org.apache.parquet.hadoop.ParquetFileReader.open(input(in),
      org.apache.parquet.ParquetReadOptions.builder(org.apache.parquet.conf.PlainParquetConfiguration()).build())

  def footer(in: ReadAt): Footer =
    val r = open(in)
    try
      val meta = r.getFooter
      val schema = meta.getFileMetaData.getSchema
      Footer(leaves(schema).map(p => p.getName -> Column.describe(empty(p, 0))),
        r.getRowGroups.asScala.toVector.map(_.getRowCount),
        meta.getFileMetaData.getKeyValueMetaData.asScala.toVector.sortBy(_._1),
        Option(meta.getFileMetaData.getCreatedBy))()
    finally r.close()

  private def leaves(schema: MessageType): Vector[PrimitiveType] =
    schema.getFields.asScala.toVector.map { f =>
      if !f.isPrimitive then throw Refused(s"column '${f.getName}' is a group: nested Parquet schemas are not read yet (specs/parquet.md)")
      if f.isRepetition(org.apache.parquet.schema.Type.Repetition.REPEATED) then
        throw Refused(s"column '${f.getName}' is REPEATED: nested Parquet schemas are not read yet (specs/parquet.md)")
      f.asPrimitiveType()
    }

  /** a column of `n` nulls of this leaf's kind, and the kind's cell reader */
  private def empty(p: PrimitiveType, n: Int): Column =
    val no = new Array[Boolean](n)
    val lt = p.getLogicalTypeAnnotation
    p.getPrimitiveTypeName match
      case P.BOOLEAN => Column.Bool(new Array[Boolean](n), no)
      case P.INT32 => lt match
        case d: LT.DecimalLogicalTypeAnnotation => Column.Decimal(d.getPrecision, d.getScale, Array.fill(n)(BigInt(0)), no)
        case _: LT.DateLogicalTypeAnnotation => Column.Date32(new Array[Int](n), no)
        case i: LT.IntLogicalTypeAnnotation => Column.Ints(i.getBitWidth, i.isSigned, new Array[Long](n), no)
        case _ => Column.Ints(32, true, new Array[Long](n), no)
      case P.INT64 => lt match
        case t: LT.TimestampLogicalTypeAnnotation =>
          Column.Timestamp(unit(t.getUnit), if t.isAdjustedToUTC then Some("UTC") else None, new Array[Long](n), no)
        case d: LT.DecimalLogicalTypeAnnotation => Column.Decimal(d.getPrecision, d.getScale, Array.fill(n)(BigInt(0)), no)
        case i: LT.IntLogicalTypeAnnotation if !i.isSigned => Column.Ints(64, false, new Array[Long](n), no)
        case _ => Column.Int64(new Array[Long](n), no)
      case P.INT96 => Column.Timestamp(TimeUnit.Nano, Some("UTC"), new Array[Long](n), no)
      case P.FLOAT => Column.Float32(new Array[Float](n), no)
      case P.DOUBLE => Column.Float64(new Array[Double](n), no)
      case P.BINARY => lt match
        case _: LT.StringLogicalTypeAnnotation | _: LT.EnumLogicalTypeAnnotation | _: LT.JsonLogicalTypeAnnotation =>
          Column.Utf8(Array.fill(n)(""), no)
        case d: LT.DecimalLogicalTypeAnnotation => Column.Decimal(d.getPrecision, d.getScale, Array.fill(n)(BigInt(0)), no)
        case _ => Column.Binary(Array.fill(n)(Array.emptyByteArray), no)
      case P.FIXED_LEN_BYTE_ARRAY => lt match
        case d: LT.DecimalLogicalTypeAnnotation => Column.Decimal(d.getPrecision, d.getScale, Array.fill(n)(BigInt(0)), no)
        case _ => Column.FixedBinary(p.getTypeLength, Array.fill(n)(new Array[Byte](p.getTypeLength)), no)

  private def unit(u: LT.TimeUnit): TimeUnit = u match
    case LT.TimeUnit.MILLIS => TimeUnit.Milli
    case LT.TimeUnit.MICROS => TimeUnit.Micro
    case LT.TimeUnit.NANOS => TimeUnit.Nano

  def group(in: ReadAt, footer: Footer, g: Int, columns: Option[Set[String]] = None)
           (using Compression): Table =
    val r = open(in)
    try
      val meta = r.getFooter.getFileMetaData
      val schema = meta.getSchema
      val ps = leaves(schema)
      val pages = r.readRowGroup(g)
      val rows = pages.getRowCount.toInt
      val reader = org.apache.parquet.io.ColumnIOFactory().getColumnIO(schema)
        .getRecordReader(pages, org.apache.parquet.example.data.simple.convert.GroupRecordConverter(schema))
      val records = Array.fill(rows)(reader.read())
      val cols = ps.zipWithIndex.filter((p, _) => columns.forall(_.contains(p.getName))).map { (p, j) =>
        val ok = Array.tabulate(rows)(i => records(i).getFieldRepetitionCount(j) > 0)
        def each[A: scala.reflect.ClassTag](zero: A)(f: Int => A): Array[A] =
          Array.tabulate(rows)(i => if ok(i) then f(i) else zero)
        val c = empty(p, 0) match
          case _: Column.Bool => Column.Bool(each(false)(i => records(i).getBoolean(j, 0)), ok)
          case Column.Ints(b, s, _, _) =>
            Column.Ints(b, s, each(0L)(i =>
              if p.getPrimitiveTypeName == P.INT64 then records(i).getLong(j, 0)
              else if s then records(i).getInteger(j, 0).toLong else records(i).getInteger(j, 0) & 0xffffffffL), ok)
          case _: Column.Date32 => Column.Date32(each(0)(i => records(i).getInteger(j, 0)), ok)
          case _: Column.Int64 => Column.Int64(each(0L)(i => records(i).getLong(j, 0)), ok)
          case Column.Timestamp(u, z, _, _) if p.getPrimitiveTypeName == P.INT96 =>
            Column.Timestamp(u, z, each(0L) { i =>
              val b = java.nio.ByteBuffer.wrap(records(i).getInt96(j, 0).getBytes).order(java.nio.ByteOrder.LITTLE_ENDIAN)
              val nanos = b.getLong
              (b.getInt - 2440588L) * 86400000000000L + nanos
            }, ok)
          case Column.Timestamp(u, z, _, _) => Column.Timestamp(u, z, each(0L)(i => records(i).getLong(j, 0)), ok)
          case _: Column.Float32 => Column.Float32(each(0f)(i => records(i).getFloat(j, 0)), ok)
          case _: Column.Float64 => Column.Float64(each(0d)(i => records(i).getDouble(j, 0)), ok)
          case _: Column.Utf8 => Column.Utf8(each("")(i => records(i).getString(j, 0)), ok)
          case _: Column.Binary => Column.Binary(each(Array.emptyByteArray)(i => records(i).getBinary(j, 0).getBytes), ok)
          case Column.FixedBinary(w, _, _) =>
            Column.FixedBinary(w, each(new Array[Byte](w))(i => records(i).getBinary(j, 0).getBytes), ok)
          case Column.Decimal(pr, sc, _, _) =>
            Column.Decimal(pr, sc, each(BigInt(0))(i => p.getPrimitiveTypeName match
              case P.INT32 => BigInt(records(i).getInteger(j, 0))
              case P.INT64 => BigInt(records(i).getLong(j, 0))
              case _ => BigInt(records(i).getBinary(j, 0).getBytes)), ok)
          case other => throw Refused(s"column '${p.getName}' is ${Column.describe(other)}: not read")
        p.getName -> c
      }
      Table(cols, meta.getKeyValueMetaData.asScala.toVector.sortBy(_._1))
    finally r.close()

  def writer(out: Array[Byte] => Unit, compress: Compress = Compress.Snappy,
             metadata: Vector[(String, String)] = Vector.empty)
            (using Compression): ParquetWriter = new ParquetWriter:
    ready
    private var w: org.apache.parquet.hadoop.ParquetWriter[org.apache.parquet.example.data.Group] | Null = null
    private var schema: MessageType | Null = null

    private val file = new org.apache.parquet.io.OutputFile:
      private def stream(): org.apache.parquet.io.PositionOutputStream = new org.apache.parquet.io.PositionOutputStream:
        private var pos = 0L
        def getPos: Long = pos
        def write(b: Int): Unit = { out(Array(b.toByte)); pos += 1 }
        override def write(b: Array[Byte], off: Int, len: Int): Unit =
          out(java.util.Arrays.copyOfRange(b, off, off + len)); pos += len
      def create(blockSizeHint: Long): org.apache.parquet.io.PositionOutputStream = stream()
      def createOrOverwrite(blockSizeHint: Long): org.apache.parquet.io.PositionOutputStream = stream()
      def supportsBlockSize: Boolean = false
      def defaultBlockSize: Long = 0L

    def append(t: Table): Unit =
      if w == null then
        schema = messageOf(t)
        w = org.apache.parquet.hadoop.example.ExampleParquetWriter.builder(file)
          .withConf(org.apache.parquet.conf.PlainParquetConfiguration())
          .withType(schema.nn)
          .withExtraMetaData(metadata.toMap.asJava)
          .withCompressionCodec(compress match
            case Compress.None => org.apache.parquet.hadoop.metadata.CompressionCodecName.UNCOMPRESSED
            case Compress.Snappy => org.apache.parquet.hadoop.metadata.CompressionCodecName.SNAPPY
            case Compress.Zstd => org.apache.parquet.hadoop.metadata.CompressionCodecName.ZSTD)
          .build()
      val s = schema.nn
      val factory = org.apache.parquet.example.data.simple.SimpleGroupFactory(s)
      for i <- 0 until t.rows do
        val g = factory.newGroup()
        for ((name, c), j) <- t.cols.zipWithIndex if c.validity(i) do c match
          case Column.Bool(v, _) => g.add(j, v(i))
          case Column.Int64(v, _) => g.add(j, v(i))
          case Column.Ints(b, _, v, _) => if b <= 32 then g.add(j, v(i).toInt) else g.add(j, v(i))
          case Column.Float32(v, _) => g.add(j, v(i))
          case Column.Float64(v, _) => g.add(j, v(i))
          case Column.Utf8(v, _) => g.add(j, v(i))
          case Column.Binary(v, _) => g.add(j, Binary.fromConstantByteArray(v(i)))
          case Column.FixedBinary(_, v, _) => g.add(j, Binary.fromConstantByteArray(v(i)))
          case Column.Date32(v, _) => g.add(j, v(i))
          case Column.Timestamp(_, _, v, _) => g.add(j, v(i))
          case other => throw Refused(s"column '$name' is ${Column.describe(other)}: not written")
        w.nn.write(g)

    def close(): Unit = if w != null then w.nn.close()

  private def messageOf(t: Table): MessageType =
    val b = Types.buildMessage()
    for (name, c) <- t.cols do
      val field = c match
        case _: Column.Bool => Types.optional(P.BOOLEAN)
        case _: Column.Int64 => Types.optional(P.INT64)
        case Column.Ints(bits, s, _, _) =>
          Types.optional(if bits <= 32 then P.INT32 else P.INT64).as(LT.intType(bits, s))
        case _: Column.Float32 => Types.optional(P.FLOAT)
        case _: Column.Float64 => Types.optional(P.DOUBLE)
        case _: Column.Utf8 => Types.optional(P.BINARY).as(LT.stringType())
        case _: Column.Binary => Types.optional(P.BINARY)
        case Column.FixedBinary(w, _, _) => Types.optional(P.FIXED_LEN_BYTE_ARRAY).length(w)
        case _: Column.Date32 => Types.optional(P.INT32).as(LT.dateType())
        case Column.Timestamp(u, z, _, _) =>
          val lu = u match
            case TimeUnit.Milli => LT.TimeUnit.MILLIS
            case TimeUnit.Micro => LT.TimeUnit.MICROS
            case TimeUnit.Nano => LT.TimeUnit.NANOS
            case TimeUnit.Second => throw Refused(s"column '$name' is a timestamp in seconds")
          Types.optional(P.INT64).as(LT.timestampType(z.isDefined, lu))
        case other => throw Refused(s"column '$name' is ${Column.describe(other)}: not written")
      b.addField(field.named(name)): Unit
    b.named("schema")

/** the implementations by NAME (JVM: the library one lives here) */
object Parquets:
  def byName(name: String): Either[String, ParquetCodec] = name match
    case "okay" => Right(OkayParquet)
    case "parquet-java" => Right(ParquetJava)
    case other => Left(s"unknown Parquet implementation '$other' (okay, parquet-java)")

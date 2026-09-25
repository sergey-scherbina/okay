package okay.arrow

import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import org.apache.arrow.memory.{BufferAllocator, RootAllocator}
import org.apache.arrow.vector.{BigIntVector, BitVector, FieldVector, Float8Vector, NullVector, VarCharVector, VectorSchemaRoot}
import org.apache.arrow.vector.ipc.{ArrowStreamReader, ArrowStreamWriter}
import org.apache.arrow.vector.types.pojo.Schema

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
 * Besides the facade, `toRoot` and `fromRoot` move a table between the
 * model and a `VectorSchemaRoot`, for code that already lives in Arrow.
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
        var parts = Vector.empty[Table]
        while r.loadNextBatch() do parts :+= fromRoot(root)
        val meta = Option(schema.getCustomMetadata).fold(Vector.empty[(String, String)])(_.asScala.toVector)
        val names = schema.getFields.asScala.toVector.map(_.getName)
        if parts.isEmpty then Table(names.map(n => n -> empty(root.getVector(n))), meta)
        else Table(names.indices.toVector.map(j => names(j) -> Column.concat(parts.map(_.cols(j)._2))), meta)
      catch case e: java.io.IOException => throw IllegalStateException(s"not an Arrow stream Arrow Java reads: ${e.getMessage}", e)
      finally r.close()
    finally alloc.close()

  /** the model as Arrow Java's columns, in `alloc` (the caller closes the root) */
  def toRoot(t: Table, alloc: BufferAllocator): VectorSchemaRoot =
    ready
    val n = t.rows
    t.cols.find(_._2.length != n).foreach { (name, c) =>
      throw IllegalArgumentException(s"column '$name' has ${c.length} rows, the first has $n")
    }
    val vectors: Vector[FieldVector] = t.cols.map { (name, c) =>
      c match
        case Column.Int64(v, ok) =>
          val x = BigIntVector(name, alloc); x.allocateNew(n)
          var i = 0
          while i < n do { if ok(i) then x.set(i, v(i)) else x.setNull(i); i += 1 }
          x.setValueCount(n); x
        case Column.Float64(v, ok) =>
          val x = Float8Vector(name, alloc); x.allocateNew(n)
          var i = 0
          while i < n do { if ok(i) then x.set(i, v(i)) else x.setNull(i); i += 1 }
          x.setValueCount(n); x
        case Column.Bool(v, ok) =>
          val x = BitVector(name, alloc); x.allocateNew(n)
          var i = 0
          while i < n do { if ok(i) then x.set(i, if v(i) then 1 else 0) else x.setNull(i); i += 1 }
          x.setValueCount(n); x
        case Column.Utf8(v, ok) =>
          val x = VarCharVector(name, alloc); x.allocateNew(n)
          var i = 0
          while i < n do { if ok(i) && v(i) != null then x.setSafe(i, v(i).getBytes(UTF_8)) else x.setNull(i); i += 1 }
          x.setValueCount(n); x
        case Column.Nulls(_) =>
          val x = NullVector(name); x.setValueCount(n); x
    }
    val schema = Schema(vectors.map(_.getField).asJava, t.metadata.toMap.asJava)
    VectorSchemaRoot(schema, vectors.asJava, n)

  /** Arrow Java's columns as the model; a column outside it is refused by name */
  def fromRoot(root: VectorSchemaRoot): Table =
    ready
    val n = root.getRowCount
    val meta = Option(root.getSchema.getCustomMetadata).fold(Vector.empty[(String, String)])(_.asScala.toVector)
    Table(root.getFieldVectors.asScala.toVector.map { v =>
      def valid = Array.tabulate(n)(i => !v.isNull(i))
      v.getName -> (v match
        case x: BigIntVector => Column.Int64(Array.tabulate(n)(i => if x.isNull(i) then 0L else x.get(i)), valid)
        case x: Float8Vector => Column.Float64(Array.tabulate(n)(i => if x.isNull(i) then 0.0 else x.get(i)), valid)
        case x: BitVector => Column.Bool(Array.tabulate(n)(i => !x.isNull(i) && x.get(i) == 1), valid)
        case x: VarCharVector => Column.Utf8(Array.tabulate(n)(i => if x.isNull(i) then "" else String(x.get(i), UTF_8)), valid)
        case _: NullVector => Column.Nulls(n)
        case other => refuse(other))
    }, meta)

  private def empty(v: FieldVector): Column = v match
    case _: BigIntVector => Column.Int64(Array.emptyLongArray, Array.emptyBooleanArray)
    case _: Float8Vector => Column.Float64(Array.emptyDoubleArray, Array.emptyBooleanArray)
    case _: BitVector => Column.Bool(Array.emptyBooleanArray, Array.emptyBooleanArray)
    case _: VarCharVector => Column.Utf8(Array.empty[String], Array.emptyBooleanArray)
    case _: NullVector => Column.Nulls(0)
    case other => refuse(other)

  private def refuse(v: FieldVector): Nothing =
    throw IllegalStateException(s"column '${v.getName}' is Arrow ${v.getField.getType}; the model holds int64, float64, utf8, bool and null")

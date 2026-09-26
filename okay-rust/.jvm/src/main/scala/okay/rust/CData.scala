package okay.rust

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.{MethodHandles, MethodType}
import java.nio.ByteOrder.LITTLE_ENDIAN
import java.nio.charset.StandardCharsets.UTF_8
import okay.arrow.{Column, Table}

/**
 * A table as the Arrow C Data Interface (foreign-arrow-ffm, specs/foreign-one.md
 * Decision 21): two C structs, `ArrowSchema` and `ArrowArray`, that a library
 * in this process reads in place — how a table call reaches in-process Rust
 * with no text in between. A table is a struct array whose children are its
 * columns: int64, float64, utf8, boolean and null, the columns a frame makes.
 *
 * {{{
 * given CDataCodec = OkayCData                 // the default: ours, over FFM
 * import okay.rust.ApacheCData.given           // or Apache Arrow's arrow-c-data
 * }}}
 *
 * Ours or the standard one, on choice (specs/own-or-standard.md): each reads
 * what the other wrote (TestCData).
 */
trait CDataCodec:
  def name: String
  /** `use` with the table exported into two structs, alive until it returns */
  def exporting[T](t: Table)(use: (MemorySegment, MemorySegment) => T): T
  /** the table two structs hold (a producer's export), read, then released */
  def importing(schema: MemorySegment, array: MemorySegment): Table

object CDataCodec:
  given default: CDataCodec = OkayCData

/** the two structs' shapes: nine and ten 8-byte fields (the spec's ABI) */
object CData:
  val SchemaSize = 72L
  val ArraySize = 80L
  // ArrowSchema: format, name, metadata, flags, n_children, children, dictionary, release, private_data
  val SFormat = 0L; val SName = 8L; val SFlags = 24L; val SChildren = 32L; val SChildPtrs = 40L; val SRelease = 56L
  // ArrowArray: length, null_count, offset, n_buffers, n_children, buffers, children, dictionary, release, private_data
  val ALength = 0L; val ANulls = 8L; val AOffset = 16L; val ABuffers = 24L; val AChildren = 32L
  val ABufferPtrs = 40L; val AChildPtrs = 48L; val ARelease = 64L
  val Nullable = 2L

  private val P = ValueLayout.ADDRESS
  private val L = ValueLayout.JAVA_LONG

  def long(s: MemorySegment, at: Long): Long = s.get(L, at)
  def ptr(s: MemorySegment, at: Long): MemorySegment = s.get(P, at)
  def string(s: MemorySegment, at: Long): String =
    val p = ptr(s, at)
    if p.address == 0L then "" else p.reinterpret(Long.MaxValue).getString(0L, UTF_8)

  /** the `i`th pointer of a pointer array */
  def at(ptrs: MemorySegment, i: Int, size: Long): MemorySegment =
    ptrs.reinterpret((i + 1) * 8L).get(P, i * 8L).reinterpret(size)

  private val linker = Linker.nativeLinker()
  private val releaseCall = linker.downcallHandle(FunctionDescriptor.ofVoid(P))

  /** call a struct's release callback, if it has one: the consumer's duty */
  def release(s: MemorySegment, at: Long): Unit =
    val f = ptr(s, at)
    if f.address != 0L then
      val _ = releaseCall.invokeWithArguments(f, s)

  // our structs' release: memory is the exporting call's arena, so releasing
  // only marks the struct released (a NULL release), as the spec asks
  def releasedSchema(s: MemorySegment): Unit = s.reinterpret(SchemaSize).set(P, SRelease, MemorySegment.NULL)
  def releasedArray(s: MemorySegment): Unit = s.reinterpret(ArraySize).set(P, ARelease, MemorySegment.NULL)
  private def stub(method: String): MemorySegment =
    val h = MethodHandles.lookup().findVirtual(CData.getClass, method,
      MethodType.methodType(Void.TYPE, classOf[MemorySegment])).bindTo(CData)
    linker.upcallStub(h, FunctionDescriptor.ofVoid(P), Arena.global())
  lazy val schemaRelease: MemorySegment = stub("releasedSchema")
  lazy val arrayRelease: MemorySegment = stub("releasedArray")

/** ours: okay.arrow's columns copied once into native memory, and back */
object OkayCData extends CDataCodec:
  import CData.*

  def name = "okay"

  private val P = ValueLayout.ADDRESS
  private val L = ValueLayout.JAVA_LONG_UNALIGNED.withOrder(LITTLE_ENDIAN)
  private val D = ValueLayout.JAVA_DOUBLE_UNALIGNED.withOrder(LITTLE_ENDIAN)
  private val I = ValueLayout.JAVA_INT_UNALIGNED.withOrder(LITTLE_ENDIAN)

  def exporting[T](t: Table)(use: (MemorySegment, MemorySegment) => T): T =
    val arena = Arena.ofConfined()
    try
      val schema = arena.allocate(SchemaSize)
      val array = arena.allocate(ArraySize)
      fill(t, schema, array, arena)
      use(schema, array)
    finally arena.close()

  private def cstr(s: String, arena: Arena): MemorySegment = arena.allocateFrom(s, UTF_8)

  private def bits(n: Int, arena: Arena)(on: Int => Boolean): MemorySegment =
    val b = arena.allocate(math.max(1L, (n + 7) / 8L))
    var i = 0
    while i < n do
      if on(i) then b.set(ValueLayout.JAVA_BYTE, i / 8L, (b.get(ValueLayout.JAVA_BYTE, i / 8L) | (1 << (i % 8))).toByte)
      i += 1
    b

  private def fill(t: Table, schema: MemorySegment, array: MemorySegment, arena: Arena): Unit =
    val n = t.rows
    val k = t.cols.length
    val childSchemas = arena.allocate(math.max(1L, k * 8L))
    val childArrays = arena.allocate(math.max(1L, k * 8L))
    t.cols.zipWithIndex.foreach { case ((colName, col), i) =>
      val cs = arena.allocate(SchemaSize)
      val ca = arena.allocate(ArraySize)
      val (format, buffers, nulls) = column(col, n, arena)
      cs.set(P, SFormat, cstr(format, arena)); cs.set(P, SName, cstr(colName, arena))
      cs.set(ValueLayout.JAVA_LONG, SFlags, Nullable); cs.set(P, SRelease, schemaRelease)
      val bufs = arena.allocate(math.max(1L, buffers.length * 8L))
      buffers.zipWithIndex.foreach((b, j) => bufs.set(P, j * 8L, b))
      ca.set(ValueLayout.JAVA_LONG, ALength, n.toLong); ca.set(ValueLayout.JAVA_LONG, ANulls, nulls)
      ca.set(ValueLayout.JAVA_LONG, ABuffers, buffers.length.toLong); ca.set(P, ABufferPtrs, bufs)
      ca.set(P, ARelease, arrayRelease)
      childSchemas.set(P, i * 8L, cs); childArrays.set(P, i * 8L, ca)
    }
    schema.set(P, SFormat, cstr("+s", arena)); schema.set(ValueLayout.JAVA_LONG, SChildren, k.toLong)
    schema.set(P, SChildPtrs, childSchemas); schema.set(P, SRelease, schemaRelease)
    val top = arena.allocate(8L) // one buffer, the struct's validity: none
    array.set(ValueLayout.JAVA_LONG, ALength, n.toLong); array.set(ValueLayout.JAVA_LONG, ABuffers, 1L)
    array.set(P, ABufferPtrs, top); array.set(ValueLayout.JAVA_LONG, AChildren, k.toLong)
    array.set(P, AChildPtrs, childArrays); array.set(P, ARelease, arrayRelease)

  /** a column's format, its buffers (validity first) and its null count */
  private def column(c: Column, n: Int, arena: Arena): (String, Vector[MemorySegment], Long) =
    def validity(valid: Array[Boolean]): (MemorySegment, Long) =
      val nulls = valid.count(!_).toLong
      (if nulls == 0 then MemorySegment.NULL else bits(n, arena)(valid(_)), nulls)
    c match
      case Column.Int64(vs, valid) =>
        val (v, nulls) = validity(valid)
        val d = arena.allocate(math.max(1L, n * 8L))
        MemorySegment.copy(vs, 0, d, L, 0L, n)
        ("l", Vector(v, d), nulls)
      case Column.Float64(vs, valid) =>
        val (v, nulls) = validity(valid)
        val d = arena.allocate(math.max(1L, n * 8L))
        MemorySegment.copy(vs, 0, d, D, 0L, n)
        ("g", Vector(v, d), nulls)
      case Column.Bool(vs, valid) =>
        val (v, nulls) = validity(valid)
        ("b", Vector(v, bits(n, arena)(vs(_))), nulls)
      case Column.Utf8(vs, valid) =>
        val (v, nulls) = validity(valid)
        val encoded = vs.map(s => if s == null then Array.emptyByteArray else s.getBytes(UTF_8))
        val offs = arena.allocate((n + 1) * 4L)
        var total = 0
        var i = 0
        while i < n do { offs.set(I, i * 4L, total); total += encoded(i).length; i += 1 }
        offs.set(I, n * 4L, total)
        val chars = arena.allocate(math.max(1L, total.toLong))
        var at = 0L
        encoded.foreach { b => MemorySegment.copy(b, 0, chars, ValueLayout.JAVA_BYTE, at, b.length); at += b.length }
        ("u", Vector(v, offs, chars), nulls)
      case Column.Nulls(_) => ("n", Vector.empty, n.toLong)
      case other => throw IllegalArgumentException(
        s"a C Data table carries int64, float64, utf8, boolean and null columns, not ${other.getClass.getSimpleName}")

  def importing(schema: MemorySegment, array: MemorySegment): Table =
    try read(schema.reinterpret(SchemaSize), array.reinterpret(ArraySize))
    finally
      release(array.reinterpret(ArraySize), ARelease)
      release(schema.reinterpret(SchemaSize), SRelease)

  private def read(schema: MemorySegment, array: MemorySegment): Table =
    val top = string(schema, SFormat)
    if top != "+s" then throw IllegalArgumentException(s"a C Data table is a struct array (+s), not $top")
    val n = long(array, ALength).toInt
    val k = long(schema, SChildren).toInt
    val cols = (0 until k).toVector.map { i =>
      val cs = at(ptr(schema, SChildPtrs), i, SchemaSize)
      val ca = at(ptr(array, AChildPtrs), i, ArraySize)
      string(cs, SName) -> readColumn(string(cs, SFormat), ca, n)
    }
    Table(cols, Vector.empty)

  private def readColumn(format: String, ca: MemorySegment, n: Int): Column =
    val off = long(ca, AOffset)
    val nb = long(ca, ABuffers).toInt
    def buf(j: Int, size: Long): MemorySegment = at(ptr(ca, ABufferPtrs), j, size)
    def isSet(b: MemorySegment, i: Long): Boolean = (b.get(ValueLayout.JAVA_BYTE, i / 8) >> (i % 8).toInt & 1) == 1
    val valid =
      if format == "n" then Array.fill(n)(false)
      else
        val v = if nb > 0 then ptr(ptr(ca, ABufferPtrs).reinterpret(8L), 0L) else MemorySegment.NULL
        if v.address == 0L || long(ca, ANulls) == 0L then Array.fill(n)(true)
        else
          val b = v.reinterpret((off + n + 7) / 8 + 1)
          Array.tabulate(n)(i => isSet(b, off + i))
    format match
      case "l" =>
        val out = new Array[Long](n)
        MemorySegment.copy(buf(1, (off + n) * 8), L, off * 8, out, 0, n)
        Column.Int64(out, valid)
      case "g" =>
        val out = new Array[Double](n)
        MemorySegment.copy(buf(1, (off + n) * 8), D, off * 8, out, 0, n)
        Column.Float64(out, valid)
      case "b" =>
        val b = buf(1, (off + n + 7) / 8 + 1)
        Column.Bool(Array.tabulate(n)(i => isSet(b, off + i)), valid)
      case "u" =>
        val offs = buf(1, (off + n + 1) * 4)
        val end = offs.get(I, (off + n) * 4)
        val chars = buf(2, math.max(1L, end.toLong))
        Column.Utf8(Array.tabulate(n) { i =>
          val a = offs.get(I, (off + i) * 4)
          val b = offs.get(I, (off + i + 1) * 4)
          String(chars.asSlice(a.toLong, (b - a).toLong).toArray(ValueLayout.JAVA_BYTE), UTF_8)
        }, valid)
      case "n" => Column.Nulls(n)
      case other => throw IllegalArgumentException(s"a C Data column of format '$other' is not int64, float64, utf8, boolean or null")

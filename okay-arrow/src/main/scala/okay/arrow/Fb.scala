package okay.arrow

import java.nio.charset.StandardCharsets.UTF_8

// ---- bytes, little-endian ------------------------------------------------

private[arrow] final class Bytes:
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

private[arrow] final class In(b: Array[Byte]):
  var at = 0
  def remaining: Int = b.length - at
  def skip(k: Int): Unit = at += k
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
private[arrow] object Fb:
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
    if b.length < 4 then throw IllegalStateException("not an Arrow stream this reads: a message's metadata is shorter than its root offset")
    try At(b, i32le(b, 0))
    catch case _: IndexOutOfBoundsException => throw IllegalStateException("not an Arrow stream this reads: a message's metadata points outside itself")

  def u16le(b: Array[Byte], at: Int): Int = (b(at) & 0xff) | (b(at + 1) & 0xff) << 8
  def i32le(b: Array[Byte], at: Int): Int =
    (b(at) & 0xff) | (b(at + 1) & 0xff) << 8 | (b(at + 2) & 0xff) << 16 | (b(at + 3) & 0xff) << 24
  def i64le(b: Array[Byte], at: Int): Long =
    (i32le(b, at).toLong & 0xffffffffL) | (i32le(b, at + 4).toLong << 32)

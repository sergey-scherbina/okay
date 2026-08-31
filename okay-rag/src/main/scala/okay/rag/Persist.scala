package okay.rag

import okay.codec.{Cbor, Json, Schema}
import okay.lex.Span

/**
 * The store's own persistence, through our codec: one derived Schema
 * serves both wires, so an index writes as compact CBOR and reads
 * back as the same values — and can be inspected as JSON when
 * someone needs to look at it.
 *
 * The vector travels as RAW float32 bytes, which is the shape it
 * already has in memory. It used to travel as `List[Double]`, because
 * that is what the Schema algebra could carry, and that cost three
 * things at once: every component was boxed on the way out and again
 * on the way in, every component was widened to 64 bits it did not
 * use, and CBOR spent nine bytes per component (a 0xFB tag and eight
 * bytes) where four will do. For a 1536-dimension vector that is
 * 13827 bytes against 6147.
 *
 * The fix was upstream: `Schema.SBytes`, a primitive the algebra was
 * missing — CBOR has a first-class byte string and JSON has no bytes
 * at all, so without it every binary payload has to be smuggled
 * through something else. Precision is unchanged and exact: float32
 * in, the same float32 bits out, where before it was float32 widened
 * to float64 and narrowed back (also exact, just wasteful).
 */
object Persist {

  /** the on-disk shape: List where the runtime uses Seq/Vector, since
   * that is the collection the Schema algebra derives for */
  final case class StoredSpan(offset: Int, line: Int, column: Int, length: Int)
  final case class StoredSegment(source: String, span: StoredSpan,
                                 text: String, path: List[String])
  final case class StoredItem(segment: StoredSegment, vector: Array[Byte])
  final case class StoredIndex(items: List[StoredItem])

  given Schema[StoredSpan] = Schema.derived
  given Schema[StoredSegment] = Schema.derived
  given Schema[StoredItem] = Schema.derived
  given Schema[StoredIndex] = Schema.derived

  private def out(s: Segment): StoredSegment =
    StoredSegment(s.source,
      StoredSpan(s.span.offset, s.span.line, s.span.column, s.span.length),
      s.text, s.path.toList)

  private def in(s: StoredSegment): Segment =
    Segment(s.source, Span(s.span.offset, s.span.line, s.span.column, s.span.length),
      s.text, s.path)

  /** float32 little-endian, the layout the vector already has */
  def pack(v: Embedding): Array[Byte] =
    val out = new Array[Byte](v.length * 4)
    var i = 0
    while i < v.length do
      val b = java.lang.Float.floatToIntBits(v(i))
      val o = i * 4
      out(o) = b.toByte
      out(o + 1) = (b >> 8).toByte
      out(o + 2) = (b >> 16).toByte
      out(o + 3) = (b >> 24).toByte
      i += 1
    out

  /** the inverse; a length that is not a multiple of four is damage,
   * and damage truncates rather than throws — the rule everywhere
   * else in this stack */
  def unpack(bs: Array[Byte]): Embedding =
    val n = bs.length / 4
    val out = new Array[Float](n)
    var i = 0
    while i < n do
      val o = i * 4
      val b = (bs(o) & 0xFF) | ((bs(o + 1) & 0xFF) << 8) |
        ((bs(o + 2) & 0xFF) << 16) | ((bs(o + 3) & 0xFF) << 24)
      out(i) = java.lang.Float.intBitsToFloat(b)
      i += 1
    embedding(out)

  def toStored(items: Seq[(Segment, Embedding)]): StoredIndex =
    StoredIndex(items.map((s, v) => StoredItem(out(s), pack(v))).toList)

  def fromStored(x: StoredIndex): Vector[(Segment, Embedding)] =
    x.items.map(it => (in(it.segment), unpack(it.vector))).toVector

  /** compact binary — the shipping format */
  def save(store: MemoryStore): Array[Byte] = Cbor.write(toStored(store.snapshot))

  def load(bytes: Array[Byte]): Either[String, Vector[(Segment, Embedding)]] =
    Cbor.read[StoredIndex](bytes).map(fromStored)

  /** the same index as text, for looking at it */
  def toJson(store: MemoryStore): String = Json.write(toStored(store.snapshot))
}

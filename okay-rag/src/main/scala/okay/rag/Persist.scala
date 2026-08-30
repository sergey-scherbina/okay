package okay.rag

import okay.codec.{Cbor, Json, Schema}
import okay.lex.Span

/**
 * The store's own persistence, through our codec: one derived Schema
 * serves both wires, so an index writes as compact CBOR and reads
 * back as the same values — and can be inspected as JSON when
 * someone needs to look at it. Floats travel as doubles because that
 * is what the Schema algebra carries; the round trip is exact for
 * every value an embedder produces.
 */
object Persist {

  /** the on-disk shape: List where the runtime uses Seq/Vector, since
   * that is the collection the Schema algebra derives for */
  final case class StoredSpan(offset: Int, line: Int, column: Int, length: Int)
  final case class StoredSegment(source: String, span: StoredSpan,
                                 text: String, path: List[String])
  final case class StoredItem(segment: StoredSegment, vector: List[Double])
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

  def toStored(items: Seq[(Segment, Embedding)]): StoredIndex =
    StoredIndex(items.map((s, v) => StoredItem(out(s), v.map(_.toDouble).toList)).toList)

  def fromStored(x: StoredIndex): Vector[(Segment, Embedding)] =
    x.items.map(it => (in(it.segment), it.vector.map(_.toFloat).toVector)).toVector

  /** compact binary — the shipping format */
  def save(store: MemoryStore): Array[Byte] = Cbor.write(toStored(store.snapshot))

  def load(bytes: Array[Byte]): Either[String, Vector[(Segment, Embedding)]] =
    Cbor.read[StoredIndex](bytes).map(fromStored)

  /** the same index as text, for looking at it */
  def toJson(store: MemoryStore): String = Json.write(toStored(store.snapshot))
}

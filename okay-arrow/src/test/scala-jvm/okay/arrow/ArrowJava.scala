package okay.arrow

import java.nio.charset.StandardCharsets.UTF_8
import org.apache.arrow.memory.BufferAllocator
import org.apache.arrow.vector.{BigIntVector, Float8Vector, VarCharVector, VectorSchemaRoot}
import org.apache.arrow.vector.ipc.{ArrowStreamReader, ArrowStreamWriter}

/**
 * The reference side of stage 0 (specs/okay-arrow.md): the same table
 * through Arrow Java 19, the way its documentation writes and reads an
 * IPC stream. Test scope only — okay-arrow never depends on it.
 */
object ArrowJava:

  /** the stage-0 table as plain JVM arrays: float64, int64, utf8 with a
   * null in every tenth row */
  final case class Data(a: Array[Double], b: Array[Long], s: Array[String], sValid: Array[Boolean]):
    def rows: Int = a.length
    def okay: Table = Table(Vector(
      "a" -> Column.Float64(a, Array.fill(rows)(true)),
      "b" -> Column.Int64(b, Array.fill(rows)(true)),
      "s" -> Column.Utf8(s, sValid)), Vector.empty)

  def data(rows: Int): Data =
    Data(Array.tabulate(rows)(_ + 0.5), Array.tabulate(rows)(_.toLong),
      Array.tabulate(rows)(i => "row" + i), Array.tabulate(rows)(_ % 10 != 0))

  /** Arrow's own columns, filled from the arrays */
  def fill(alloc: BufferAllocator, d: Data): VectorSchemaRoot =
    val n = d.rows
    val a = Float8Vector("a", alloc); a.allocateNew(n)
    val b = BigIntVector("b", alloc); b.allocateNew(n)
    val s = VarCharVector("s", alloc); s.allocateNew(n.toLong * 12, n)
    var i = 0
    while i < n do
      a.set(i, d.a(i)); b.set(i, d.b(i))
      if d.sValid(i) then s.setSafe(i, d.s(i).getBytes(UTF_8)) else s.setNull(i)
      i += 1
    a.setValueCount(n); b.setValueCount(n); s.setValueCount(n)
    VectorSchemaRoot.of(a, b, s)

  /** a filled root as one IPC stream */
  def write(root: VectorSchemaRoot): Array[Byte] =
    val out = java.io.ByteArrayOutputStream()
    val w = ArrowStreamWriter(root, null, java.nio.channels.Channels.newChannel(out))
    try { w.start(); w.writeBatch(); w.end() } finally w.close()
    out.toByteArray

  /** arrays to IPC bytes: fill, write, free */
  def writeFromArrays(alloc: BufferAllocator, d: Data): Array[Byte] =
    val root = fill(alloc, d)
    try write(root) finally root.close()

  /** IPC bytes loaded into Arrow's own columns, `f` applied, freed */
  def read[A](alloc: BufferAllocator, bytes: Array[Byte])(f: VectorSchemaRoot => A): A =
    val r = ArrowStreamReader(java.io.ByteArrayInputStream(bytes), alloc)
    try
      if !r.loadNextBatch() then throw IllegalStateException("no batch")
      f(r.getVectorSchemaRoot)
    finally r.close()

  /** IPC bytes to the same arrays `Data` holds */
  def readToArrays(alloc: BufferAllocator, bytes: Array[Byte]): Data =
    read(alloc, bytes) { root =>
      val n = root.getRowCount
      (root.getVector("a"), root.getVector("b"), root.getVector("s")) match
        case (a: Float8Vector, b: BigIntVector, s: VarCharVector) =>
          Data(Array.tabulate(n)(a.get), Array.tabulate(n)(b.get),
            Array.tabulate(n)(i => if s.isNull(i) then "" else String(s.get(i), UTF_8)),
            Array.tabulate(n)(i => !s.isNull(i)))
        case other => throw IllegalStateException(s"not the stage-0 table: $other")
    }

  /** ours: IPC bytes to the same arrays */
  def okayToArrays(bytes: Array[Byte]): Data =
    OkayArrow.read(bytes).cols.map(_._2) match
      case Vector(Column.Float64(a, _), Column.Int64(b, _), Column.Utf8(s, ok)) => Data(a, b, s, ok)
      case other => throw IllegalStateException(s"not the stage-0 table: $other")

  def same(x: Data, y: Data): Boolean =
    x.a.sameElements(y.a) && x.b.sameElements(y.b) && x.sValid.sameElements(y.sValid) &&
      x.s.indices.forall(i => !x.sValid(i) || x.s(i) == y.s(i))

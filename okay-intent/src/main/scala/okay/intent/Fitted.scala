package okay.intent

import okay.codec.Schema
import okay.rag.{Embedding, embedding}
import java.nio.{ByteBuffer, ByteOrder}

/**
 * A fitted model is DATA (specs/intent-classify.md).
 *
 * Without this, fitting lives wherever loading lives: every process
 * start re-fits, and re-fitting needs the teacher — which drags an
 * embedding server into the STARTUP path of a service whose request
 * path was carefully kept clean. A consumer put it exactly right: "no
 * generation on the request path" should also mean no FITTING on it.
 *
 * The schemas here are hand-built rather than derived. Weights are
 * `Array[Double]` and vectors are `ArraySeq[Float]`; a derivation
 * sends each as a JSON array of numbers, which is how an embedding
 * once travelled as `List[Double]` — nine bytes and a boxed object per
 * component. `SBytes` exists in the codec because of that incident, so
 * numbers ride as bytes here, little-endian.
 *
 * MEASURED, because the first version of this comment claimed more
 * than the code delivers: a two-class probe over 1024 dimensions is
 * 21KB as bytes against 36KB as decimal literals — 1.7x, not an order
 * of magnitude, because base64 gives back a third of what the binary
 * form saves. The reason to keep it is the one that survives the
 * number: no boxing on the way through, and a shape a reader can check
 * (a width and a blob) rather than a nested list whose rows might
 * disagree.
 */
object Fitted {

  // ---------------------------------------------------------------
  // the primitives: arrays of numbers as bytes, not as digits

  private def doublesTo(xs: Array[Double]): Array[Byte] =
    val b = ByteBuffer.allocate(xs.length * 8).order(ByteOrder.LITTLE_ENDIAN)
    xs.foreach(b.putDouble)
    b.array

  private def doublesFrom(bs: Array[Byte]): Array[Double] =
    val b = ByteBuffer.wrap(bs).order(ByteOrder.LITTLE_ENDIAN)
    Array.fill(bs.length / 8)(b.getDouble)

  private def floatsTo(v: Embedding): Array[Byte] =
    val b = ByteBuffer.allocate(v.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    v.foreach(b.putFloat)
    b.array

  private def floatsFrom(bs: Array[Byte]): Embedding =
    val b = ByteBuffer.wrap(bs).order(ByteOrder.LITTLE_ENDIAN)
    embedding(Array.fill(bs.length / 4)(b.getFloat))

  given Schema[Array[Double]] = Schema.wrap(doublesFrom, doublesTo)
  given vectorSchema: Schema[Embedding] = Schema.wrap(floatsFrom, floatsTo)

  /** a matrix as one blob plus its width — a `Vector[Array[Double]]`
   * would be a length-prefixed list of blobs, and the width is what a
   * reader actually needs to check */
  final case class Matrix(width: Int, cells: Array[Double]) derives Schema:
    def rows: Vector[Array[Double]] =
      if width <= 0 then Vector.empty
      else cells.grouped(width).toVector

  object Matrix:
    def of(rows: Seq[Array[Double]]): Matrix =
      Matrix(rows.headOption.map(_.length).getOrElse(0), rows.toArray.flatten)

  // ---------------------------------------------------------------
  // the models, each as a plain record

  final case class ProbeModel(classes: Vector[String], w: Matrix, b: Array[Double]) derives Schema
  final case class CentroidModel(classes: Vector[String], vectors: Vector[Embedding]) derives Schema
  final case class GramsModel(classes: Vector[String], dim: Int, w: Matrix,
                              b: Array[Double], low: Int, high: Int) derives Schema
  final case class StaticModel(dim: Int, units: Vector[String],
                               vectors: Vector[Embedding], weights: Array[Double]) derives Schema

  def save(t: Probe.Trained): ProbeModel = ProbeModel(t.classes, Matrix.of(t.w.toSeq), t.b)
  def load(m: ProbeModel): Probe.Trained = Probe.Trained(m.classes, m.w.rows.toArray, m.b)

  def save(t: Centroid.Trained): CentroidModel =
    val ks = t.byClass.keys.toVector.sorted
    CentroidModel(ks, ks.map(t.byClass))
  def load(m: CentroidModel): Centroid.Trained =
    Centroid.Trained(m.classes.zip(m.vectors).toMap)

  def save(t: CharGrams.Trained): GramsModel =
    GramsModel(t.classes, t.dim, Matrix.of(t.w.toSeq), t.b, t.low, t.high)
  def load(m: GramsModel): CharGrams.Trained =
    CharGrams.Trained(m.classes, m.dim, m.w.rows.toArray, m.b, m.low, m.high)

  /**
   * A static table loses its `split` on the way out and takes it back
   * on the way in, because a function is not data. The caller passes
   * the same one it distilled with — `Static.units` for a table with
   * pairs, `Static.tokens` for one without — and getting that wrong is
   * a silent accuracy loss rather than an error, which is why `load`
   * demands it rather than defaulting.
   */
  def save(t: Static.Table): StaticModel =
    val ks = t.vectors.keys.toVector.sorted
    StaticModel(t.dim, ks, ks.map(t.vectors), ks.map(t.weights.getOrElse(_, 1.0)).toArray)
  def load(m: StaticModel, split: String => Vector[String]): Static.Table =
    Static.Table(m.dim, m.units.zip(m.vectors).toMap,
      m.units.zip(m.weights).toMap, split)
}

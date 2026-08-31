package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq

/**
 * `Embedding = Vector[Float]`, and `Vector` is a generic trie over
 * `Array[AnyRef]`, so every component is a boxed `java.lang.Float`.
 * That was a default reached for, not a decision — nothing needs it,
 * and `Persist` converts to `List[Double]` for the wire anyway.
 *
 * The question is what it costs, and this project has been wrong
 * about exactly this before: the chunked-lexing gap was blamed on
 * per-character boxing and the experiment refuted it (8% of a 23%
 * gap). So the same three-way probe, at the size that matters —
 * 1536 components, which is what the common providers return.
 *
 *  - `Vector[Float]`      — what ships today
 *  - `ArraySeq[Float]`    — unboxed STORAGE, but read through the
 *                           generic `apply`, which boxes on return
 *  - `ArraySeq.ofFloat`   — the concrete class, whose `apply` is
 *                           declared to return a primitive Float
 *  - `Array[Float]`       — the floor; mutable, so not a candidate,
 *                           but it says how much is left on the table
 *
 * The gap between the second and third rows is the whole point of the
 * probe: unboxing the storage is not the same as unboxing the READ,
 * and only one of them is a type-alias change.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class EmbeddingBenchmark {

  val dim = 1536
  val corpus = 2000          // segments to score for one query

  private val rnd = scala.util.Random(42)
  private def raw(): Array[Float] =
    val a = new Array[Float](dim)
    var i = 0
    while i < dim do { a(i) = rnd.nextFloat(); i += 1 }
    a

  val queryArr: Array[Float] = raw()
  val itemsArr: Vector[Array[Float]] = Vector.fill(corpus)(raw())

  val queryVec: Vector[Float] = queryArr.toVector
  val itemsVec: Vector[Vector[Float]] = itemsArr.map(_.toVector)

  val queryAS: ArraySeq[Float] = ArraySeq.unsafeWrapArray(queryArr.clone())
  val itemsAS: Vector[ArraySeq[Float]] =
    itemsArr.map(a => ArraySeq.unsafeWrapArray(a.clone()))

  val queryOf: ArraySeq.ofFloat = ArraySeq.ofFloat(queryArr.clone())
  val itemsOf: Vector[ArraySeq.ofFloat] =
    itemsArr.map(a => ArraySeq.ofFloat(a.clone()))

  // ---- one cosine, at provider dimension

  private def cosVec(a: Vector[Float], b: Vector[Float]): Float =
    var dot = 0.0f; var na = 0.0f; var nb = 0.0f; var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      dot += a(i) * b(i); na += a(i) * a(i); nb += b(i) * b(i); i += 1
    if na == 0f || nb == 0f then 0f else dot / math.sqrt(na * nb).toFloat

  private def cosAS(a: ArraySeq[Float], b: ArraySeq[Float]): Float =
    var dot = 0.0f; var na = 0.0f; var nb = 0.0f; var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      dot += a(i) * b(i); na += a(i) * a(i); nb += b(i) * b(i); i += 1
    if na == 0f || nb == 0f then 0f else dot / math.sqrt(na * nb).toFloat

  private def cosOf(a: ArraySeq.ofFloat, b: ArraySeq.ofFloat): Float =
    var dot = 0.0f; var na = 0.0f; var nb = 0.0f; var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      dot += a(i) * b(i); na += a(i) * a(i); nb += b(i) * b(i); i += 1
    if na == 0f || nb == 0f then 0f else dot / math.sqrt(na * nb).toFloat

  private def cosArr(a: Array[Float], b: Array[Float]): Float =
    var dot = 0.0f; var na = 0.0f; var nb = 0.0f; var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      dot += a(i) * b(i); na += a(i) * a(i); nb += b(i) * b(i); i += 1
    if na == 0f || nb == 0f then 0f else dot / math.sqrt(na * nb).toFloat

  @Benchmark def cosine1Vector: Float = cosVec(queryVec, itemsVec(0))
  @Benchmark def cosine2ArraySeq: Float = cosAS(queryAS, itemsAS(0))
  @Benchmark def cosine3OfFloat: Float = cosOf(queryOf, itemsOf(0))
  @Benchmark def cosine4Array: Float = cosArr(queryArr, itemsArr(0))

  // ---- a whole query: score the corpus, which is what a user feels

  @Benchmark def search1Vector: Float =
    var best = Float.MinValue; var i = 0
    while i < corpus do
      val s = cosVec(queryVec, itemsVec(i)); if s > best then best = s; i += 1
    best

  @Benchmark def search3OfFloat: Float =
    var best = Float.MinValue; var i = 0
    while i < corpus do
      val s = cosOf(queryOf, itemsOf(i)); if s > best then best = s; i += 1
    best

  @Benchmark def search4Array: Float =
    var best = Float.MinValue; var i = 0
    while i < corpus do
      val s = cosArr(queryArr, itemsArr(i)); if s > best then best = s; i += 1
    best
}

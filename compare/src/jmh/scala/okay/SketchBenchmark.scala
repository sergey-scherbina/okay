package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.util.hashing.MurmurHash3

/**
 * What the sketches cost per element, and what the floor is.
 *
 * All three keep their state in persistent `Vector`s and rebuild it on
 * every `add`, which is the same shape of defect the aggregators had
 * (a tuple per element) one layer down:
 *
 *  - `CMS.add` does `rows.zipWithIndex.map(...)` with an `updated` per
 *    row — a tuple per row, a new outer Vector, and a copied path in
 *    each inner one, for what should be `depth` array writes.
 *  - `HLL.add` does `registers.updated(idx, ...)` — a copied path in a
 *    16k-element Vector, for what should be one byte store.
 *  - `TDigest` carries `Vector[(Double, Double)]`, two boxes per
 *    centroid.
 *
 * The floors below are hand-written array versions doing the same
 * arithmetic. Whether the difference is worth changing the types is
 * what these lanes are for; the sketches are merge-based and
 * distributable, so their state has to stay immutable ACROSS adds —
 * but not necessarily within one.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class SketchBenchmark {

  val n = 10000
  val keys: Array[String] = Array.tabulate(n)(i => s"key-$i")

  // ---- as shipped

  @Benchmark
  def hllShipped: Long =
    val agg = Sketch.hyperLogLog[String](14)
    agg.present(keys.foldLeft(agg.init)(agg.add))

  @Benchmark
  def cmsShipped: Long =
    val agg = Sketch.countMin[String](2048, 5)
    agg.present(keys.foldLeft(agg.init)(agg.add)).total

  @Benchmark
  def tdigestShipped: Double =
    val agg = Sketch.tDigest(100)
    agg.present((0 until n).foldLeft(agg.init)((s, i) => agg.add(s, i.toDouble)))
      .quantile(0.5)

  // ---- the floors: the same arithmetic over arrays

  @Benchmark
  def hllFloor: Long =
    val p = 14
    val regs = new Array[Byte](1 << p)
    var i = 0
    while i < keys.length do
      val a = keys(i)
      val h = (MurmurHash3.mix(0x9747b28c, a.##) & 0xFFFFFFFFL) |
        (MurmurHash3.mix(0x85ebca6b, a.##).toLong << 32)
      val idx = (h >>> (64 - p)).toInt
      val w = h << p
      val rank = (if w == 0 then 64 - p else java.lang.Long.numberOfLeadingZeros(w)) + 1
      if rank > regs(idx) then regs(idx) = rank.toByte
      i += 1
    var zeros = 0L
    var j = 0
    while j < regs.length do
      if regs(j) == 0 then zeros += 1
      j += 1
    zeros

  @Benchmark
  def cmsFloor: Long =
    val width = 2048
    val depth = 5
    val rows = Array.ofDim[Long](depth, width)
    var total = 0L
    var i = 0
    while i < keys.length do
      val a = keys(i)
      var d = 0
      while d < depth do
        val c = math.abs(MurmurHash3.mix(d, a.##)) % width
        rows(d)(c) += 1
        d += 1
      total += 1
      i += 1
    total
}

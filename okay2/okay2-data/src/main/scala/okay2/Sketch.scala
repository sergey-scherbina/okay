package okay2

import scala.util.hashing.MurmurHash3

/**
 * Approximate aggregators — the Scala 3 core's okay-data `Sketch`:
 * distinct count, frequencies and quantiles are NOT exact monoids in
 * bounded space, but their sketches ARE, at the price of a bounded,
 * stated error. Every sketch merges associatively, so all of them are
 * chunk-parallel and distribution-ready through the same `Aggregator`
 * triple, and zip with the exact ones (Flajolet et al. for HyperLogLog,
 * Cormode–Muthukrishnan for Count-Min, Dunning for the t-digest).
 *
 * The three accumulators are MUTABLE in place, as in the core and for
 * its measured reasons (19x, 31x and ~100x over the value shapes), under
 * two rules that keep them inside the Aggregator contract: `init`
 * allocates a fresh sketch on every call, and `merge` allocates its
 * result — Spark's `seqOp` may modify its first argument, and nothing
 * more is assumed.
 */
object Sketch {

  // HyperLogLog: distinct count in 2^p bytes, error ~ 1.04 / sqrt(2^p)

  /** the registers: elementwise max is the monoid */
  final class HLL(val p: Int, private[Sketch] val registers: Array[Byte]) {
    def m: Int = 1 << p

    private[Sketch] def observe(idx: Int, rank: Int): this.type = {
      if (rank > registers(idx)) registers(idx) = rank.toByte
      this
    }

    private[Sketch] def merged(that: HLL): HLL = {
      val out = new Array[Byte](registers.length)
      var i = 0
      while (i < out.length) {
        val x = registers(i); val y = that.registers(i)
        out(i) = if (x >= y) x else y
        i += 1
      }
      new HLL(p, out)
    }

    /** the cardinality estimate */
    def estimate: Long = {
      val zeros = registers.count(_ == 0)
      var sum = 0.0
      registers.foreach(r => sum += math.pow(2.0, -r.toDouble))
      val alpha = m match {
        case 16 => 0.673
        case 32 => 0.697
        case 64 => 0.709
        case _ => 0.7213 / (1.0 + 1.079 / m)
      }
      val e = alpha * m * m / sum
      val corrected =
        if (e <= 2.5 * m && zeros > 0) m * math.log(m.toDouble / zeros)
        else if (e > (1L << 32) / 30.0) -(1L << 32).toDouble * math.log1p(-e / (1L << 32).toDouble)
        else e
      math.round(corrected)
    }
  }

  /** approximate distinct count: 2^p one-byte registers (p = 14 is 16KB
   * for ~0.8% standard error) */
  def hyperLogLog[A](p: Int = 14): Aggregator[A, HLL, Long] = new Aggregator[A, HLL, Long] {
    // a def, not a captured value: every fold gets its own registers
    def init: HLL = new HLL(p, new Array[Byte](1 << p))

    def add(s: HLL, a: A): HLL = {
      val h = (MurmurHash3.mix(0x9747b28c, a.##) & 0xFFFFFFFFL) | (MurmurHash3.mix(0x85ebca6b, a.##).toLong << 32)
      val idx = (h >>> (64 - p)).toInt
      val w = h << p
      val rank = (if (w == 0) 64 - p else java.lang.Long.numberOfLeadingZeros(w)) + 1
      s.observe(idx, rank)
    }

    def merge(a: HLL, b: HLL): HLL = a.merged(b)
    def present(s: HLL): Long = s.estimate
  }

  // Count-Min: frequencies over-estimated by at most eps * N, with
  // probability 1 - delta; width = e/eps, depth = ln(1/delta)

  /** the counter matrix: elementwise addition is the monoid */
  final class CMS(val width: Int, private[Sketch] val rows: Array[Array[Long]], private var count: Long) {
    /** how many elements went in */
    def total: Long = count

    private def cell(row: Int, a: Any): Int = math.floorMod(MurmurHash3.mix(row * 0x9e3779b9 + 1, a.##), width)

    /** the estimated count of a (never an under-estimate) */
    def apply[A](a: A): Long = {
      var min = Long.MaxValue
      var i = 0
      while (i < rows.length) {
        val v = rows(i)(cell(i, a))
        if (v < min) min = v
        i += 1
      }
      min
    }

    private[Sketch] def add(a: Any): this.type = {
      var i = 0
      while (i < rows.length) { rows(i)(cell(i, a)) += 1; i += 1 }
      count += 1
      this
    }

    private[Sketch] def merged(that: CMS): CMS = {
      val out = Array.ofDim[Long](rows.length, width)
      var i = 0
      while (i < rows.length) {
        val r = rows(i); val o = out(i); val t = that.rows(i)
        var j = 0
        while (j < width) { o(j) = r(j) + t(j); j += 1 }
        i += 1
      }
      new CMS(width, out, count + that.count)
    }
  }

  /** approximate frequencies: the answer is the queryable sketch */
  def countMin[A](width: Int = 2048, depth: Int = 5): Aggregator[A, CMS, CMS] = new Aggregator[A, CMS, CMS] {
    def init: CMS = new CMS(width, Array.ofDim[Long](depth, width), 0L)
    def add(s: CMS, a: A): CMS = s.add(a)
    def merge(a: CMS, b: CMS): CMS = a.merged(b)
    def present(s: CMS): CMS = s
  }

  // t-digest: quantiles from clustered centroids; clusters stay small
  // near the tails (the k-scale bound 4 n q (1-q) / delta)

  /**
   * Sorted centroids, and a buffer: incoming points land in the unsorted
   * buffer at O(1), and compression merges it into the centroids once it
   * fills (Dunning's shape); merge-then-compress is the monoid.
   */
  final class TDigest private[Sketch] (
      val delta: Int,
      private[Sketch] var means: Array[Double],
      private[Sketch] var weights: Array[Double],
      private[Sketch] var size: Int,
      private[Sketch] var buf: Array[Double],
      private[Sketch] var buffered: Int,
      private var n: Long) {

    def count: Long = n

    /** the centroids, (mean, weight) */
    def centroids: Vector[(Double, Double)] = {
      flushed()
      Vector.tabulate(size)(i => (means(i), weights(i)))
    }

    private[Sketch] def add(x: Double): this.type = {
      if (buffered == buf.length) compress()
      buf(buffered) = x
      buffered += 1
      n += 1
      this
    }

    private[Sketch] def flushed(): Unit = if (buffered > 0) compress()

    /** merge the sorted centroids with the sorted buffer, then the k-scale pass */
    private def compress(): Unit = {
      val add = java.util.Arrays.copyOf(buf, buffered)
      java.util.Arrays.sort(add)
      val total = size + buffered
      val m = new Array[Double](total)
      val w = new Array[Double](total)
      var i = 0; var j = 0; var k = 0
      while (i < size || j < buffered) {
        if (j >= buffered || (i < size && means(i) <= add(j))) { m(k) = means(i); w(k) = weights(i); i += 1 }
        else { m(k) = add(j); w(k) = 1.0; j += 1 }
        k += 1
      }
      buffered = 0
      bound(m, w, total)
    }

    /** the k-scale pass over a sorted, weighted run — shared with
     * `merged`, whose centroids come with their own weights */
    private[Sketch] def bound(m: Array[Double], w: Array[Double], total: Int): Unit =
      if (total == 0) { means = m; weights = w; size = 0 }
      else {
        val om = new Array[Double](total)
        val ow = new Array[Double](total)
        var out = 0
        var cm = m(0); var cw = w(0)
        var cum = 0.0
        var t = 1
        while (t < total) {
          val q = (cum + cw / 2) / n
          val lim = 4.0 * n * q * (1 - q) / delta
          if (cw + w(t) <= math.max(lim, 1.0)) {
            val nw = cw + w(t)
            cm = cm + (m(t) - cm) * w(t) / nw
            cw = nw
          } else {
            om(out) = cm; ow(out) = cw; out += 1
            cum += cw
            cm = m(t); cw = w(t)
          }
          t += 1
        }
        om(out) = cm; ow(out) = cw; out += 1
        means = om; weights = ow; size = out
      }

    /** the q-quantile estimate (0 <= q <= 1) */
    def quantile(q: Double): Double = {
      flushed()
      if (size == 0) Double.NaN
      else if (size == 1) means(0)
      else {
        val target = q * n
        var cum = 0.0
        var i = 0
        while (i < size && cum + weights(i) / 2 < target) { cum += weights(i); i += 1 }
        if (i == 0) means(0)
        else if (i >= size) means(size - 1)
        else {
          val w1 = weights(i - 1); val w2 = weights(i)
          val between = (target - (cum - w1 / 2)) / ((w1 + w2) / 2)
          means(i - 1) + (means(i) - means(i - 1)) * between.max(0).min(1)
        }
      }
    }

    /** a two-finger merge of two weighted runs, then the same k-scale
     * pass — through the buffer every centroid would weigh 1 */
    private[Sketch] def merged(that: TDigest): TDigest = {
      flushed(); that.flushed()
      val total = size + that.size
      val m = new Array[Double](total)
      val w = new Array[Double](total)
      var i = 0; var j = 0; var k = 0
      while (i < size || j < that.size) {
        if (j >= that.size || (i < size && means(i) <= that.means(j))) { m(k) = means(i); w(k) = weights(i); i += 1 }
        else { m(k) = that.means(j); w(k) = that.weights(j); j += 1 }
        k += 1
      }
      val out = TDigest.empty(delta)
      out.n = n + that.n
      out.bound(m, w, total)
      out
    }
  }

  object TDigest {
    /** the buffer is a few times delta: compression rare, a merge pass cache-friendly */
    def empty(delta: Int): TDigest =
      new TDigest(delta, new Array[Double](0), new Array[Double](0), 0, new Array[Double](delta * 5), 0, 0L)
  }

  /** approximate quantiles; delta ~ 100 gives sharp tails */
  def tDigest(delta: Int = 100): Aggregator[Double, TDigest, TDigest] = new Aggregator[Double, TDigest, TDigest] {
    def init: TDigest = TDigest.empty(delta)
    def add(s: TDigest, x: Double): TDigest = s.add(x)
    def merge(a: TDigest, b: TDigest): TDigest = a.merged(b)
    def present(s: TDigest): TDigest = { s.flushed(); s }
  }
}

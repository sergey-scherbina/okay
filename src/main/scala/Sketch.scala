package okay

import scala.util.hashing.MurmurHash3

/**
 * Approximate aggregators (specs/aggregators.md P1b): distinct count,
 * frequencies and quantiles are NOT exact monoids in bounded space —
 * but their sketches ARE monoids, at the price of a bounded, stated
 * error. Exact and approximate are two distinguishable, equally
 * first-class kinds of Aggregator; every sketch below merges
 * associatively, so all of them are chunk-parallel and
 * distribution-ready through the same (init, add, merge) triple.
 * Written fresh from the papers (Flajolet et al. for HyperLogLog,
 * Cormode–Muthukrishnan for Count-Min, Dunning for the t-digest).
 */
object Sketch {

  // ------------------------------------------------------------------
  // HyperLogLog: distinct count in 2^p bytes, error ~ 1.04 / sqrt(2^p)

  /**
   * The registers: elementwise-max is the monoid.
   *
   * A raw `Array[Byte]`, and `add` writes into it and hands the SAME
   * sketch back. That is the one place this file departs from
   * value semantics, and it is deliberate: with a `Vector[Byte]` every
   * element rebuilt a path through a 16k-element tree, measured at
   * 489.3us per 10k against 25.8 for the same arithmetic over an
   * array — 19x, all of it copying.
   *
   * It stays within the contract `Aggregator` is declared against:
   * Spark's `seqOp` is explicitly "allowed to modify and return their
   * first argument", and `Collect.aggregator` already does exactly
   * this for a java `Collector`. Two rules keep it safe, and both are
   * kept below — `init` allocates a FRESH sketch on every call (so
   * two folds never share one), and `merge` allocates its result (so
   * neither side is disturbed by combining).
   */
  final class HLL(val p: Int, private[Sketch] val registers: Array[Byte]):
    def m: Int = 1 << p

    private[Sketch] def observe(idx: Int, rank: Int): this.type =
      if rank > registers(idx) then registers(idx) = rank.toByte
      this

    private[Sketch] def merged(that: HLL): HLL =
      val out = new Array[Byte](registers.length)
      var i = 0
      while i < out.length do
        val x = registers(i); val y = that.registers(i)
        out(i) = if x >= y then x else y
        i += 1
      HLL(p, out)

    /** the cardinality estimate */
    def estimate: Long =
      val zeros = registers.count(_ == 0)
      var sum = 0.0
      registers.foreach(r => sum += math.pow(2.0, -r.toDouble))
      val alpha = m match
        case 16 => 0.673
        case 32 => 0.697
        case 64 => 0.709
        case _ => 0.7213 / (1.0 + 1.079 / m)
      val e = alpha * m * m / sum
      val corrected =
        if e <= 2.5 * m && zeros > 0 then m * math.log(m.toDouble / zeros)
        else if e > (1L << 32) / 30.0 then
          -(1L << 32).toDouble * math.log1p(-e / (1L << 32).toDouble)
        else e
      math.round(corrected)

  /**
   * Approximate distinct count: 2^p one-byte registers (p = 14 is
   * 16KB for ~0.8% standard error).
   */
  def hyperLogLog[A](p: Int = 14): Aggregator[A, HLL, Long] =
    new Aggregator[A, HLL, Long]:
      // a def, not a captured value: every fold gets its own registers
      def init: HLL = HLL(p, new Array[Byte](1 << p))

      def add(s: HLL, a: A): HLL =
        val h = (MurmurHash3.mix(0x9747b28c, a.##) & 0xFFFFFFFFL) |
          (MurmurHash3.mix(0x85ebca6b, a.##).toLong << 32)
        val idx = (h >>> (64 - p)).toInt
        val w = h << p
        val rank = (if w == 0 then 64 - p else java.lang.Long.numberOfLeadingZeros(w)) + 1
        s.observe(idx, rank)

      def merge(a: HLL, b: HLL): HLL = a.merged(b)
      def present(s: HLL): Long = s.estimate

  // ------------------------------------------------------------------
  // Count-Min: frequencies over-estimated by at most eps * N, with
  // probability 1 - delta; width = e/eps, depth = ln(1/delta)

  /**
   * The counter matrix: elementwise addition is the monoid.
   *
   * `Array[Array[Long]]` and an in-place `add`, for the reason given
   * on `HLL` and with the same two safety rules. Here the old shape
   * was the worst in the file: `rows.zipWithIndex.map(...)` with an
   * `updated` per row allocated a tuple per row, a fresh outer vector,
   * and a copied path through each 2048-element inner one — every
   * element, for what is `depth` array increments. Measured 2282.5us
   * per 10k against 74.3 for the arithmetic alone. 31x.
   *
   * `total` is a var for the same reason; the counters and the count
   * have to move together.
   */
  final class CMS(val width: Int, private[Sketch] val rows: Array[Array[Long]],
                  private var count: Long):
    /** how many elements went in — readable, never writable from outside */
    def total: Long = count
    private def cell(row: Int, a: Any): Int =
      math.floorMod(MurmurHash3.mix(row * 0x9e3779b9 + 1, a.##), width)

    /** the estimated count of a (never an under-estimate) */
    def apply[A](a: A): Long =
      var min = Long.MaxValue
      var i = 0
      while i < rows.length do
        val v = rows(i)(cell(i, a))
        if v < min then min = v
        i += 1
      min

    private[Sketch] def add(a: Any): this.type =
      var i = 0
      while i < rows.length do
        val c = cell(i, a)
        rows(i)(c) += 1
        i += 1
      count += 1
      this

    private[Sketch] def merged(that: CMS): CMS =
      val out = Array.ofDim[Long](rows.length, width)
      var i = 0
      while i < rows.length do
        val r = rows(i); val o = out(i); val t = that.rows(i)
        var j = 0
        while j < width do
          o(j) = r(j) + t(j)
          j += 1
        i += 1
      CMS(width, out, count + that.count)

  /** approximate frequencies: the answer is the queryable sketch */
  def countMin[A](width: Int = 2048, depth: Int = 5): Aggregator[A, CMS, CMS] =
    new Aggregator[A, CMS, CMS]:
      def init: CMS = CMS(width, Array.ofDim[Long](depth, width), 0L)
      def add(s: CMS, a: A): CMS = s.add(a)
      def merge(a: CMS, b: CMS): CMS = a.merged(b)
      def present(s: CMS): CMS = s

  // ------------------------------------------------------------------
  // t-digest: quantiles from clustered centroids; clusters stay small
  // near the tails (the k-scale bound 4 n q (1-q) / delta), so p99 is
  // sharp where it matters

  /**
   * Sorted centroids; merge-then-compress is the monoid.
   *
   * Two flat `Array[Double]`s rather than a `Vector[(Double, Double)]`,
   * which cost a tuple and two boxes per centroid — and, more
   * importantly, a BUFFER, which is where the real cost was.
   *
   * The previous `add` did, per element: `indexWhere` (a linear scan
   * of up to 2*delta centroids), `patch` (a full copy of the vector to
   * insert one point), and `compressed` (which sorts when it runs). At
   * delta = 100 that is several hundred operations and a fresh vector
   * for every single value — 70.0ms per 10k, 7us an element, by far
   * the worst number in this file.
   *
   * The standard shape, and the one Dunning describes: incoming points
   * land in an unsorted buffer at O(1), and compression runs once the
   * buffer fills, merging the sorted centroids with the sorted buffer
   * in one pass. The per-element cost becomes an array store, and the
   * O(k log k) work is amortized over a whole buffer.
   *
   * Mutable in place for the same reason and under the same two rules
   * as `HLL` and `CMS`: `init` allocates, `merge` allocates.
   */
  final class TDigest(val delta: Int,
                      private[Sketch] var means: Array[Double],
                      private[Sketch] var weights: Array[Double],
                      private[Sketch] var size: Int,
                      private[Sketch] var buf: Array[Double],
                      private[Sketch] var buffered: Int,
                      private var n: Long):

    def count: Long = n

    /** the centroids, as the old shape exposed them */
    def centroids: Vector[(Double, Double)] =
      flushed()
      Vector.tabulate(size)(i => (means(i), weights(i)))

    private[Sketch] def add(x: Double): this.type =
      if buffered == buf.length then compress()
      buf(buffered) = x
      buffered += 1
      n += 1
      this

    private[Sketch] def flushed(): Unit = if buffered > 0 then compress()

    /** merge the sorted centroids with the sorted buffer, then apply
     * the k-scale bound in one left-to-right pass */
    private def compress(): Unit =
      val add = java.util.Arrays.copyOf(buf, buffered)
      java.util.Arrays.sort(add)
      val total = size + buffered
      val m = new Array[Double](total)
      val w = new Array[Double](total)
      var i = 0; var j = 0; var k = 0
      while i < size || j < buffered do
        if j >= buffered || (i < size && means(i) <= add(j)) then
          m(k) = means(i); w(k) = weights(i); i += 1
        else
          m(k) = add(j); w(k) = 1.0; j += 1
        k += 1
      buffered = 0
      bound(m, w, total)

    /** the k-scale pass over a sorted, weighted run — the half of
     * compression that is shared with `merged`, which brings its
     * centroids with their own weights rather than with weight 1 */
    private[Sketch] def bound(m: Array[Double], w: Array[Double], total: Int): Unit =
      if total == 0 then
        means = m; weights = w; size = 0
      else
        val om = new Array[Double](total)
        val ow = new Array[Double](total)
        var out = 0
        var cm = m(0); var cw = w(0)
        var cum = 0.0
        var t = 1
        while t < total do
          val q = (cum + cw / 2) / n
          val lim = 4.0 * n * q * (1 - q) / delta
          if cw + w(t) <= math.max(lim, 1.0) then
            val nw = cw + w(t)
            cm = cm + (m(t) - cm) * w(t) / nw
            cw = nw
          else
            om(out) = cm; ow(out) = cw; out += 1
            cum += cw
            cm = m(t); cw = w(t)
          t += 1
        om(out) = cm; ow(out) = cw; out += 1
        means = om; weights = ow; size = out

    /** the q-quantile estimate (0 <= q <= 1) */
    def quantile(q: Double): Double =
      flushed()
      if size == 0 then Double.NaN
      else if size == 1 then means(0)
      else
        val target = q * n
        var cum = 0.0
        var i = 0
        while i < size && cum + weights(i) / 2 < target do
          cum += weights(i)
          i += 1
        if i == 0 then means(0)
        else if i >= size then means(size - 1)
        else
          val w1 = weights(i - 1); val w2 = weights(i)
          val between = (target - (cum - w1 / 2)) / ((w1 + w2) / 2)
          means(i - 1) + (means(i) - means(i - 1)) * between.max(0).min(1)

    /** both sides are sorted runs of WEIGHTED centroids, so this is a
     * two-finger merge and then the same k-scale pass — going through
     * the buffer would give every incoming centroid weight 1 and lose
     * what the other side had already summarized */
    private[Sketch] def merged(that: TDigest): TDigest =
      flushed(); that.flushed()
      val total = size + that.size
      val m = new Array[Double](total)
      val w = new Array[Double](total)
      var i = 0; var j = 0; var k = 0
      while i < size || j < that.size do
        if j >= that.size || (i < size && means(i) <= that.means(j)) then
          m(k) = means(i); w(k) = weights(i); i += 1
        else
          m(k) = that.means(j); w(k) = that.weights(j); j += 1
        k += 1
      val out = TDigest.empty(delta)
      out.n = n + that.n
      out.bound(m, w, total)
      out

  object TDigest:
    /** the buffer is a few times delta: big enough that compression is
     * rare, small enough that a merge pass stays cache-friendly */
    def empty(delta: Int): TDigest =
      TDigest(delta, new Array[Double](0), new Array[Double](0), 0,
        new Array[Double](delta * 5), 0, 0L)

  /** approximate quantiles; delta ~ 100 gives sharp tails */
  def tDigest(delta: Int = 100): Aggregator[Double, TDigest, TDigest] =
    new Aggregator[Double, TDigest, TDigest]:
      def init: TDigest = TDigest.empty(delta)
      def add(s: TDigest, x: Double): TDigest = s.add(x)
      def merge(a: TDigest, b: TDigest): TDigest = a.merged(b)
      def present(s: TDigest): TDigest = { s.flushed(); s }
}

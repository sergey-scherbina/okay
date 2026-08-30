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

  /** the registers: elementwise-max is the monoid */
  final case class HLL(p: Int, registers: Vector[Byte]):
    def m: Int = 1 << p

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
    Aggregator[A, HLL, Long](HLL(p, Vector.fill(1 << p)(0: Byte))) { (s, a) =>
      val h = (MurmurHash3.mix(0x9747b28c, a.##) & 0xFFFFFFFFL) |
        (MurmurHash3.mix(0x85ebca6b, a.##).toLong << 32)
      val idx = (h >>> (64 - p)).toInt
      val w = h << p
      val rank = (if w == 0 then 64 - p else java.lang.Long.numberOfLeadingZeros(w)) + 1
      if rank > s.registers(idx) then
        s.copy(registers = s.registers.updated(idx, rank.toByte))
      else s
    } { (a, b) =>
      a.copy(registers = a.registers.lazyZip(b.registers).map((x, y) => if x >= y then x else y))
    }(_.estimate)

  // ------------------------------------------------------------------
  // Count-Min: frequencies over-estimated by at most eps * N, with
  // probability 1 - delta; width = e/eps, depth = ln(1/delta)

  /** the counter matrix: elementwise addition is the monoid */
  final case class CMS(width: Int, rows: Vector[Vector[Long]], total: Long):
    private def cell(row: Int, a: Any): Int =
      math.floorMod(MurmurHash3.mix(row * 0x9e3779b9 + 1, a.##), width)

    /** the estimated count of a (never an under-estimate) */
    def apply[A](a: A): Long =
      rows.zipWithIndex.map((r, i) => r(cell(i, a))).min

    private[Sketch] def add(a: Any): CMS =
      copy(rows = rows.zipWithIndex.map((r, i) =>
        val c = cell(i, a)
        r.updated(c, r(c) + 1)), total = total + 1)

    private[Sketch] def merged(that: CMS): CMS =
      copy(rows = rows.lazyZip(that.rows).map(_.lazyZip(_).map(_ + _)),
        total = total + that.total)

  /** approximate frequencies: the answer is the queryable sketch */
  def countMin[A](width: Int = 2048, depth: Int = 5): Aggregator[A, CMS, CMS] =
    Aggregator[A, CMS, CMS](CMS(width, Vector.fill(depth)(Vector.fill(width)(0L)), 0L))(
      (s, a) => s.add(a))((a, b) => a.merged(b))(identity)

  // ------------------------------------------------------------------
  // t-digest: quantiles from clustered centroids; clusters stay small
  // near the tails (the k-scale bound 4 n q (1-q) / delta), so p99 is
  // sharp where it matters

  /** sorted centroids (mean, weight); merge-then-compress is the monoid */
  final case class TDigest(delta: Int, centroids: Vector[(Double, Double)], count: Long):

    /** the q-quantile estimate (0 <= q <= 1) */
    def quantile(q: Double): Double =
      if centroids.isEmpty then Double.NaN
      else if centroids.length == 1 then centroids.head._1
      else
        val target = q * count
        var cum = 0.0
        var i = 0
        while i < centroids.length && cum + centroids(i)._2 / 2 < target do
          cum += centroids(i)._2
          i += 1
        if i == 0 then centroids.head._1
        else if i >= centroids.length then centroids.last._1
        else
          val (m1, w1) = centroids(i - 1)
          val (m2, w2) = centroids(i)
          val between = (target - (cum - w1 / 2)) / ((w1 + w2) / 2)
          m1 + (m2 - m1) * between.max(0).min(1)

    private[Sketch] def compressed: TDigest =
      if centroids.length <= 2 * delta then this
      else
        val sorted = centroids.sortBy(_._1)
        val out = Vector.newBuilder[(Double, Double)]
        var (cm, cw) = sorted.head
        var cum = 0.0
        for (m, w) <- sorted.tail do
          val q = (cum + cw / 2) / count
          val bound = 4.0 * count * q * (1 - q) / delta
          if cw + w <= bound.max(1.0) then
            val nw = cw + w
            cm = cm + (m - cm) * w / nw
            cw = nw
          else
            out += ((cm, cw))
            cum += cw
            cm = m
            cw = w
        out += ((cm, cw))
        copy(centroids = out.result())

  /** approximate quantiles; delta ~ 100 gives sharp tails */
  def tDigest(delta: Int = 100): Aggregator[Double, TDigest, TDigest] =
    Aggregator[Double, TDigest, TDigest](TDigest(delta, Vector.empty, 0L)) { (s, x) =>
      val i = s.centroids.indexWhere(_._1 >= x) match
        case -1 => s.centroids.length
        case j => j
      s.copy(centroids = s.centroids.patch(i, Seq((x, 1.0)), 0), count = s.count + 1).compressed
    } { (a, b) =>
      a.copy(centroids = (a.centroids ++ b.centroids).sortBy(_._1),
        count = a.count + b.count).compressed
    }(identity)
}

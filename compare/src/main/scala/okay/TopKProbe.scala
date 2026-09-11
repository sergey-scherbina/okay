package okay

/**
 * What the top-k selection ALLOCATES, exactly.
 *
 * JMH's `gc.alloc.rate.norm` is an average of a sampled rate, and on
 * a loaded box it came back ±147 KB on a 250 KB number — measured
 * 2026-09-10, with the SAME implementation on both sides of the pair
 * reading 342 073 and 251 778. An instrument whose bars are half its
 * reading cannot price this change.
 *
 * The allocation itself is deterministic, so it does not need an
 * average at all: `getThreadAllocatedBytes` around one call, on one
 * thread, is the exact number of bytes that call allocated. The
 * probe's own overhead is measured and subtracted, and every lane is
 * run to steady state first so the JIT is not being priced.
 *
 * Run: sbt 'compare/runMain okay.TopKProbe'
 */
object TopKProbe:

  private val bean = java.lang.management.ManagementFactory.getThreadMXBean
    .asInstanceOf[com.sun.management.ThreadMXBean]

  private def allocated(body: => Any): Long =
    val id = Thread.currentThread().threadId()
    val before = bean.getThreadAllocatedBytes(id)
    val r = body
    val after = bean.getThreadAllocatedBytes(id)
    if r == null then 0L else after - before

  /** the implementation `Aggregator.topK` had before this lane */
  private def topKSorting[A](k: Int)(using O: Ordering[A]): Aggregator[A, List[A], List[A]] =
    def keep(xs: List[A]) = xs.sorted(using O.reverse).take(k)
    Aggregator(List.empty[A])((s, a: A) => keep(a :: s))((a, b) => keep(a ++ b))(identity)

  def main(args: Array[String]): Unit =
    val n = if args.length > 0 then args(0).toInt else 10000
    val k = if args.length > 1 then args(1).toInt else 8

    // a deterministic corpus: the scores are what the selection sees
    val rnd = new java.util.Random(42L)
    val scored: Vector[Double] = Vector.fill(n)(rnd.nextDouble())

    given Ordering[Double] = Ordering.Double.TotalOrdering

    def select(agg: Aggregator[Double, List[Double], List[Double]]): List[Double] =
      agg.present(scored.foldLeft(agg.init)(agg.add))

    val old_ = topKSorting[Double](k)
    val now = Aggregator.topK[Double](k)

    // the two must agree on the answer before either is priced
    val a = select(old_); val b = select(now)
    require(a == b, s"the selections differ: $a vs $b")

    // steady state
    var warm: List[Double] = Nil
    for _ <- 0 until 50 do { warm = select(old_); warm = select(now) }
    require(warm.nonEmpty || n == 0)

    val zero = allocated(())
    val oldBytes = allocated(select(old_)) - zero
    val nowBytes = allocated(select(now)) - zero

    def t(body: => Any): Double =
      var sink: Any = null
      val t0 = System.nanoTime(); var i = 0
      while i < 20 do { sink = body; i += 1 }
      val dt = (System.nanoTime() - t0) / 20.0 / 1000.0
      if sink == null then Double.NaN else dt

    println(f"corpus $n%,d elements, k = $k  — Double (the Ordering boxes)")
    println(f"  sort every element : $oldBytes%,12d B  ${t(select(old_))}%8.1f us")
    println(f"  guarded            : $nowBytes%,12d B  ${t(select(now))}%8.1f us")
    if oldBytes > 0 then
      println(f"  allocation         : ${100.0 * (nowBytes - oldBytes) / oldBytes}%+.1f%%")

    // the shape the RAG store actually folds: a record with a score,
    // so the Ordering compares a field and nothing is boxed. What is
    // left is the selection's own allocation and nothing else.
    val rows: Vector[Row] = scored.zipWithIndex.map((d, i) => Row(i, d.toFloat))
    given Ordering[Row] = Ordering.by(_.score)
    val rowsOld = topKSorting[Row](k)
    val rowsNow = Aggregator.topK[Row](k)
    def selectRows(agg: Aggregator[Row, List[Row], List[Row]]): List[Row] =
      agg.present(rows.foldLeft(agg.init)(agg.add))
    require(selectRows(rowsOld).map(_.score) == selectRows(rowsNow).map(_.score))
    var warmR: List[Row] = Nil
    for _ <- 0 until 50 do { warmR = selectRows(rowsOld); warmR = selectRows(rowsNow) }
    require(warmR.nonEmpty || n == 0)
    val rowsOldBytes = allocated(selectRows(rowsOld)) - zero
    val rowsNowBytes = allocated(selectRows(rowsNow)) - zero
    println(f"corpus $n%,d records, k = $k  — a scored record (nothing boxed)")
    println(f"  sort every element : $rowsOldBytes%,12d B  ${t(selectRows(rowsOld))}%8.1f us")
    println(f"  guarded            : $rowsNowBytes%,12d B  ${t(selectRows(rowsNow))}%8.1f us")
    if rowsOldBytes > 0 then
      println(f"  allocation         : ${100.0 * (rowsNowBytes - rowsOldBytes) / rowsOldBytes}%+.1f%%")

  final case class Row(id: Int, score: Float)

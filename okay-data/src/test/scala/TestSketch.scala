package okay

/** Approximate aggregators: stated error bounds, associative merges. */
class TestSketch extends munit.FunSuite {

  def lcg(seed: Long): LazyList[Long] =
    LazyList.iterate(seed)(x => x * 6364136223846793005L + 1442695040888963407L).tail

  test("HyperLogLog: distinct count within a few percent") {
    val n = 20000
    val xs = lcg(42).take(n).toList
    val est = Sketch.hyperLogLog[Long](12).run(xs)   // 2^12: ~1.6% expected error
    assert(math.abs(est - n.toDouble) / n < 0.05, s"estimate $est for $n")
    // duplicates do not inflate it
    val est2 = Sketch.hyperLogLog[Long](12).run(xs ++ xs.take(n / 2))
    assert(math.abs(est2 - n.toDouble) / n < 0.05, s"estimate $est2 for $n with dupes")
  }

  test("HyperLogLog: split-merge equals the whole") {
    val xs = lcg(7).take(5000).toList
    val h = Sketch.hyperLogLog[Long](12)
    val (l, r) = xs.splitAt(2000)
    assertEquals(
      h.present(h.merge(l.foldLeft(h.init)(h.add), r.foldLeft(h.init)(h.add))),
      h.run(xs))
  }

  test("Count-Min: never under-estimates, sharp for heavy hitters") {
    val heavy = List.fill(1000)("popular")
    val noise = lcg(3).take(5000).map("n" + _).toList
    val cms = Sketch.countMin[String]().run(heavy ++ noise)
    assert(cms("popular") >= 1000)
    assert(cms("popular") <= 1100, s"heavy hitter ${cms("popular")}")
    assert(cms("absent") <= 30)   // eps * N slack only
  }

  test("Count-Min: split-merge equals the whole for queries") {
    val xs = (1 to 3000).map(i => "k" + i % 100).toList
    val c = Sketch.countMin[String]()
    val (l, r) = xs.splitAt(1234)
    val merged = c.merge(l.foldLeft(c.init)(c.add), r.foldLeft(c.init)(c.add))
    val whole = xs.foldLeft(c.init)(c.add)
    assertEquals(merged("k7"), whole("k7"))
    assertEquals(merged.total, whole.total)
  }

  test("t-digest: median and p99 within a percent of truth on uniform data") {
    val n = 10000
    val xs = lcg(11).take(n).map(x => math.floorMod(x, 10000).toDouble).toList
    val td = Sketch.tDigest().run(xs)
    val sorted = xs.sorted
    def trueQ(q: Double) = sorted(((n - 1) * q).toInt)
    assert(math.abs(td.quantile(0.5) - trueQ(0.5)) / 10000 < 0.01,
      s"median ${td.quantile(0.5)} vs ${trueQ(0.5)}")
    assert(math.abs(td.quantile(0.99) - trueQ(0.99)) / 10000 < 0.01,
      s"p99 ${td.quantile(0.99)} vs ${trueQ(0.99)}")
    assert(td.centroids.length < 500, s"compressed to ${td.centroids.length}")
  }

  test("t-digest: merged halves agree with the whole within tolerance") {
    val xs = lcg(5).take(4000).map(x => math.floorMod(x, 1000).toDouble).toList
    val t = Sketch.tDigest()
    val (l, r) = xs.splitAt(1700)
    val merged = t.merge(l.foldLeft(t.init)(t.add), r.foldLeft(t.init)(t.add))
    val whole = xs.foldLeft(t.init)(t.add)
    assert(math.abs(merged.quantile(0.5) - whole.quantile(0.5)) / 1000 < 0.02)
  }

  test("sketches zip with exact aggregators: one pass, both kinds") {
    val xs = lcg(9).take(1000).map(x => math.floorMod(x, 100).toDouble).toList
    val agg = Aggregator.mean[Double].zip(Sketch.tDigest())
    val (mean, td) = agg.run(xs)
    assert(math.abs(mean - xs.sum / xs.size) < 1e-9)
    assert(math.abs(td.quantile(0.5) - 50) < 10)
  }
}

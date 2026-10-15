package okay.std

import okay.{!, +, %, effect, pure}

/** msplit and what derives from it are LINEAR in the work of the search (LogicT's msplit): the rest of a split is
 * the remaining search, resumed — never re-run from an earlier point, no layer of reflection per answer */
class TestLogicScaling extends munit.FunSuite {

  type R = Choose

  /** the benchmark's tree (LogicBenchmark.observe_tree): 2^d leaves */
  def tree(d: Int): Int ! R =
    if d == 0 then pure(1) else effect[Choose, Int](Choose(Seq(0, 1))).flatMap(x => tree(d - 1).map(_ + x))

  def observeAll(d: Int): Seq[Int] = Logic.observe[Int, R](1 << d)(tree(d)).run

  /** the best of a few runs, in nanoseconds */
  def time(d: Int): Long =
    (1 to 7).map { _ =>
      val t0 = System.nanoTime()
      val r = observeAll(d)
      val t = System.nanoTime() - t0
      assertEquals(r.size, 1 << d)
      t
    }.min

  test("observe gives runChoice's answers, in order, at depths 4..10") {
    for d <- 4 to 10 do
      assertEquals(observeAll(d), runChoice[Int, R](tree(d)).run, s"depth $d")
  }

  test("observe over the tree grows with the leaves: depth d+2 no more than ~6x depth d") {
    for d <- 4 to 12 do time(d) // warm up
    val ts = (6 to 12).map(d => d -> time(d)).toMap
    println(ts.toSeq.sorted.map((d, t) => f"depth $d: ${t / 1e6}%.2f ms").mkString("observe_tree: ", ", ", ""))
    for d <- Seq(8, 10) do
      val ratio = ts(d + 2).toDouble / ts(d)
      assert(ratio <= 6.0, f"depth ${d + 2} took ${ratio}%.1fx depth $d (${ts(d + 2)} ns vs ${ts(d)} ns)")
  }

  test("the derived words keep their meaning") {
    def amb[A](as: A*): A ! R = effect[Choose, A](Choose(as))
    def fail[A]: A ! R = effect[Choose, A](Choose(Seq.empty))
    def nats: Long ! R = effect[Choose, Long](Choose(LazyList.from(0).map(_.toLong)))

    val Some((a, rest)) = Logic.msplit[Int, R](amb(1, 2, 3)).run: @unchecked
    assertEquals(a, 1)
    assertEquals(runChoice[Int, R](rest).run, Seq(2, 3))
    assertEquals(Logic.msplit[Int, R](fail[Int]).run, None)
    // a rest split again, then run whole
    val Some((b, rest2)) = Logic.msplit[Int, R](rest).run: @unchecked
    assertEquals(b, 2)
    assertEquals(runChoice[Int, R](rest2).run, Seq(3))
    assertEquals(runChoice[Int, R](rest).run, Seq(2, 3)) // a rest is a value: run again, the same

    assertEquals(runChoice[Int, R](Logic.cut[Int, R](amb(1, 2, 3))).run, Seq(1))
    assertEquals(runChoice[Int, R](Logic.cut[Int, R](fail[Int])).run, Seq.empty)
    assertEquals(runChoice[Int, R](Logic.ifte[Int, Int, R](amb(1, 2))(x => pure(x * 10))(pure(-1))).run, Seq(10, 20))
    assertEquals(runChoice[Int, R](Logic.ifte[Int, Int, R](fail[Int])(x => pure(x))(pure(-1))).run, Seq(-1))
    assertEquals(runChoice[Unit, R](Logic.gnot[Int, R](fail[Int])).run, Seq(()))
    assertEquals(runChoice[Unit, R](Logic.gnot[Int, R](amb(1))).run, Seq.empty)
    val evens = nats.map(_ * 2)
    val odds = nats.map(_ * 2 + 1)
    assertEquals(Logic.observe[Long, R](6)(Logic.interleave[Long, R](evens, odds)).run, Seq(0L, 1L, 2L, 3L, 4L, 5L))
    val witness = Logic.fairBind[Long, Long, R](nats)(x => if x * x == 16 then pure(x) else fail[Long])
    assertEquals(Logic.observe[Long, R](1)(witness).run, Seq(4L))
    assertEquals(Logic.observe[Long, R](5)(nats).run, Seq(0L, 1L, 2L, 3L, 4L))
    // a deep tree interleaved, its rest split again and again
    assertEquals(Logic.observe[Int, R](1 << 9)(Logic.interleave[Int, R](tree(8), tree(8))).run.sorted,
      (runChoice[Int, R](tree(8)).run ++ runChoice[Int, R](tree(8)).run).sorted)
  }

  test("effects forward through the search: a Writer on the crossed path, once") {
    type F = Writer % String
    val prog: Int ! Choose + F =
      effect[Choose, Int](Choose(Seq(1, 2, 3))).at[Choose + F].flatMap(x =>
        effect[F, Unit](Writer.Say(s"seen $x")).at[Choose + F].map(_ => x * 10))
    val (told, found) = Writer.run[String, Seq[Int], F](Logic.observe[Int, Choose + F](2)(prog)).run
    assertEquals(found, Seq(10, 20))
    assertEquals(told, Seq("seen 1", "seen 2"))
  }
}

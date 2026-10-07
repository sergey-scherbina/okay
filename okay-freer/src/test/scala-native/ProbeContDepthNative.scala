package okay.freer
/**
 * cont-leaf-by-platform on Native: contAnswer's body `k(x + 1) + 1` at depth, the macro's lazy leaf against
 * the strict one (`Cps.shiftLeaf`), timed by hand (no JMH on Native). Runs only with OKAY_PROBE_CONT_DEPTH
 * set, built in release mode by whoever runs it: a debug build's timings say nothing.
 */
class ProbeContDepthNative extends munit.FunSuite:

  override def munitTimeout = scala.concurrent.duration.Duration(30, "min")

  private def lazyLeaf(n: Int): Int =
    Cps.reset((1 to n).foldLeft(Cps.Pure[Int, Int](0): Int />> Int)((m, _) => m.flatMap(x => Cps.shift[Int, Int, Int](k => k(x + 1) + 1))))

  private def strictLeaf(n: Int): Int =
    Cps.reset((1 to n).foldLeft(Cps.Pure[Int, Int](0): Int />> Int)((m, _) => m.flatMap(x => Cps.shiftLeaf[Int, Int, Int](k => k(x + 1) + 1))))

  /** the median of `reps` timed runs, in microseconds, after `warm` untimed ones */
  private def time(warm: Int, reps: Int)(f: () => Int): Double =
    for _ <- 1 to warm do assertEquals(f() > 0, true)
    val ts = (1 to reps).map { _ =>
      val t0 = System.nanoTime()
      val r = f()
      val t = System.nanoTime() - t0
      assert(r > 0)
      t / 1000.0
    }.sorted
    ts(reps / 2)

  test("lazy against strict at depth") {
    assume(sys.env.contains("OKAY_PROBE_CONT_DEPTH"), "a probe: set OKAY_PROBE_CONT_DEPTH and build releaseFast")
    for (n, warm, reps) <- List((1000, 2000, 2001), (100000, 20, 21), (1000000, 3, 7)) do
      for round <- 1 to 2 do
        val l = time(warm, reps)(() => lazyLeaf(n))
        val s = time(warm, reps)(() => strictLeaf(n))
        println(f"PROBE depth=$n%d round=$round%d lazy=$l%.1f us strict=$s%.1f us ratio=${s / l}%.2f")
  }

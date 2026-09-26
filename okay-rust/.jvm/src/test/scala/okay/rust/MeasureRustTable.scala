package okay.rust

import okay.foreign.{ForeignEval, ForeignWorker, PyFrame, PyValue, WireLink}

/**
 * A TABLE CALL INTO IN-PROCESS RUST, TWO ROADS (foreign-arrow-ffm): one
 * 1M-row Int64 column through the conformance crate's `scale` over FFM, as
 * the Arrow C Data Interface and as JSON (the same library, its table road
 * hidden), the arms alternating. The JSON road's breakdown — the call WAS
 * its codec — is specs/foreign-one.md Decision 21. Printed, not asserted; Live.
 */
class MeasureRustTable extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !RustInProcess.available

  test("MEASURE: the same call as C Data and as JSON, arms alternating") {
    val n = 1_000_000
    val frame = PyFrame(Vector("x" -> Vector.tabulate(n)(i => PyValue.I64(i.toLong))))
    val cdata = ForeignWorker.inProcess(RustInProcess.dylib)
    // the same library, its table road hidden: what every table took before
    val link = InProcessLinks.ffm(NativeLib.load(RustInProcess.dylib)).fold(e => fail(e), identity)
    val json = ForeignWorker.over(new WireLink:
      def hello() = link.hello()
      def roundTrip(line: String) = link.roundTrip(line)
      def exchange(m: Array[Byte]) = link.exchange(m)
      def close() = link.close()
      override def inProcess = true, "the library, JSON only")
    try
      def call(w: ForeignWorker): Double =
        val t0 = System.nanoTime()
        assert(w.handler.handle(ForeignEval.Frame("scale", frame, Vector(PyValue.I64(3)))).isRight)
        (System.nanoTime() - t0) / 1e6
      (1 to 3).foreach { _ => val _ = (call(cdata), call(json)) }
      val rounds = (1 to 7).map(_ => (call(cdata), call(json)))
      val c = rounds.map(_._1).min
      val j = rounds.map(_._2).min
      println(f"MEASURE rust-ffm table 1M, min of 7 alternating: C Data $c%.1f ms, JSON $j%.1f ms, ${j / c}%.1fx " +
        f"(load ${java.lang.management.ManagementFactory.getOperatingSystemMXBean.getSystemLoadAverage}%.1f)")
    finally { cdata.close(); json.close() }
  }

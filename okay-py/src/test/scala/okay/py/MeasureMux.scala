package okay.py

/**
 * THE MUX READER'S PRICE (foreign-mux-duplex): the same Go worker binary,
 * 2 000 calls one after another, multiplexed (a reader thread, answers by
 * id) beside the one-exchange-at-a-time wire (the same pipes, their duplex
 * hidden), arms alternating. Printed, not asserted; Live.
 */
class MeasureMux extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !GoWorkerBinary.available

  test("MEASURE: 2 000 sequential calls, muxed and not, alternating") {
    val muxed = ForeignWorker.speaking(Seq(GoWorkerBinary.binary.toString))
    val proc = ProcessBuilder(GoWorkerBinary.binary.toString).start()
    val pipes = WireLink.pipes(proc)
    val plain = ForeignWorker.over(new WireLink:
      def hello() = pipes.hello()
      def roundTrip(line: String) = pipes.roundTrip(line)
      def exchange(m: Array[Byte]) = pipes.exchange(m)
      def close() = pipes.close(), "the Go worker, one exchange at a time")
    try
      assert(muxed.muxed && !plain.muxed)
      def round(w: ForeignWorker): Double =
        val t0 = System.nanoTime()
        var i = 0
        while i < 2000 do
          assert(w.handler.handle(ForeignEval.Call("describe", Vector(PyValue.I64(i), PyValue.I64(1)))).isRight)
          i += 1
        (System.nanoTime() - t0) / 1e6
      (1 to 2).foreach { _ => val _ = (round(muxed), round(plain)) }
      val rounds = (1 to 7).map(_ => (round(muxed), round(plain)))
      val (m, p) = (rounds.map(_._1).min, rounds.map(_._2).min)
      println(f"MEASURE mux 2000 calls, min of 7 alternating: muxed $m%.1f ms, one at a time $p%.1f ms, ${m / p}%.2fx " +
        f"(load ${java.lang.management.ManagementFactory.getOperatingSystemMXBean.getSystemLoadAverage}%.1f)")
    finally { muxed.close(); plain.close() }
  }

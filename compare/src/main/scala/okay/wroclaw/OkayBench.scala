package okay.wroclaw

/** okay's own lanes, and the floor every other row is read against */
object OkayBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(
      Bench.measure(ask, "the floor (a while loop)", 1, "no stream machinery")(
        OkayLane.floor(ask.feed)),
      Bench.measure(ask, "okay, 1 thread (Chunks)", 1, "Aggregator.summary")(
        OkayLane.run(ask.feed)),
      Bench.measure(ask, "okay, 1 thread, count zip sum zip max", 1,
        "what composability costs")(OkayLane.runZip(ask.feed)),
      Bench.measure(ask, "okay, 1 thread, packed-key windows", 1, "the pre-core operator")(
        OkayLane.packed(ask.feed)),
      // the 2x2 of §20's "why one core loses": one lookup or two,
      // against a value accumulator or a mutable cell
      Bench.measure(ask, "okay, 1 thread, flat summary aggregator", 1,
        "Aggregator.summary: one object, four longs")(OkayLane.runSummary(ask.feed)),
      Bench.measure(ask, "okay, 1 thread, mutable-cell aggregator", 1,
        "the general operator, no tuples")(OkayLane.runCells(ask.feed)),
      Bench.measure(ask, "okay, 1 thread, packed + mutable cell", 1,
        "one lookup, no tuples, still evicting")(OkayLane.packedCells(ask.feed)),
      Bench.measure(ask, "okay, 2 fibres (merge)", 2)(OkayLane.parallel(ask.feed, 2)),
      Bench.measure(ask, "okay, 4 fibres (merge)", 4)(OkayLane.parallel(ask.feed, 4)),
      Bench.measure(ask, "okay, 8 fibres (merge)", 8)(OkayLane.parallel(ask.feed, 8)),
    )
  }
}

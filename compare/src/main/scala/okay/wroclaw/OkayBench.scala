package okay.wroclaw

/** okay's own lanes, and the floor every other row is read against */
object OkayBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(
      Bench.measure(ask, "the floor (a while loop)", 1, "no stream machinery")(
        OkayLane.floor(ask.feed)),
      Bench.measure(ask, "okay, 1 thread (Chunks)", 1)(OkayLane.run(ask.feed)),
      Bench.measure(ask, "okay, 1 thread, packed-key windows", 1, "the pre-core operator")(
        OkayLane.packed(ask.feed)),
      Bench.measure(ask, "okay, 2 fibres (merge)", 2)(OkayLane.parallel(ask.feed, 2)),
      Bench.measure(ask, "okay, 4 fibres (merge)", 4)(OkayLane.parallel(ask.feed, 4)),
      Bench.measure(ask, "okay, 8 fibres (merge)", 8)(OkayLane.parallel(ask.feed, 8)),
    )
  }
}

package okay.flink.wroclaw

import okay.wroclaw.Bench

/** Flink's lanes (docs/benchmarks.md §20) */
object FlinkBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(
      Bench.measure(ask, "flink, parallelism 1", 1)(FlinkLane.run(ask.feed, 1)),
      Bench.measure(ask, "flink, parallelism 4", 4)(FlinkLane.run(ask.feed, 4)),
      Bench.measure(ask, "flink, parallelism 4 + checkpoints 5 s", 4, "the guarantee, priced")(
        FlinkLane.run(ask.feed, 4, checkpointMs = 5000L)),
      Bench.measure(ask, "flink, parallelism 4, object reuse off", 4)(
        FlinkLane.run(ask.feed, 4, objectReuse = false)),
    )
  }
}

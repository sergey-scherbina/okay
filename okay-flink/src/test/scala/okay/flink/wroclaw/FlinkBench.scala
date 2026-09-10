package okay.flink.wroclaw

import okay.wroclaw.Bench

/** Flink's lanes (docs/benchmarks.md §20) */
object FlinkBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(
      // the same 1/2/4/8 axis every other lane carries, so the cores
      // column compares like with like rather than being reconstructed
      Bench.measure(ask, "flink, parallelism 1", 1)(FlinkLane.run(ask.feed, 1)),
      Bench.measure(ask, "flink, parallelism 2", 2)(FlinkLane.run(ask.feed, 2)),
      Bench.measure(ask, "flink, parallelism 4", 4)(FlinkLane.run(ask.feed, 4)),
      Bench.measure(ask, "flink, parallelism 8", 8)(FlinkLane.run(ask.feed, 8)),
      Bench.measure(ask, "flink, parallelism 4 + checkpoints 5 s", 4, "the guarantee, priced")(
        FlinkLane.run(ask.feed, 4, checkpointMs = 5000L)),
      Bench.measure(ask, "flink, parallelism 4, object reuse off", 4)(
        FlinkLane.run(ask.feed, 4, objectReuse = false)),
    )
  }
}

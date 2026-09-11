package okay.kyo.wroclaw

import okay.wroclaw.Bench

/** kyo on §20's job, in its own vocabulary and at four widths */
object KyoBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(
      Bench.measure(ask, "kyo streams, 1 core", 1, "evaluated where it is built")(
        KyoLane.run(ask.feed)),
      Bench.measure(ask, "kyo streams, 2 cores (Async.parallel)", 2, "kyo's own scheduler")(
        KyoLane.parallel(ask.feed, 2)),
      Bench.measure(ask, "kyo streams, 4 cores (Async.parallel)", 4, "kyo's own scheduler")(
        KyoLane.parallel(ask.feed, 4)),
      Bench.measure(ask, "kyo streams, 8 cores (Async.parallel)", 8, "kyo's own scheduler")(
        KyoLane.parallel(ask.feed, 8)),
    )
  }
}

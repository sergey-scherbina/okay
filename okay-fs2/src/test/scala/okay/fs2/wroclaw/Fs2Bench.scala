package okay.fs2.wroclaw

import okay.wroclaw.Bench

/** fs2 on §20's job, in its own vocabulary and at four widths */
object Fs2Bench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(
      Bench.measure(ask, "fs2, 1 core (pure)", 1, "no runtime: Stream[Pure, *]")(
        Fs2Lane.run(ask.feed)),
      Bench.measure(ask, "fs2, 2 cores (parEvalMap)", 2, "on IO: parallelism needs Concurrent")(
        Fs2Lane.parallel(ask.feed, 2)),
      Bench.measure(ask, "fs2, 4 cores (parEvalMap)", 4, "on IO: parallelism needs Concurrent")(
        Fs2Lane.parallel(ask.feed, 4)),
      Bench.measure(ask, "fs2, 8 cores (parEvalMap)", 8, "on IO: parallelism needs Concurrent")(
        Fs2Lane.parallel(ask.feed, 8)),
    )
  }
}

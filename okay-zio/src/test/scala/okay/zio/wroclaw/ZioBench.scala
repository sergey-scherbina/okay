package okay.zio.wroclaw

import okay.wroclaw.Bench

/** zio-streams on §20's job, in its own vocabulary and at four widths */
object ZioBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(
      Bench.measure(ask, "zio-streams, 1 core", 1, "no pure interpreter: the runtime is paid")(
        ZioLane.run(ask.feed)),
      Bench.measure(ask, "zio-streams, 2 cores (foreachPar)", 2, "withParallelism(2)")(
        ZioLane.parallel(ask.feed, 2)),
      Bench.measure(ask, "zio-streams, 4 cores (foreachPar)", 4, "withParallelism(4)")(
        ZioLane.parallel(ask.feed, 4)),
      Bench.measure(ask, "zio-streams, 8 cores (foreachPar)", 8, "withParallelism(8)")(
        ZioLane.parallel(ask.feed, 8)),
    )
  }
}

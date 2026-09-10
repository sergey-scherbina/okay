package okay.zio.wroclaw

import okay.wroclaw.Bench

/** zio-streams on §20's job — the library carries okay's window
 * operator, because it has none of its own */
object ZioBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(Bench.measure(ask, "zio-streams, our window operator", 1,
      "no event-time window of its own")(ZioLane.run(ask.feed)))
  }
}

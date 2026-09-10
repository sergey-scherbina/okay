package okay.fs2.wroclaw

import okay.wroclaw.Bench

/** fs2 (pure) on §20's job — the library carries okay's window
 * operator, because it has none of its own */
object Fs2Bench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(Bench.measure(ask, "fs2 (pure), our window operator", 1,
      "no event-time window of its own")(Fs2Lane.run(ask.feed)))
  }
}

package okay.kyo.wroclaw

import okay.wroclaw.Bench

/** kyo streams on §20's job — the library carries okay's window
 * operator, because it has none of its own */
object KyoBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    Seq(Bench.measure(ask, "kyo streams, our window operator", 1,
      "no event-time window of its own")(KyoLane.run(ask.feed)))
  }
}

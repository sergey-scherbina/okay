package okay.wroclaw

/** the plain JVM on §20's job: loops and threads, no library at all */
object JvmBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    // WHERE THE PARALLEL ROWS STOP SCALING, measured rather than
    // reasoned: the fold across 8 threads, then the single-threaded
    // reduction of what they built, then how many pane cells that was.
    val (foldMs, combineMs, cells) = JvmLane.split(ask.feed, 8)
    println(f"NOTE\tplain JVM, 8 threads: fold $foldMs%d ms, combine $combineMs%d ms, " +
      f"$cells%,d pane cells held (okay's operator holds ~4 918 panes at once)")
    Seq(
      Bench.measure(ask, "plain JVM, while loop", 1, "no library at all")(
        JvmLane.loop(ask.feed)),
      Bench.measure(ask, "plain JVM, 2 threads", 2, "slices joined by hand")(
        JvmLane.threads(ask.feed, 2)),
      Bench.measure(ask, "plain JVM, 4 threads", 4, "slices joined by hand")(
        JvmLane.threads(ask.feed, 4)),
      Bench.measure(ask, "plain JVM, 8 threads", 8, "slices joined by hand")(
        JvmLane.threads(ask.feed, 8)),
    )
  }
}

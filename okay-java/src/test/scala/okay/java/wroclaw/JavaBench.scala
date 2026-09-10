package okay.java.wroclaw

import okay.wroclaw.Bench

/** the JDK's two roads through okay-java's interop (docs/benchmarks.md §20) */
object JavaBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    // THE groupingBy ROADS DO NOT FIT A LARGE FEED, and that is their
    // finding rather than an accident: with no event time every pane of
    // the run stays live, and at 2.4M events the parallel road dies
    // with an OutOfMemoryError on an 8 GB heap. The main says so and
    // moves on instead of taking the whole run down with it.
    val fits = ask.feed.events.length <= 1200000
    if !fits then
      println("SKIP\tjava.util.stream, groupingBy — OutOfMemoryError at this size (no eviction)")
    Seq(
      Bench.measure(ask, "java.util.stream, windowed collector", 1,
        "okay.java.Windowed: panes evicted on the watermark")(JavaLane.windowed(ask.feed)),
    ) ++ (if !fits then Seq.empty else Seq(
      Bench.measure(ask, "java.util.stream, groupingBy", 1,
        "no event time: the state is the history")(JavaLane.run(ask.feed, parallel = false)),
      Bench.measure(ask, "java.util.stream, groupingBy, parallel", 4,
        "a map per split, merged pairwise")(JavaLane.run(ask.feed, parallel = true)),
    ))
  }
}

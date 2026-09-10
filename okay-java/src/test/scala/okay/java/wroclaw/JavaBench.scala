package okay.java.wroclaw

import okay.wroclaw.Bench

/**
 * THE JDK ON §20's JOB. Two kinds of row, and the difference between
 * them is the point of the section:
 *
 *   - `java.util.stream` on ITS OWN — `Arrays.stream`, the platform's
 *     `filter`/`map`, and a hand-written fold into maps that never
 *     evict, sequential and then in a `ForkJoinPool` of 2, 4 and 8.
 *     This is what the JDK gives a user who needs event-time windows:
 *     the windows, but not the event time.
 *   - the same job with OUR window operator inside a JDK `Collector`
 *     (`okay.java.Windowed`), which evicts on the watermark — the
 *     interop's answer to exactly that gap.
 *
 * The `groupingBy` rows measure a third thing: the shape a JDK user
 * reaches for first, materialising a group per key. They are what
 * makes the memory column worth reading, and they do not fit a large
 * feed at all.
 */
object JavaBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    // THE groupingBy ROADS DO NOT FIT A LARGE FEED, and that is their
    // finding rather than an accident: the bunching stage materialises
    // every event of every key, and at 2.4M events the parallel road
    // dies with an OutOfMemoryError on an 8 GB heap. The main says so
    // and moves on instead of taking the whole run down with it.
    val fits = ask.feed.events.length <= 1200000
    if !fits then
      println("SKIP\tjava.util.stream, groupingBy — OutOfMemoryError at this size (a group per key, held)")
    Seq(
      Bench.measure(ask, "java.util.stream, 1 core", 1,
        "no event time: the state is the history")(JavaLane.stream(ask.feed)),
      Bench.measure(ask, "java.util.stream, 2 cores", 2,
        "mutable reduction in a ForkJoinPool(2)")(JavaLane.parallel(ask.feed, 2)),
      Bench.measure(ask, "java.util.stream, 4 cores", 4,
        "mutable reduction in a ForkJoinPool(4)")(JavaLane.parallel(ask.feed, 4)),
      Bench.measure(ask, "java.util.stream, 8 cores", 8,
        "mutable reduction in a ForkJoinPool(8)")(JavaLane.parallel(ask.feed, 8)),
      Bench.measure(ask, "java.util.stream, windowed collector", 1,
        "okay.java.Windowed: panes evicted on the watermark")(JavaLane.windowed(ask.feed)),
    ) ++ (if !fits then Seq.empty else Seq(
      Bench.measure(ask, "java.util.stream, groupingBy", 1,
        "three passes, a group per key")(JavaLane.run(ask.feed, parallel = false)),
      Bench.measure(ask, "java.util.stream, groupingBy, parallel", 4,
        "a map per split, merged pairwise")(JavaLane.run(ask.feed, parallel = true)),
    ))
  }
}

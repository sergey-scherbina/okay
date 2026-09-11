package okay.cluster

import okay.{Aggregator, Pane}
import okay.given

/**
 * NO WINDOW IS PRESENTED TWICE (specs/dataflow.md, stage 6a).
 *
 * The checksums in `TestStream` catch a split pane only because they
 * count panes as well as summing them. This says the invariant
 * directly, and it is the one an epoch boundary breaks: a pane
 * retired by the coordinator before its last contributor has handed
 * its half over comes out as two panes with the same window and key,
 * whose values sum to the right answer.
 *
 * It was three separate mistakes that produced exactly that, and all
 * three are recorded in the code they were fixed in: a local
 * completeness rule that needs the whole extent to be legal, a
 * watermark built from the OBSERVED backwardness rather than the
 * declared lateness, and — the last one — treating the sources being
 * exhausted as the stream being over, when every operator still held
 * the panes its own watermark never closed.
 */
class TestPanesOnce extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  val feed: Feed = Feed(20000, Late - 1)

  final case class Row(start: Long, key: Int, value: Long) derives okay.codec.Schema
  final case class Rows(rows: Vector[Row]) derives okay.codec.Schema

  val collect: Aggregator[Pane[Int, Long], Rows, Vector[Row]] =
    Aggregator[Pane[Int, Long], Rows, Vector[Row]](Rows(Vector.empty))((b, p) =>
      Rows(b.rows :+ Row(p.start, p.key, p.value)))((a, b) => Rows(a.rows ++ b.rows))(_.rows)

  object Diag extends Job[Feed, Vector[Row]] {
    type A = Ev
    def name = "diag.window"
    def params = summon[okay.codec.Schema[Feed]]
    def flow(f: Feed, parts: Int) = Flow.slices(events(f), parts)
    def sink(f: Feed) = Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(collect)
  }
  Jobs.register(Diag)

  test("no window is presented twice, at any partition count or epoch size") {
    for parts <- Vector(1, 2, 4, 8); take <- Vector(64, 1000) do
      val batch = Flows.fan(Diag.flow(feed, parts), Diag.sink(feed)).runWith.value
      val streamed = Cluster.stream(Diag, feed, parts, Vector(Cluster.local), take).runWith.value
      val twice = streamed.groupBy(r => (r.start, r.key)).filter(_._2.length > 1)
      assert(twice.isEmpty,
        s"$parts partitions, epochs of $take: ${twice.size} windows presented twice, " +
          s"e.g. ${twice.keys.take(3).mkString(", ")}")
      assertEquals(streamed.length, batch.length, s"$parts partitions, epochs of $take")
      assertEquals(streamed.map(r => (r.start, r.key)).toSet,
        batch.map(r => (r.start, r.key)).toSet, s"$parts partitions, epochs of $take")
  }
}

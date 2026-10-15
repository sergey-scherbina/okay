package okay.flink

import okay.Aggregator
import FlinkInterop.*

/** The AggregateFunction contract, exercised the way a Flink window
 * driver does: partial accumulators per pane, merged, presented. */
class TestFlinkInterop extends munit.FunSuite {

  test("the same aggregator through Flink's lifecycle equals the direct run") {
    val agg = Aggregator.variance[Double]
    val f = toFlink(agg)
    val xs = (1 to 5000).map(_.toDouble * 1.7)
    // three panes, as a window would: accumulate separately, merge, present
    val panes = xs.grouped(1700).map(_.foldLeft(f.createAccumulator())((a, x) => f.add(x, a)))
    val merged = panes.reduce(f.merge)
    assert(math.abs(f.getResult(merged) - agg.run(xs)) / agg.run(xs) < 1e-9)
  }

  test("zip and groupBy travel as one AggregateFunction") {
    val f = toFlink(Aggregator.sum[Long].zip(Aggregator.count[Long]))
    val acc = (1L to 100L).foldLeft(f.createAccumulator())((a, x) => f.add(x, a))
    assertEquals(f.getResult(acc), (5050L, 100L))
    assert(f.isInstanceOf[java.io.Serializable])
  }
}

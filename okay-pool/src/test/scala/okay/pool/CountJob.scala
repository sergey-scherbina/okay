package okay.pool

import okay.Aggregator
import okay.cluster.{Flow, Job, Jobs, Wire}
import okay.codec.Schema

/** the smallest job that still needs several epochs to finish: count
 * `n` elements, `take` at a time. Registered once for the whole suite. */
object CountJob extends Job[Long, Long]:
  type A = Long
  def name = "pool.test.count"
  def params: Schema[Long] = summon[Schema[Long]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(n: Long, parts: Int): Flow[Long] = Flow.slices(Vector.range(0L, n), parts)
  def sink(n: Long): Wire[Long, Long] = Wire.fold(Aggregator.count[Long])

object CountJobs:
  def install(): Unit = Jobs.register(CountJob)

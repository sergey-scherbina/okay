package okay.lake

import okay.cluster.{Flow, Job, Jobs, Wire}
import okay.codec.Schema
import java.util.concurrent.atomic.AtomicBoolean

final case class Trip(id: Long, city: String, km: Double) derives Schema
final case class Scored(id: Long, city: String, score: Double) derives Schema

/** where a scoring run reads and writes, and its row group size */
final case class ScoreParams(plan: LakePlan, out: String, groupRows: Int) derives Schema

/**
 * A MODEL OVER A LAKE: trips in, scores out — the shape of the operator's
 * risk and fraud jobs, with the model a line of Scala. `Faults.killAt`
 * makes the worker scoring that trip die ONCE, mid-write: its partition's
 * object is half-built on its disk and never put.
 */
object ScoreJob extends Job[ScoreParams, Manifest] {
  type A = Scored
  def name: String = "test.lake.score"
  def params: Schema[ScoreParams] = summon[Schema[ScoreParams]]
  def answer: Schema[Manifest] = summon[Schema[Manifest]]
  def score(t: Trip): Double = t.km * 1.5 + t.city.length
  def flow(p: ScoreParams, parts: Int): Flow[Scored] =
    ParquetSource.flow[Trip](p.plan).map { t =>
      Faults.maybe(t.id)
      Scored(t.id, t.city, score(t))
    }
  def sink(p: ScoreParams): Wire[Scored, Manifest] = ParquetSink.to[Scored](p.plan.lake, p.out, p.groupRows)
}

object Faults:
  @volatile var killAt: Long = -1L
  private val fired = AtomicBoolean(false)
  def arm(id: Long): Unit = { killAt = id; fired.set(false) }
  def maybe(id: Long): Unit =
    if id == killAt && fired.compareAndSet(false, true) then
      throw java.io.IOException(s"the worker was killed writing trip $id")

object LakeJobs:
  Jobs.register(ScoreJob)
  def install(): Unit = ()

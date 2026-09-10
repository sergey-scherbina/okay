package okay.cluster

import okay.{Aggregator, Pane}
import okay.codec.Schema

/**
 * A JOB A WORKER PROCESS CAN BE ASKED FOR, and the registrar that
 * tells a build it knows it (specs/dataflow.md, stage 4b).
 *
 * THE SOURCE IS GENERATED FROM THE PARAMETERS, not shipped. That is
 * not a convenience for the test: it is the shape a distributed job
 * actually has. A worker is told which partition of how many, and it
 * produces exactly that partition — from a seed here, from a file
 * range or a topic offset or a table split in a real one. No data
 * crosses the wire on the way IN; only the partial crosses on the way
 * out.
 */
final case class Feed(n: Int, jitter: Long) derives Schema

object Feeds {
  val Size = 1000L
  val Slide = 250L
  val Late = 300L

  final case class Ev(ts: Long, key: Int, v: Int)

  def mix(x: Long): Long =
    var z = x + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  /** the same events in every process, because they are a pure
   * function of the parameters */
  def events(f: Feed): IndexedSeq[Ev] =
    (0 until f.n).map { i =>
      val h = mix(i.toLong)
      val back = if f.jitter == 0 then 0L else math.floorMod(h, f.jitter)
      Ev(i * 10L - back, math.floorMod(h >>> 20, 16).toInt, math.floorMod(h >>> 40, 100).toInt)
    }

  final case class Sum(n: Long, total: Long, x: Long) derives Schema

  val value: Aggregator[Ev, Long, Long] = Aggregator.sum[Long].contramap[Ev](_.v.toLong)

  val paneSum: Aggregator[Pane[Int, Long], Sum, Sum] =
    Aggregator[Pane[Int, Long], Sum, Sum](Sum(0, 0, 0))((s, p) =>
      Sum(s.n + 1, s.total + p.value, s.x ^ mix(p.start * 31 + p.key * 7 + p.value)))((a, b) =>
      Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)
}

/** a tumbling window per key — the shape that exercises the seeding,
 * the completeness rule and a three-Schema partial all at once */
object WindowJob extends Job[Feed, Feeds.Sum] {
  import Feeds.*
  type A = Ev
  def name: String = "test.window"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, Sum] =
    Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
}

/** two windowed stages and a keyed one, over ONE pass — the fan, at a
 * distance */
object FanJob extends Job[Feed, ((Feeds.Sum, Feeds.Sum), Feeds.Sum)] {
  import Feeds.*
  type A = Ev
  def name: String = "test.fan"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, ((Sum, Sum), Sum)] =
    Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
      .and(Wire.sliding(Size, Slide, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum))
      .and(Wire.keyed((e: Ev) => e.key, value)(
        Aggregator[(Int, Long), Sum, Sum](Sum(0, 0, 0))((s, kv) =>
          Sum(s.n + 1, s.total + kv._2, s.x ^ mix(kv._1 * 31 + kv._2)))((a, b) =>
          Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)))
}

/**
 * WHAT THIS BUILD KNOWS HOW TO RUN.
 *
 * `WorkerMain` is handed this class's name and loads it, which runs
 * the registrations below. That is the whole of how a job reaches a
 * worker process: a name in an argument list and a build that
 * contains it.
 */
object TestJobs {
  Jobs.register(WindowJob)
  Jobs.register(FanJob)

  /** loading the class is what registers; this exists so a caller in
   * THIS process can be explicit about it rather than relying on
   * when the object happens to initialise */
  def install(): Unit = ()
}

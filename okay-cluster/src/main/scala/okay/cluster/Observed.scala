package okay.cluster

import okay.codec.Schema
import scala.collection.mutable

/**
 * WHAT A WORKER SAYS IT SPENT ON ONE REQUEST (specs/dataflow.md,
 * stage 15): the rows it read, the nanoseconds between receiving the
 * request and answering it, and — of those — the nanoseconds and the
 * calls a foreign function took. A worker wrapped in `Cluster.measured`
 * answers this beside its answer; the coordinator computes the wire as
 * the round trip minus `nanos`.
 */
final case class Work(rows: Long, nanos: Long, foreignNanos: Long, foreignCalls: Long)

object Work:
  given Schema[Work] = Schema.derived

/**
 * WHAT THE COORDINATOR SEES, as events (stage 15). Times are
 * `System.nanoTime` on the coordinator; `Began` carries the wall clock
 * at the same instant, so a reader can place the rest on a calendar.
 * `what` names the request: extent, run, open, advance, close,
 * shuffle, reduce.
 */
enum Seen:
  case Began(job: String, parts: Int, wallMillis: Long, at: Long)
  case Ended(at: Long, failure: Option[String])
  case Phase(name: String, start: Long, end: Long)
  case Asked(part: Int, worker: Int, what: String, start: Long, end: Long, work: Option[Work])
  case Lost(part: Int, worker: Int, what: String, start: Long, end: Long, why: String)
  case Buried(worker: Int, at: Long)
  /** a stream's epoch: its span, and how far the watermark stood
   * behind the greatest event time seen (event-time units) */
  case Epoch(epoch: Int, lag: Long, start: Long, end: Long)

/**
 * WHO HEARS THE EVENTS. One method; `on` false is `Probe.none`, and
 * the coordinator tests it before BUILDING an event, so a run nobody
 * observes allocates nothing for it. Called from every partition's
 * fibre at once: an implementation synchronizes.
 */
trait Probe:
  def apply(seen: Seen): Unit
  def on: Boolean = true

object Probe:
  val none: Probe = new Probe:
    def apply(seen: Seen): Unit = ()
    override def on: Boolean = false

  /** both hear everything, in this order */
  def both(a: Probe, b: Probe): Probe =
    if !a.on then b else if !b.on then a
    else new Probe:
      def apply(seen: Seen): Unit = { a(seen); b(seen) }

/**
 * THE WORKER'S METER (stage 15): what the request being answered on
 * THIS thread has read and spent in foreign code. `Cluster.measured`
 * resets it before a request and reads it after; the engine adds rows
 * where it folds, and a foreign stage adds its calls. Thread-local
 * because a worker answers several requests at once, each on its own
 * thread, and a partition is folded on the thread that asked for it.
 */
object Meter:
  private final class Now:
    var rows = 0L
    var foreignNanos = 0L
    var foreignCalls = 0L

  private val here: ThreadLocal[Now] = new ThreadLocal[Now]:
    override def initialValue(): Now = Now()

  /** `n` more rows read for the request on this thread */
  def rows(n: Long): Unit =
    val h = here.get.nn
    h.rows += n

  /** one foreign call of `nanos`, for the request on this thread */
  def foreign(nanos: Long): Unit =
    val h = here.get.nn
    h.foreignNanos += nanos
    h.foreignCalls += 1

  private[cluster] def begin(): Unit =
    val h = here.get.nn
    h.rows = 0L; h.foreignNanos = 0L; h.foreignCalls = 0L

  private[cluster] def taken(nanos: Long): Work =
    val h = here.get.nn
    Work(h.rows, nanos, h.foreignNanos, h.foreignCalls)

/**
 * A JOB'S METRICS, AS PROMETHEUS TEXT (stage 15): a fold over the
 * events of every run it is given a probe for. One `JobStats` serves a
 * whole process — `probe()` makes a probe per RUN (a recompute is a
 * per-run fact: an attempt that succeeds for a partition that had
 * already lost one) and adds into counters labelled by the job's name.
 */
final class JobStats:
  private val counters = mutable.LinkedHashMap.empty[(String, String), Double]
  private val gauges = mutable.LinkedHashMap.empty[(String, String), Double]

  private def add(metric: String, labels: String, v: Double): Unit = synchronized {
    counters.update((metric, labels), counters.getOrElse((metric, labels), 0.0) + v)
  }
  private def set(metric: String, labels: String, v: Double): Unit = synchronized {
    gauges.update((metric, labels), v)
  }

  /** a probe for one run */
  def probe(): Probe = new Probe:
    private var job = "?"
    private val hurt = mutable.Set.empty[(String, Int)]
    private def l(more: String = ""): String = s"""job="${JobStats.esc(job)}"$more"""
    def apply(seen: Seen): Unit = synchronized {
      seen match
        case Seen.Began(name, _, _, _) => job = name
        case Seen.Ended(_, failure) =>
          add("okay_job_runs_total", l(s""",outcome="${if failure.isEmpty then "ok" else "failed"}""""), 1)
        case Seen.Phase(_, _, _) => ()
        case Seen.Asked(part, _, what, start, end, work) =>
          add("okay_job_attempts_total", l(s""",what="$what""""), 1)
          if hurt.remove((what, part)) then add("okay_job_recomputes_total", l(), 1)
          val took = (end - start).toDouble / 1e9
          work match
            case Some(w) =>
              add("okay_job_rows_total", l(), w.rows.toDouble)
              if w.rows > 0 then add("okay_job_partition_rows_total", l(s""",partition="$part""""), w.rows.toDouble)
              add("okay_job_seconds_total", l(""",where="foreign""""), w.foreignNanos.toDouble / 1e9)
              add("okay_job_seconds_total", l(""",where="engine""""), (w.nanos - w.foreignNanos).toDouble / 1e9)
              add("okay_job_seconds_total", l(""",where="wire""""), took - w.nanos.toDouble / 1e9)
              add("okay_job_foreign_calls_total", l(), w.foreignCalls.toDouble)
            case None =>
              add("okay_job_seconds_total", l(""",where="unmeasured""""), took)
        case Seen.Lost(part, _, what, _, _, _) =>
          add("okay_job_attempts_lost_total", l(s""",what="$what""""), 1)
          val _ = hurt.add((what, part))
        case Seen.Buried(_, _) => add("okay_job_workers_buried_total", l(), 1)
        case Seen.Epoch(epoch, lag, _, _) =>
          set("okay_job_epoch", l(), epoch.toDouble)
          set("okay_job_watermark_lag", l(), lag.toDouble)
    }

  /** one counter's value, for a caller that asks rather than scrapes */
  def value(metric: String, job: String, more: String = ""): Double = synchronized {
    counters.getOrElse((metric, s"""job="${JobStats.esc(job)}"$more"""),
      gauges.getOrElse((metric, s"""job="${JobStats.esc(job)}"$more"""), 0.0))
  }

  /** the text format, every metric under one HELP/TYPE */
  def render: String = synchronized {
    val sb = new StringBuilder
    def emit(kind: String, rows: mutable.LinkedHashMap[(String, String), Double]): Unit =
      for (metric, series) <- rows.toVector.groupBy(_._1._1).toVector.sortBy(_._1) do
        sb ++= s"# HELP $metric ${JobStats.help.getOrElse(metric, metric)}\n# TYPE $metric $kind\n"
        for ((_, labels), v) <- series do sb ++= s"$metric{$labels} ${JobStats.num(v)}\n"
    emit("counter", counters)
    emit("gauge", gauges)
    sb.result()
  }

object JobStats:
  private[cluster] def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")
  private def num(v: Double): String =
    if v == math.rint(v) && math.abs(v) < 1e15 then v.toLong.toString else v.toString
  private val help: Map[String, String] = Map(
    "okay_job_runs_total" -> "runs coordinated here, by how they ended",
    "okay_job_attempts_total" -> "requests a worker answered, by kind",
    "okay_job_attempts_lost_total" -> "requests lost to a failed worker, by kind",
    "okay_job_recomputes_total" -> "partitions answered again after a lost attempt",
    "okay_job_workers_buried_total" -> "workers taken out of the rotation",
    "okay_job_rows_total" -> "rows the workers read",
    "okay_job_partition_rows_total" -> "rows the workers read, per partition",
    "okay_job_seconds_total" -> "time spent, by where: engine and foreign on the worker, wire the rest of the round trip",
    "okay_job_foreign_calls_total" -> "foreign function calls on the workers",
    "okay_job_epoch" -> "the last epoch a stream committed",
    "okay_job_watermark_lag" -> "how far the watermark stood behind the greatest event time, in event-time units")

/**
 * A JOB'S TRACE (stage 15): the events of ONE run as a span tree — the
 * run, its phases (or a stream's epochs), and every attempt under the
 * phase it started in. A lost attempt is an error span, a burial a
 * zero-length one. Times are nanoseconds since the Unix epoch.
 */
final class JobTrace extends Probe:
  import JobTrace.Span
  private val events = mutable.ArrayBuffer.empty[Seen]
  def apply(seen: Seen): Unit = synchronized { val _ = events += seen }

  /** the spans so far; the root is open (ends now) until the run ends */
  def spans: Vector[Span] = synchronized {
    val began = events.collectFirst { case b: Seen.Began => b }
    began match
      case None => Vector.empty
      case Some(b) =>
        def wall(t: Long): Long = b.wallMillis * 1000000L + (t - b.at)
        val ended = events.collectFirst { case e: Seen.Ended => e }
        val end = ended.fold(System.nanoTime())(_.at)
        val root = Span("0", None, s"job ${b.job}", wall(b.at), wall(end),
          Vector("job" -> b.job, "partitions" -> b.parts.toString), ended.flatMap(_.failure))
        var n = 0
        def id(): String = { n += 1; n.toString }
        val phases = events.toVector.collect {
          case Seen.Phase(name, s, e) => (name, s, e, Vector.empty[(String, String)])
          case Seen.Epoch(k, lag, s, e) => (s"epoch $k", s, e, Vector("lag" -> lag.toString))
        }.map((name, s, e, attrs) => (s, e, Span(id(), Some(root.id), name, wall(s), wall(e), attrs, None)))
        def under(t: Long): String =
          phases.find((s, e, _) => s <= t && t <= e).fold(root.id)(_._3.id)
        val attempts = events.toVector.collect {
          case Seen.Asked(part, w, what, s, e, work) =>
            val split = work.toVector.flatMap(k => Vector(
              "rows" -> k.rows.toString,
              "engine_ns" -> (k.nanos - k.foreignNanos).toString,
              "foreign_ns" -> k.foreignNanos.toString,
              "foreign_calls" -> k.foreignCalls.toString,
              "wire_ns" -> ((e - s) - k.nanos).toString))
            Span(id(), Some(under(s)), s"$what $part", wall(s), wall(e),
              Vector("partition" -> part.toString, "worker" -> w.toString) ++ split, None)
          case Seen.Lost(part, w, what, s, e, why) =>
            Span(id(), Some(under(s)), s"$what $part", wall(s), wall(e),
              Vector("partition" -> part.toString, "worker" -> w.toString), Some(why))
          case Seen.Buried(w, t) =>
            Span(id(), Some(under(t)), s"buried worker $w", wall(t), wall(t),
              Vector("worker" -> w.toString), None)
        }
        root +: (phases.map(_._3) ++ attempts)
  }

object JobTrace:
  /** one span: `error` is its status when it failed */
  final case class Span(id: String, parent: Option[String], name: String, start: Long, end: Long,
                        attrs: Vector[(String, String)], error: Option[String])

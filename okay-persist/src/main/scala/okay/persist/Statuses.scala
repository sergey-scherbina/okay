package okay.persist

import okay.codec.Schema

/**
 * WHICH RUNS EXIST AND WHAT THEY ARE DOING (workflow-visibility,
 * 2026-09-17) — the question an operator asks at three in the morning
 * and the model, on its own, cannot answer cheaply.
 *
 * It CAN answer it expensively: a dialogue's place is its journal
 * folded by its own program, so "what is every run waiting for" means
 * running every program over every journal. That is the cost of "the
 * fold is the program", and it is the wrong thing to pay on a
 * dashboard refresh.
 *
 * So the WORKER writes it down. Every `advance` already knows what it
 * learned — finished, sleeping until, waiting on, or broken — and it
 * puts that under the dialogue's key in a compacted topic. Reading
 * the index is then one scan of small records, with no program run at
 * all.
 *
 * WHAT THIS INDEX IS NOT, and it is the architecture's first rule
 * again: it is not state. Lose the whole topic and every run is still
 * exactly where its journal says it is — the dashboard goes blank and
 * fills again as workers touch runs. Nothing reads it to decide
 * anything; it exists to be looked at. That is also why a status may
 * be STALE (a run whose worker died between advancing and writing) and
 * why nothing here pretends otherwise: `at` says when it was written,
 * and an operator comparing that to the clock is the honest check.
 */
final class Statuses(val snapshots: Snapshots):

  def put(s: Statuses.Status): Unit =
    val _ = snapshots.putValue(s.id.getBytes("UTF-8"), s)

  def get(id: String): Option[Statuses.Status] =
    snapshots.latestValue[Statuses.Status](id.getBytes("UTF-8")).flatMap(_._2.toOption)

  /** everything the workers have told this index, newest write per id */
  def all: List[Statuses.Status] =
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Statuses.Status]
    val t = snapshots.topic
    var p = 0
    while p < t.partitions do
      var from = t.begin(p)
      var going = true
      while going do
        t.read(p, from, 512) match
          case Topic.Read.TooEarly(b) => from = b
          case Topic.Read.Records(rs) =>
            if rs.isEmpty then going = false
            else
              rs.foreach: r =>
                okay.codec.Codecs.readCbor[Statuses.Status](r.value).toOption
                  .foreach(s => out(s.id) = s)
              from = rs.last.offset + 1
      p += 1
    out.values.toList

  /** the runs an operator usually wants: not finished, and not moved
   * since `since` — the stuck ones, or the ones simply sleeping long */
  def idleSince(sinceMillis: Long): List[Statuses.Status] =
    all.filter(s => !s.state.isInstanceOf[Statuses.State.Finished] && s.at <= sinceMillis)

  /** every run waiting on this signal name — "who is blocked on
   * approval" as one read */
  /**
   * WHAT NEEDS A PERSON, which is the one question an operator
   * actually asks (statuses-verdicts, 2026-09-17).
   *
   * `Broken` — the journal cannot be folded: damage, or a record from
   * another program. `Incompatible` — the journal is fine and THIS
   * CODE refuses it. Both wait for a human and neither improves on
   * its own.
   *
   * `Failed` is deliberately absent. The worker retries it, so
   * listing it here would fill the page with rows that fix
   * themselves, and a page like that is one nobody reads.
   */
  def needsAttention: List[Statuses.Status] =
    all.filter: s =>
      s.state match
        case Statuses.State.Broken(_) => true
        case Statuses.State.Incompatible(_) => true
        case _ => false

  def waitingOn(name: String): List[Statuses.Status] =
    all.filter(_.state == Statuses.State.Waiting(s"signal:$name"))

object Statuses:

  /** where a run stood when a worker last touched it */
  enum State:
    case Running
    case Sleeping(untilMillis: Long)
    case Waiting(what: String)
    case Finished(answer: String)
    case Broken(why: String)
    // APPENDED, NOT INSERTED (statuses-verdicts, 2026-09-17): a
    // derived Schema numbers an enum's cases in order, so a new case
    // at the END leaves every status written before this readable.
    // Putting `Failed` beside `Broken` where it belongs alphabetically
    // would have renumbered both.
    /** the drive threw and the worker caught it. It retries by
     * itself, so this is NOT `needsAttention` — waking somebody for
     * it is how a dashboard teaches people to ignore dashboards. */
    case Failed(why: String)
    /** the program cannot replay history it accepted: a bad deploy.
     * Deterministic, so no amount of retrying helps — a person
     * chooses between fixing the code and retiring the run. */
    case Incompatible(why: String)

  given Schema[State] = Schema.derived

  /** one line of a dashboard */
  final case class Status(id: String, program: String, state: State,
                          asking: Option[String], where: Option[String],
                          at: Long)

  given Schema[Status] = Schema.derived

  def over(store: Store, name: String = "__status"): Statuses =
    new Statuses(Snapshots(store, name))

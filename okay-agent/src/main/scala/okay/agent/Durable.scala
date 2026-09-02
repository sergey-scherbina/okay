package okay.agent

import okay.Handler
import okay.codec.Json

/**
 * The overlay seam (obs-durable-overlay, specs/obs.md "The Durable
 * resonance"): a journaled operation opens a span carrying the
 * journal's identity, so an incident replayed by `Durable.replaying`
 * lays its spans over the originals. Deliberately NEUTRAL — okay-agent
 * does not depend on okay-obs; `okay.obs.Tracer` adapts to this in one
 * line, and any other span sink can too. The journal and the trace
 * stay two things (the spec resists merging them); they meet only on
 * the operation identity, which is the journal `Entry.key`.
 */
trait OpTrace:
  def span[A](name: String, attrs: (String, String)*)(body: => A): A

/**
 * Durable execution without repeated side effects
 * (specs/llm-agentic.md).
 *
 * The honest starting point: exactly-once EXECUTION of an external
 * effect is impossible. A process can die after the request left the
 * machine and before the answer was written down, and no amount of
 * local bookkeeping can tell "it never arrived" from "it succeeded
 * and the reply was lost". What IS achievable is exactly-once
 * OUTCOME, and it is achieved at the far end — by an idempotency key
 * the far end deduplicates on, or by asking it what happened.
 *
 * So the design is not a guarantee, it is a DECISION, taken per
 * operation and declared where the tool is declared:
 *
 *   - `Redo`      the call is safe to repeat (a read, a search)
 *   - `WithKey`   repeat it carrying the SAME key as the first
 *                 attempt, so the far end deduplicates (this is the
 *                 answer for payments — every payment API supports it)
 *   - `Reconcile` do not repeat: ask the far end, by that key, what
 *                 the outcome was
 *   - `Escalate`  do not repeat: a human decides
 *   - `Fail`      do not repeat: refuse to continue
 *
 * The journal is written INTENT FIRST — name, arguments, key — and
 * the answer after, so recovery can always tell the three cases
 * apart: an entry with an answer (skip it, the effect already
 * happened), an entry without one (the crash window: apply the
 * policy), and no entry at all (never ran: execute normally).
 *
 * The same handler serves the first run and the recovery: on a fresh
 * journal everything executes and is recorded; on a populated one the
 * recorded answers are handed back without touching the world.
 */
object Durable {

  /** what to do when recovery finds an intent with no answer */
  enum OnRepeat:
    case Redo, WithKey, Reconcile, Escalate, Fail

  /** one journalled step. `fingerprint` is what the program asked
   * for; a mismatch on replay means the code changed under us */
  final case class Entry(seq: Int, op: String, fingerprint: String,
                         key: String, answer: Option[String])

  /** the journal: append-only, and that is the whole interface —
   * memory here, a file or a table behind the same three methods */
  trait Journal:
    def append(e: Entry): Unit
    def complete(seq: Int, answer: String): Unit
    def all: Vector[Entry]

  final class MemoryJournal extends Journal:
    private var entries = Vector.empty[Entry]
    def append(e: Entry): Unit = entries = entries :+ e
    def complete(seq: Int, answer: String): Unit =
      entries = entries.map(e => if e.seq == seq then e.copy(answer = Some(answer)) else e)
    def all: Vector[Entry] = entries

  /** the program changed between the run and the replay */
  final class Drift(val expected: String, val got: String)
    extends RuntimeException(
      s"the journal does not match the program: expected $expected, got $got")

  /** recovery found an intent whose outcome is unknown and whose
   * policy forbids repeating it */
  final class Unresolved(val op: String, val key: String)
    extends RuntimeException(s"unknown outcome for $op (key $key) and no way to resolve it")

  /** the reserved argument a WithKey retry carries, so the far end
   * can deduplicate the second attempt against the first */
  val KeyField = "idempotency_key"

  private def fingerprintOf(c: ToolCall): String =
    s"${c.name}(${Json.print(c.args)})"

  /** a key that is stable across replays: the step's position and
   * what it asked for, nothing that varies per process */
  def keyFor(seq: Int, c: ToolCall): String =
    s"${c.name}-$seq-${math.abs(fingerprintOf(c).hashCode)}"

  /** run one operation inside its overlay span (obs-durable-overlay):
   * the span carries the journal identity — `durable.key` equals the
   * `Entry.key` for this (seq, call) — so first-run and replay spans
   * share it and lay over one another. No tracer, no span: the join
   * is opt-in and costs nothing when off. */
  private def traced(trace: Option[OpTrace], c: ToolCall, n: Int,
                     replay: Boolean = false)(body: => String): String =
    trace match
      case None => body
      case Some(t) =>
        val base = Vector("durable.op" -> c.name, "durable.key" -> keyFor(n, c),
                          "durable.seq" -> n.toString)
        val attrs = if replay then base :+ ("durable.replay" -> "true") else base
        t.span(c.name, attrs*)(body)

  /**
   * The durable tool handler. Wraps any inner handler; the journal
   * decides whether the world is touched at all.
   *
   * `policy` answers per tool name — the declaration site of a tool
   * is where its repeat semantics belong. `reconcile` is consulted
   * only for `Reconcile`, `escalate` only for `Escalate`.
   */
  def tools(inner: Handler[Tool], journal: Journal)
           (policy: String => OnRepeat = _ => OnRepeat.Fail,
            reconcile: (ToolCall, String) => Option[String] = (_, _) => None,
            escalate: (ToolCall, String) => Option[String] = (_, _) => None,
            trace: Option[OpTrace] = None)
  : Handler[Tool] = new Handler[Tool]:

    private var seq = 0
    private val recorded = journal.all

    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) =>
        val n = seq
        seq += 1
        val fp = fingerprintOf(c)

        // the overlay span: same identity as the journal entry (the
        // key), so a later replay lays over exactly this operation
        traced(trace, c, n) {
          recorded.find(_.seq == n) match
            // never ran: execute and journal, intent first
            case None => execute(n, c, fp, c)

            case Some(entry) =>
              if entry.fingerprint != fp then throw Drift(entry.fingerprint, fp)
              entry.answer match
                // it already happened: hand the answer back, touch nothing
                case Some(a) => a

                // the crash window: the outcome is unknown
                case None => policy(c.name) match
                  case OnRepeat.Redo => execute(n, c, fp, c)
                  case OnRepeat.WithKey =>
                    // the same key as the first attempt, so the far end
                    // recognises the retry as the same request
                    execute(n, c, fp, withKey(c, entry.key))
                  case OnRepeat.Reconcile =>
                    reconcile(c, entry.key) match
                      case Some(a) => journal.complete(n, a); a
                      case None => throw Unresolved(c.name, entry.key)
                  case OnRepeat.Escalate =>
                    escalate(c, entry.key) match
                      case Some(a) => journal.complete(n, a); a
                      case None => throw Unresolved(c.name, entry.key)
                  case OnRepeat.Fail => throw Unresolved(c.name, entry.key)
        }

    private def withKey(c: ToolCall, key: String): ToolCall =
      c.args match
        case Json.JObj(fs) =>
          c.copy(args = Json.JObj(fs.filterNot(_._1 == KeyField) :+ (KeyField, Json.JStr(key))))
        case _ => c.copy(args = Json.JObj(Vector((KeyField, Json.JStr(key)))))

    /** intent first, then the effect, then the answer — the order is
     * the whole point: a crash between the second and third steps is
     * exactly what the policies above are for */
    private def execute(n: Int, original: ToolCall, fp: String, toRun: ToolCall): String =
      val key = keyFor(n, original)
      if !recorded.exists(_.seq == n) then
        journal.append(Entry(n, original.name, fp, key, None))
      val answer: String = inner.handle(Tool.Call(toRun))
      journal.complete(n, answer)
      answer

  /**
   * Deterministic replay for its own sake: answer every call from the
   * journal and NEVER touch the world — a production incident run
   * again, offline, with no model and no side effects. The half of
   * durability that is worth as much as the recovery.
   */
  def replaying(journal: Journal, trace: Option[OpTrace] = None): Handler[Tool] = new Handler[Tool]:
    private var seq = 0
    private val recorded = journal.all
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) =>
        val n = seq
        seq += 1
        val fp = fingerprintOf(c)
        // replay=true: the overlay span is marked as the re-run, but
        // carries the SAME key, so it lands over the original
        traced(trace, c, n, replay = true) {
          recorded.find(_.seq == n) match
            case Some(entry) if entry.fingerprint != fp => throw Drift(entry.fingerprint, fp)
            case Some(Entry(_, _, _, _, Some(a))) => a
            case Some(entry) => throw Unresolved(c.name, entry.key)
            case None => throw Unresolved(c.name, "beyond the journal")
        }
}

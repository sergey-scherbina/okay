package okay.durable

import okay.Answers
import okay.codec.{Base64, Json, Journalled}

/**
 * The overlay seam (obs-durable-overlay, specs/obs.md "The Durable
 * resonance"): a journaled operation opens a span carrying the
 * journal's identity, so an incident replayed by `Durable.replaying`
 * lays its spans over the originals. Deliberately NEUTRAL — okay-durable
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
 *                 answer for payments — when the payment API supports it)
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

  /**
   * What a missing answer MEANS for this operation.
   *
   * Four of these answer the recovery question — the crash window,
   * an outcome nobody can know. `Await` answers a different one: the
   * answer comes from OUTSIDE the program and has not arrived yet,
   * which is not a failure and not unknown. Nearest to `Escalate` (a
   * human decides) and differing in when: an escalation resolves
   * inside the call through a callback, an await leaves the program
   * parked and hands control back to whoever ran it.
   */
  enum OnRepeat:
    case Redo, WithKey, Reconcile, Escalate, Fail, Await

  /** one journalled step. `fingerprint` is what the program asked
   * for; a mismatch on replay means the code changed under us */
  final case class Entry(seq: Int, op: String, fingerprint: String,
                         key: String, answer: Option[String])

  /** Append-only journal with stable run identity; memory here,
   * a file or a table behind the same storage methods. */
  trait Journal:
    /** Stable identity, unique in the provider's deduplication namespace.
     * Include tenant/workflow identity when local run numbers can repeat. */
    def runId: Option[String] = None
    def append(e: Entry): Unit
    def complete(seq: Int, answer: String): Unit
    def all: Vector[Entry]

  final class MemoryJournal(run: String) extends Journal:
    def this() = this(RunId.fresh())
    override val runId: Option[String] = Some(run)
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

  /**
   * Not a failure: the program asked something whose answer comes
   * from outside it, and is parked until that answer is journalled.
   *
   * A control transfer rather than an error, for the same reason
   * `Drift` and `Unresolved` are thrown from a handler that must
   * otherwise produce an `A`: `Answers.handle` has no way to leave
   * without one. `NoStackTrace` because being parked is the normal
   * state of a conversation, not an incident — a stack trace per
   * question is a cost paid on every turn for nothing.
   *
   * Carries what a caller needs to render the question now and to
   * answer it later: the operation, its arguments, and the sequence
   * number `Journal.complete` takes.
   */
  final class Awaiting(val op: String, val seq: Int, val key: String,
                       val args: Json)
    extends RuntimeException(s"awaiting an answer to $op (seq $seq, key $key)")
    with scala.util.control.NoStackTrace

  /**
   * The entry a program is parked on, or `None` when it is not
   * parked. Enough to render the outstanding question after a restart
   * without running the program to find it.
   *
   * The FIRST answerless entry, because a program resumes in its own
   * sequence: a later question cannot have been asked before an
   * earlier one was answered.
   */
  /**
   * What an entry's operation ASKED FOR, read back out of its
   * fingerprint.
   *
   * The fingerprint is `op(args)` by construction (`fingerprintOf`),
   * built for comparison rather than for reading — so the reading
   * lives here, next to the writing, and a caller never has to know
   * the shape. This is what makes `awaiting` enough to render an
   * outstanding question after a restart: the entry says not only
   * that something was asked but what.
   */
  def argsOf(e: Entry): Option[Json] =
    Option(e.fingerprint)
      .filter(f => f.startsWith(e.op + "(") && f.endsWith(")"))
      .map(f => f.drop(e.op.length + 1).dropRight(1))
      .flatMap(s => scala.util.Try(Json.parse(s)).toOption)

  def awaiting(journal: Journal): Option[Entry] =
    journal.all.sortBy(_.seq).find(_.answer.isEmpty)

  /** Legacy unscoped identity, retained for archived entries and source compatibility. */
  def keyFor[Op[_], A](seq: Int, op: Op[A])(using J: Journalled[Op]): String =
    keyOf(J.name(op), seq, J.fingerprint(op))

  /** Identity for a new step. Persist the run identity across restarts.
   * Unscoped journals retain the legacy helper; fresh WithKey requires runId. */
  def keyFor[Op[_], A](journal: Journal, seq: Int, op: Op[A])
                      (using J: Journalled[Op]): String =
    journal.runId match
      case Some(run) => scopedKey(run, seq)
      case None => keyOf(J.name(op), seq, J.fingerprint(op))

  private def scopedKey(run: String, seq: Int): String =
    val bytes = run.getBytes("UTF-8")
    require(bytes.nonEmpty && bytes.length <= 96, "durable runId must contain 1..96 UTF-8 bytes")
    require(new String(bytes, "UTF-8") == run, "durable runId must be well-formed Unicode")
    require(seq >= 0, "durable sequence must be non-negative")
    val encoded = Base64.encode(bytes).replace('+', '-').replace('/', '_').takeWhile(_ != '=')
    s"okay-$encoded-$seq"

  /** the same rule for any operation — name, position, fingerprint,
   * and nothing that varies per process */
  private def keyOf(name: String, seq: Int, fingerprint: String): String =
    s"$name-$seq-${math.abs(fingerprint.hashCode)}"

  /** run one operation inside its overlay span (obs-durable-overlay):
   * the span carries the journal identity — `durable.key` equals the
   * `Entry.key` for this (seq, call) — so first-run and replay spans
   * share it and lay over one another. No tracer, no span: the join
   * is opt-in and costs nothing when off. */
  private def traced[A](trace: Option[OpTrace], name: String, key: String, n: Int,
                        replay: Boolean = false)(body: => A): A =
    trace match
      case None => body
      case Some(t) =>
        val base = Vector("durable.op" -> name, "durable.key" -> key,
                          "durable.seq" -> n.toString)
        val attrs = if replay then base :+ ("durable.replay" -> "true") else base
        t.span(name, attrs*)(body)

  /**
   * The durable handler for ANY journalled operation
   * (durable-any-operation). Wraps any inner handler; the journal
   * decides whether the world is touched at all.
   *
   * `policy` answers per operation NAME — the declaration site of an
   * operation is where its repeat semantics belong. `reconcile` is
   * consulted only for `Reconcile`, `escalate` only for `Escalate`;
   * both answer in the journal's own written form, which is what the
   * far end can be asked for and what the journal must record.
   */
  def over[Op[_]](inner: Answers[Op], journal: Journal)
                 (policy: String => OnRepeat = _ => OnRepeat.Fail,
                  // POLYMORPHIC, not `Op[?]`: an abstract type
                  // constructor cannot be applied to a wildcard, and
                  // the honest reading is that these answer for an
                  // operation of ANY answer type
                  reconcile: [X] => (Op[X], String) => Option[String] =
                    [X] => (_: Op[X], _: String) => None,
                  escalate: [X] => (Op[X], String) => Option[String] =
                    [X] => (_: Op[X], _: String) => None,
                  trace: Option[OpTrace] = None,
                  /** told of each operation answered FROM THE JOURNAL, and
                   * its answer: a handler that keeps state across its
                   * operations (a supervised foreign worker's continuation
                   * table) rebuilds it from what it did not see happen
                   * (foreign-workflow stage 3). Nothing by default. */
                  replayed: [X] => (Op[X], X) => Unit =
                    [X] => (_: Op[X], _: X) => ())
                 (using J: Journalled[Op])
  : Answers[Op] = new Answers[Op]:

    private var seq = 0
    private val recorded = journal.all
    private val scope = journal.runId

    def handle[A](op: Op[A]): A =
      val n = seq
      seq += 1
      val name = J.name(op)
      val fp = J.fingerprint(op)
      val entryAt = recorded.find(_.seq == n)
      val key = entryAt match
        case Some(entry) => entry.key
        case None => scope.fold(keyOf(name, n, fp))(scopedKey(_, n))

      // the overlay span: same identity as the journal entry (the
      // key), so a later replay lays over exactly this operation
      traced(trace, name, key, n) {
        entryAt match
          // never ran: execute and journal, intent first — unless
          // the answer is a person's, in which case there is no
          // effect to run and the question is what gets recorded
          case None => policy(name) match
            case OnRepeat.Await => park(n, op, name, fp, key)
            case OnRepeat.WithKey =>
              require(scope.isDefined, "fresh WithKey requires a stable journal runId")
              execute(n, name, fp, key, J.withKey(op, key))
            case _ => execute(n, name, fp, key, op)

          case Some(entry) =>
            if entry.fingerprint != fp then throw Drift(entry.fingerprint, fp)
            entry.answer match
              // it already happened: hand the answer back, touch nothing
              // (the witness is told, so state kept beside the effect
              // can catch up)
              case Some(a) =>
                val answer = J.decode(op, a)
                replayed(op, answer)
                answer

              // the crash window: the outcome is unknown — except
              // for an operation whose answer was never this
              // program's to produce, where it means "not yet"
              case None => policy(name) match
                case OnRepeat.Await => throw Awaiting(name, n, entry.key, J.asked(op))
                case OnRepeat.Redo => execute(n, name, fp, entry.key, op)
                case OnRepeat.WithKey =>
                  // the same key as the first attempt, so the far end
                  // recognises the retry as the same request
                  execute(n, name, fp, entry.key, J.withKey(op, entry.key))
                case OnRepeat.Reconcile =>
                  reconcile(op, entry.key) match
                    case Some(a) => journal.complete(n, a); J.decode(op, a)
                    case None => throw Unresolved(name, entry.key)
                case OnRepeat.Escalate =>
                  escalate(op, entry.key) match
                    case Some(a) => journal.complete(n, a); J.decode(op, a)
                    case None => throw Unresolved(name, entry.key)
                case OnRepeat.Fail => throw Unresolved(name, entry.key)
      }

    /** the question, recorded, and control handed back. The inner
     * handler is never reached: asking a person touches no world. */
    private def park[A](n: Int, op: Op[A], name: String, fp: String, key: String): Nothing =
      if !recorded.exists(_.seq == n) then
        journal.append(Entry(n, name, fp, key, None))
      throw Awaiting(name, n, key, J.asked(op))

    /** intent first, then the effect, then the answer — the order is
     * the whole point: a crash between the second and third steps is
     * exactly what the policies above are for */
    private def execute[A](n: Int, name: String, fp: String,
                           key: String, toRun: Op[A]): A =
      if !recorded.exists(_.seq == n) then
        journal.append(Entry(n, name, fp, key, None))
      // performed by the instance, which is the only place the answer
      // type is exact — see `Journalled.perform`
      val (answer, written) = J.perform(toRun, inner)
      journal.complete(n, written)
      answer

  /**
   * Deterministic replay for its own sake: answer every call from the
   * journal and NEVER touch the world — a production incident run
   * again, offline, with no model and no side effects. The half of
   * durability that is worth as much as the recovery.
   */
  def replayingOver[Op[_]](journal: Journal, trace: Option[OpTrace] = None)
                          (using J: Journalled[Op]): Answers[Op] = new Answers[Op]:
    private var seq = 0
    private val recorded = journal.all
    def handle[A](op: Op[A]): A =
      val n = seq
      seq += 1
      val name = J.name(op)
      val fp = J.fingerprint(op)
      // replay=true: the overlay span is marked as the re-run, but
      // carries the SAME key, so it lands over the original
      val entryAt = recorded.find(_.seq == n)
      val key = entryAt.fold(keyOf(name, n, fp))(_.key)
      traced(trace, name, key, n, replay = true) {
        entryAt match
          case Some(entry) if entry.fingerprint != fp => throw Drift(entry.fingerprint, fp)
          case Some(Entry(_, _, _, _, Some(a))) => J.decode(op, a)
          case Some(entry) => throw Unresolved(name, entry.key)
          case None => throw Unresolved(name, "beyond the journal")
      }

}

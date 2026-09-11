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
/**
 * What a journal needs of an operation in order to record it
 * (durable-any-operation, specs/llm-agentic.md "Any operation, not
 * only a tool").
 *
 * `Durable` journalled `Tool` and nothing else, and two other specs
 * noticed by promising what it could not do — specs/r.md and
 * specs/py.md both said a foreign-runtime step is "journalable by
 * Durable", and both had to be corrected. This is the seam that makes
 * them true.
 *
 * FOUR OF THESE ARE WHAT THE CODE ALREADY USED. Reading the handler
 * below, a `ToolCall` reached the journal in exactly four ways: its
 * name, a fingerprint compared on replay, a key a far end
 * deduplicates on, and the retry that carries that key. Nothing else
 * about a tool ever got in.
 *
 * THE FIFTH IS THE ONE THAT DECIDES THE SHAPE. `Entry.answer` is an
 * `Option[String]` and every file and table behind `Journal` stores
 * strings, so an operation answering an `A` must say how that `A` is
 * written down and read back. The codec belongs HERE rather than in
 * the framework because the operation knows its own answer type and
 * the framework does not — and because it is the honest place to pay:
 * a journal that can replay a million-row frame has to have written a
 * million rows down, and an instance that does not want that journals
 * a handle instead. Either way the choice is stated where the
 * operation is declared, which is where every other Durable decision
 * already lives.
 *
 * The `Tool` instance is the identity on all of it, which is why
 * `Durable.tools` behaves exactly as it did.
 */
trait Journalled[Op[_]]:
  /** the journal's `op` column, and the span's name */
  def name[A](op: Op[A]): String

  /** what the program ASKED FOR, compared on replay to catch drift.
   * Whatever identifies the request — an R instance fingerprints the
   * script and a hash of its inputs, not the frame it will return. */
  def fingerprint[A](op: Op[A]): String

  /** the retry that carries the first attempt's key, so the far end
   * recognises it as the same request (`OnRepeat.WithKey`). An
   * operation with nowhere to put a key returns itself — and should
   * not be declared `WithKey`. */
  def withKey[A](op: Op[A], key: String): Op[A]

  /**
   * Run the operation and say how its answer is written down — both
   * at once, and that is not a convenience.
   *
   * `Tool[+A]` is COVARIANT, so matching `Tool.Call` refines `A` only
   * to `A >: String`: enough to hand a `String` back as an `A`
   * (decode), not enough to turn an `A` into a `String` (encode). An
   * `encode[A](op: Op[A], answer: A): String` therefore cannot be
   * written for it without a runtime type test, and this repository
   * does not take a cast while a typed road exists.
   *
   * The typed road is here. The answer type is exact wherever the
   * operation is CONSTRUCTED — `inner.handle(Tool.Call(c))` is a
   * `Tool[String]` and answers a `String` — so the instance performs
   * the call inside its own match, where it still knows that, and
   * returns the typed answer beside its written form. `(s, s)` for a
   * tool, and neither half is a cast.
   *
   * (Making `Tool` invariant was tried first and refused by the
   * compiler for a better reason than variance pedantry: the effect
   * row needs it, and `effect(Tool.Call(c)): String ! Agent` stops
   * type-checking without it.)
   */
  def perform[A](op: Op[A], inner: Handler[Op]): (A, String)

  /** the answer, back out of the journal. Given the operation, so a
   * GADT can refine `A` — and String <: A is the easy direction. */
  def decode[A](op: Op[A], written: String): A

  /** what to show whoever must answer a parked question. Defaults to
   * the fingerprint, which is always available and always cheap. */
  def asked[A](op: Op[A]): Json = Json.JStr(fingerprint(op))

object Journalled:
  /** the instance the shipped behaviour is made of: the answer is
   * already a `String`, so the codec is the identity, and the parked
   * question shows the call's own arguments */
  given Journalled[Tool] with
    def name[A](op: Tool[A]): String = op match
      case Tool.Call(c) => c.name
    def fingerprint[A](op: Tool[A]): String = op match
      case Tool.Call(c) => s"${c.name}(${Json.print(c.args)})"
    def withKey[A](op: Tool[A], key: String): Tool[A] = op match
      case Tool.Call(c) =>
        val args = c.args match
          case Json.JObj(fs) =>
            Json.JObj(fs.filterNot(_._1 == Durable.KeyField) :+ (Durable.KeyField, Json.JStr(key)))
          case _ => Json.JObj(Vector((Durable.KeyField, Json.JStr(key))))
        Tool.Call(c.copy(args = args))
    def perform[A](op: Tool[A], inner: Handler[Tool]): (A, String) = op match
      case Tool.Call(c) =>
        // rebuilt here, so the call is a `Tool[String]` and its answer
        // is a `String` — the point of `perform` existing at all
        val answer: String = inner.handle(Tool.Call(c))
        (answer, answer)
    def decode[A](op: Tool[A], written: String): A = op match
      case Tool.Call(_) => written
    override def asked[A](op: Tool[A]): Json = op match
      case Tool.Call(c) => c.args

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

  /**
   * Not a failure: the program asked something whose answer comes
   * from outside it, and is parked until that answer is journalled.
   *
   * A control transfer rather than an error, for the same reason
   * `Drift` and `Unresolved` are thrown from a handler that must
   * otherwise produce an `A`: `Handler.handle` has no way to leave
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

  /** the reserved argument a WithKey retry carries, so the far end
   * can deduplicate the second attempt against the first */
  val KeyField = "idempotency_key"

  private def fingerprintOf(c: ToolCall): String =
    summon[Journalled[Tool]].fingerprint(Tool.Call(c))

  /** a key that is stable across replays: the step's position and
   * what it asked for, nothing that varies per process */
  def keyFor(seq: Int, c: ToolCall): String =
    keyOf(c.name, seq, fingerprintOf(c))

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
  def over[Op[_]](inner: Handler[Op], journal: Journal)
                 (policy: String => OnRepeat = _ => OnRepeat.Fail,
                  // POLYMORPHIC, not `Op[?]`: an abstract type
                  // constructor cannot be applied to a wildcard, and
                  // the honest reading is that these answer for an
                  // operation of ANY answer type
                  reconcile: [X] => (Op[X], String) => Option[String] =
                    [X] => (_: Op[X], _: String) => None,
                  escalate: [X] => (Op[X], String) => Option[String] =
                    [X] => (_: Op[X], _: String) => None,
                  trace: Option[OpTrace] = None)
                 (using J: Journalled[Op])
  : Handler[Op] = new Handler[Op]:

    private var seq = 0
    private val recorded = journal.all

    def handle[A](op: Op[A]): A =
      val n = seq
      seq += 1
      val name = J.name(op)
      val fp = J.fingerprint(op)
      val key = keyOf(name, n, fp)

      // the overlay span: same identity as the journal entry (the
      // key), so a later replay lays over exactly this operation
      traced(trace, name, key, n) {
        recorded.find(_.seq == n) match
          // never ran: execute and journal, intent first — unless
          // the answer is a person's, in which case there is no
          // effect to run and the question is what gets recorded
          case None =>
            if policy(name) == OnRepeat.Await then park(n, op, name, fp, key)
            else execute(n, name, fp, key, op)

          case Some(entry) =>
            if entry.fingerprint != fp then throw Drift(entry.fingerprint, fp)
            entry.answer match
              // it already happened: hand the answer back, touch nothing
              case Some(a) => J.decode(op, a)

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
   * The durable TOOL handler: `over` at `Tool`, and the signature the
   * callers had before there was an `over`. Every behaviour is the
   * instance's — the `idempotency_key` field, the span attributes,
   * the `Awaiting` carrying the call's arguments — so this is a
   * spelling, not a second implementation.
   */
  def tools(inner: Handler[Tool], journal: Journal)
           (policy: String => OnRepeat = _ => OnRepeat.Fail,
            reconcile: (ToolCall, String) => Option[String] = (_, _) => None,
            escalate: (ToolCall, String) => Option[String] = (_, _) => None,
            trace: Option[OpTrace] = None)
  : Handler[Tool] =
    def onCall(f: (ToolCall, String) => Option[String]): [X] => (Tool[X], String) => Option[String] =
      [X] => (op: Tool[X], key: String) => op match { case Tool.Call(c) => f(c, key) }
    over[Tool](inner, journal)(policy, onCall(reconcile), onCall(escalate), trace)

  /**
   * Deterministic replay for its own sake: answer every call from the
   * journal and NEVER touch the world — a production incident run
   * again, offline, with no model and no side effects. The half of
   * durability that is worth as much as the recovery.
   */
  def replayingOver[Op[_]](journal: Journal, trace: Option[OpTrace] = None)
                          (using J: Journalled[Op]): Handler[Op] = new Handler[Op]:
    private var seq = 0
    private val recorded = journal.all
    def handle[A](op: Op[A]): A =
      val n = seq
      seq += 1
      val name = J.name(op)
      val fp = J.fingerprint(op)
      // replay=true: the overlay span is marked as the re-run, but
      // carries the SAME key, so it lands over the original
      traced(trace, name, keyOf(name, n, fp), n, replay = true) {
        recorded.find(_.seq == n) match
          case Some(entry) if entry.fingerprint != fp => throw Drift(entry.fingerprint, fp)
          case Some(Entry(_, _, _, _, Some(a))) => J.decode(op, a)
          case Some(entry) => throw Unresolved(name, entry.key)
          case None => throw Unresolved(name, "beyond the journal")
      }

  /** replay at `Tool`, the spelling the callers had */
  def replaying(journal: Journal, trace: Option[OpTrace] = None): Handler[Tool] =
    replayingOver[Tool](journal, trace)
}

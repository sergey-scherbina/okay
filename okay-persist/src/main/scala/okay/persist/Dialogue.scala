package okay.persist

import okay.{!, +, Delim, pure}
import okay.codec.Schema

/**
 * A PAUSED PROGRAM WHOSE JOURNAL IS A TOPIC (durable-dialogue,
 * 2026-09-17): the glue between `Delim.replay` and the durable log.
 *
 * `Delim.resumable` stops a program in the middle and hands the rest
 * of it back as a value; that value is a closure and does not survive
 * a restart, so what is kept is the JOURNAL — the answers given so
 * far — and where the program stands is re-derived by running it
 * again and feeding those answers back without asking. Here the
 * journal is a partition of a topic, so "so far" survives the
 * process.
 *
 * THIS IS EVENT SOURCING, with one difference worth the whole class:
 * the events are the ANSWERS (the discipline is that everything the
 * outside world tells the program enters through `pause`, so the
 * answers are the only non-determinism there is), the aggregate's
 * state is where the program stands — and the fold that rebuilds it
 * is THE PROGRAM ITSELF. There is no `apply(state, event)` to write,
 * keep in step with the code, and get wrong.
 *
 * ── WHAT A RECORD CARRIES, AND WHY (durable-workflow stage 0,
 * 2026-09-17). The first cut journalled a bare answer, and probing
 * found three ways that loses or corrupts a dialogue. All three are
 * fixed by the same envelope, so they are one change:
 *
 *   `program` — the identity of the code that wrote this record. A
 *   long-lived dialogue outlives deploys, and a program that gained a
 *   question reads the OLD answers onto the NEW questions silently:
 *   measured, a v2 booking read "Kyiv" as its promo code and carried
 *   on. A record whose program is not the reader's STOPS the fold and
 *   is named in `recovered.stopped` — an outage instead of a
 *   corruption, and specs/durable-workflow.md stage 2 is how the
 *   outage goes away (`patch`, Temporal's `getVersion`).
 *
 *   `expect` — how many answers the writer had accepted when it wrote
 *   this one. Two processes answering one dialogue both appended and
 *   the fold took both: one dialogue ended up with an answer nobody
 *   chose. The fold now accepts a record only at the position its
 *   writer expected; a loser is reported in `recovered.rejected` and
 *   changes nothing. This is optimistic concurrency in the
 *   PROJECTION, because `Topic.append` has no conditional form and
 *   giving it one would change every store, the wire protocol and the
 *   Kafka interop for one consumer. Damage is data; so is a lost
 *   race.
 *
 * ── THE ORDER: ADVANCE FIRST, THEN JOURNAL. The first cut appended
 * durably and then advanced, defending the order with "a crash in
 * that window replays to the same place; the other loses an answer
 * the outside world already acted on". Half right: an answer the
 * PROGRAM refuses was then committed forever, and every later process
 * replayed it and threw — the dialogue was dead and the answer could
 * not be corrected. The append now sits in the continuation of the
 * advance, so a program that throws on the answer never reaches it
 * and the journal is untouched. The remaining window (advanced, not
 * yet appended) replays to the same place and re-asks, which is the
 * safe direction.
 *
 * ── AT-LEAST-ONCE, STATED. `run` calls the oracle with an `Attempt`
 * carrying `(id, index)` — stable across restarts, and what an
 * idempotent external call needs. A crash between performing the call
 * and journalling its answer re-asks on restart; that is the contract
 * every workflow engine has, and the key is how a caller survives it.
 *
 * One dialogue = one key = one partition, the convention `Saga`
 * follows.
 *
 * COST, in two paths, because they are different problems
 * (dialogue-snapshots, 2026-09-17):
 *
 *   WARM — you are holding the program: `step(p, a)` advances it by
 *   one and journals, no replay. A drive that answers n questions
 *   costs O(n).
 *
 *   COLD — you have only the log: `answer(a)` replays, so it is
 *   O(answers so far). Give the dialogue a `Snapshots` and an
 *   interval and it writes CHAPTERS — a journal prefix and the offset
 *   it ends at — so a start reads one chapter plus the tail.
 *
 * What no snapshot can remove: the program is run once over the
 * answers to find out where it stands. That is exactly what "the fold
 * is the program" costs, and it is cheap in the shape this is for —
 * straight-line code between pauses. specs/durable-workflow.md stage
 * 3 (`continueAs`) is how it is bounded when it stops being cheap.
 */
final class Dialogue[Q, A, R, F[+_]](topic: Topic, val id: String,
                                     val program: String,
                                     snapshots: Option[Snapshots] = None,
                                     snapshotEvery: Int = 0,
                                     version: Int = 1,
                                     upcasts: Map[Int, Typed.Upcast] = Map.empty)
                                    (body: Delim.Asking[Q, A, R, Delim + F] ?=> R ! (Delim + F))
                                    (using Schema[A]):

  private val typed = Typed[Dialogue.Entry[A]](topic, version, upcasts)
  private val key = id.getBytes("UTF-8")
  private val partition = Topic.route(key, topic.partitions)

  /** what the log remembers, oldest first — with the races that lost
   * and the record that stopped the fold, if either happened */
  def recovered: Dialogue.Recovered[A] =
    val out = Vector.newBuilder[A]
    var accepted = 0
    var rejected = List.empty[Dialogue.Lost]
    var stopped: Option[Dialogue.Stopped] = None

    // the newest chapter, if one was written BY THIS PROGRAM: its
    // answers are the prefix and the log is read from just after it.
    // A chapter that does not decode, or that belongs to another
    // program, is ignored and the log is read from the start — the
    // log is the truth, a snapshot is only a shortcut.
    val chapter: Option[Dialogue.Chapter[A]] = snapshots.flatMap: snaps =>
      snaps.latestValue[Dialogue.Chapter[A]](key)
        .flatMap(_._2.toOption)
        .filter(_.program == program)
    chapter.foreach: c =>
      out ++= c.answers
      accepted = c.answers.size
    var from = chapter.map(_.upTo + 1).getOrElse(topic.begin(partition))
    var going = true
    while going do
      typed.read(partition, from, 256) match
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            for d <- rs if going do
              d match
                case Typed.Decoded.Ok(off, _, k, e) =>
                  if k.sameElements(key) then
                    e match
                      case Dialogue.Entry.Answered(p, _, _) if p != program =>
                        stopped = Some(Dialogue.Stopped.Mismatch(off, p, program))
                        going = false
                      case Dialogue.Entry.Answered(_, expect, a) =>
                        if expect == accepted then
                          out += a
                          accepted += 1
                        else rejected = rejected :+ Dialogue.Lost(off, expect, accepted)
                  if going then from = off + 1
                case Typed.Decoded.Bad(off, err) =>
                  stopped = Some(Dialogue.Stopped.Damage(off, err))
                  going = false
    seen = from
    Dialogue.Recovered(out.result().toList, rejected, stopped)

  /** the answers accepted so far; a stopped fold ends the journal
   * where it stopped, and `recovered` is how you see that it did */
  def journal: Delim.Journal[A] = recovered.answers

  /**
   * Where the program stands — itself, folded over its journal.
   *
   * It answers `Left` rather than throwing when the log cannot be
   * folded into a place at all: a bad deploy must be VISIBLE to
   * whoever asked, not an exception storm in a request handler. What
   * it does NOT catch is a program that throws while replaying a
   * journal it accepts — that exception belongs to the caller's
   * interpretation and reaches them where they ran it.
   */
  def at: Either[Dialogue.Stopped, Delim.Dialogue[Q, A, R, F]] ! F =
    val r = recovered
    r.stopped match
      case Some(s) => pure(Left(s))
      case None => Delim.replay[Q, A, R, F](body)(r.answers).map(Right(_))

  /**
   * THE COLD PATH. Answer the question it is asking, knowing only the
   * log: advance, and journal the answer IN THE CONTINUATION of the
   * advance, so a program that refuses the answer leaves the journal
   * untouched and can be answered again.
   *
   * O(answers so far), because it replays. Use it to take one step
   * from a standing start — a request arriving at a process that was
   * not holding this dialogue. If you ARE holding it, `step` is the
   * same move without the replay.
   */
  def answer(a: A): Dialogue.Answered[Q, A, R, F] ! F =
    // ONE fold, not three: `at` would re-read the log, and so would
    // asking it again for the position
    val r = recovered
    r.stopped match
      case Some(s) => pure(Dialogue.Answered.Broken(s))
      case None =>
        Delim.replay[Q, A, R, F](body)(r.answers)
          .flatMap(advance(_, a, r.answers.size))

  /**
   * THE WARM PATH. Advance a dialogue you are already holding: the
   * program in your hand takes ONE step and the answer is journalled
   * after it, so a drive that answers n questions costs O(n) rather
   * than O(n²) — which is the whole reason this exists beside
   * `answer`.
   *
   * `expect` is the position this answer will occupy; a holder of the
   * program knows it because it counted its own steps.
   */
  def step(p: Delim.Dialogue[Q, A, R, F], a: A, expect: Int)
          : Dialogue.Answered[Q, A, R, F] ! F =
    advance(p, a, expect)

  /** the one move both paths make: advance, then journal, then say
   * whether this writer's answer is the one the fold accepted */
  private def advance(p: Delim.Dialogue[Q, A, R, F], a: A, expect: Int)
                     : Dialogue.Answered[Q, A, R, F] ! F =
    p match
      case Delim.Paused.Done(_) => pure(Dialogue.Answered.NotAsking(p))
      case Delim.Paused.Ask(_, _) =>
        // the durable journal is the journal, so the in-memory one
        // this hands to `Delim.answer` is empty and its copy dropped
        Delim.answer(p, List.empty[A])(a).flatMap: (next, _) =>
          // ONLY HERE: the advance produced a value, so the answer is
          // one the program accepts. A throw above never reaches this.
          val before = seen
          val off = journalled(a, expect)
          if won(before, off) then
            seen = off + 1
            pure(Dialogue.Answered.Advanced(next))
          else at.map:
            case Right(actual) => Dialogue.Answered.Lost(actual)
            case Left(s) => Dialogue.Answered.Broken(s)

  /** write the journal so far as a chapter, so a cold start reads one
   * record and a tail instead of everything. Explicit, because a
   * snapshot is an optimisation a consumer opts into — the same
   * doctrine `Snapshots` states. */
  def snapshot(): Unit = snapshots.foreach: snaps =>
    val r = recovered
    if r.intact then
      val _ = snaps.putValue(key,
        Dialogue.Chapter(program, topic.end(partition) - 1, r.answers))

  private def journalled(a: A, expect: Int): Long =
    val off = typed.append(partition, key,
      Dialogue.Entry.Answered(program, expect, a), Ack.Durable)
    written += 1
    if snapshotEvery > 0 && written % snapshotEvery == 0 then snapshot()
    off

  /**
   * Did the fold take the record at this offset? A writer that lost a
   * race wrote a record the projection ignores, and this is how it
   * finds out without guessing.
   *
   * The first line is the whole point: an append that landed exactly
   * where this instance had read to cannot have been overtaken, so no
   * read happens at all. Only a gap — somebody else wrote in between
   * — costs a fold.
   */
  private def won(before: Long, off: Long): Boolean =
    if before >= 0 && off == before then true
    else !recovered.rejected.exists(_.offset == off)

  private var written = 0

  /**
   * The offset this instance has read up to. It is what makes the
   * won-the-race check free on the warm path: if the record this
   * writer just appended landed exactly where it had already read to,
   * NOBODY wrote in between — not another writer, not another
   * dialogue sharing the partition — so there is nothing to re-read.
   * Without it the check re-folds the whole journal per answer and
   * `run` is O(n squared) again, which is the regression
   * dialogue-snapshots had already paid to remove.
   */
  private var seen: Long = -1L

  /**
   * Run to the end, journaling every answer: `oracle` is what
   * actually talks to the outside world, and it is called once per
   * question — never for one the journal already answered.
   *
   * The `Attempt` is the idempotency key. A crash between the
   * oracle's call and the append re-asks that question on restart, so
   * an oracle with a side effect must be idempotent in `(id, index)`
   * — which is stable across restarts precisely because it is the
   * journal's own position.
   */
  def run(oracle: (Q, Dialogue.Attempt) => A ! F): R ! F =
    def go(p: Delim.Dialogue[Q, A, R, F], index: Int): R ! F = p match
      case Delim.Paused.Done(r) => pure(r)
      // the WARM path: the program is in hand, so no step replays
      case Delim.Paused.Ask(q, _) =>
        oracle(q, Dialogue.Attempt(id, index)).flatMap: a =>
          step(p, a, index).flatMap:
            case Dialogue.Answered.Advanced(next) => go(next, index + 1)
            case Dialogue.Answered.Lost(actual) => go(actual, index)
            case Dialogue.Answered.NotAsking(next) => go(next, index)
            case Dialogue.Answered.Broken(s) => throw Dialogue.Halted(s)
    val r = recovered
    r.stopped match
      case Some(s) => throw Dialogue.Halted(s)
      case None =>
        Delim.replay[Q, A, R, F](body)(r.answers).flatMap(go(_, r.answers.size))

object Dialogue:

  /**
   * WHAT A JOURNAL RECORD IS. Not a bare answer: the two fields
   * beside it are what make a long-lived dialogue safe against a
   * deploy and against a second writer. The class header says why.
   */
  enum Entry[A]:
    case Answered[A](program: String, expect: Int, a: A) extends Entry[A]

  object Entry:
    given [A](using Schema[A]): Schema[Entry[A]] = Schema.derived

  /** why the fold stopped: the log says something this reader must
   * not guess about */
  enum Stopped:
    /** a record that did not decode */
    case Damage(offset: Long, error: String)
    /** a record written by a DIFFERENT program. Folding it would map
     * old answers onto new questions silently, which is the one
     * failure that corrupts rather than stops. */
    case Mismatch(offset: Long, found: String, expected: String)

  /** a record whose writer expected to be at another position: it
   * lost a race and the fold ignores it */
  final case class Lost(offset: Long, expect: Int, had: Int)

  /** what an attempt to answer did */
  enum Answered[Q, A, R, F[+_]]:
    /** this writer's answer was accepted; here is where it stands */
    case Advanced[Q, A, R, F[+_]](to: Delim.Dialogue[Q, A, R, F]) extends Answered[Q, A, R, F]
    /** another writer got there first; `to` is where it ACTUALLY
     * stands, which is what the caller needs to decide what to do */
    case Lost[Q, A, R, F[+_]](to: Delim.Dialogue[Q, A, R, F]) extends Answered[Q, A, R, F]
    /** nobody was asking, so nothing was written */
    case NotAsking[Q, A, R, F[+_]](to: Delim.Dialogue[Q, A, R, F]) extends Answered[Q, A, R, F]
    /** the log could not be folded into a place at all */
    case Broken[Q, A, R, F[+_]](why: Stopped) extends Answered[Q, A, R, F]

  /** the oracle's idempotency key: stable across restarts, because it
   * is the journal's own position */
  final case class Attempt(id: String, index: Int)

  /** `run` met a log it cannot fold; a driver has nowhere to put an
   * `Answered.Broken`, so it says so loudly */
  final class Halted(val why: Stopped)
    extends RuntimeException(s"dialogue halted: $why")

  /**
   * A JOURNAL PREFIX, WRITTEN DOWN. A continuation cannot be
   * snapshotted — it is a closure — so what a chapter holds is the
   * answers up to `upTo` and nothing derived from them. A cold start
   * then reads one record plus whatever arrived after it.
   *
   * It carries the program it was written by for the same reason a
   * record does: a chapter from another program is a shortcut to the
   * wrong place, and is ignored.
   */
  final case class Chapter[A](program: String, upTo: Long, answers: List[A])

  object Chapter:
    given [A](using Schema[A]): Schema[Chapter[A]] = Schema.derived

  /** the journal, the races that lost, and what stopped the fold */
  final case class Recovered[A](answers: List[A],
                                rejected: List[Lost],
                                stopped: Option[Stopped]):
    def intact: Boolean = stopped.isEmpty

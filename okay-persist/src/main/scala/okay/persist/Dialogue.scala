package okay.persist

import okay.{!, +, At, Delim, Replayable, Row, Wf, pure}
import okay.Row.up
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
 * THE DISCIPLINE IS A CONSTRAINT, not a hope
 * (dialogue-replay-discipline): `Replayable[Delim + F]` is required
 * here, so a body that performs an `Async` effect between two pauses
 * does not compile as a durable dialogue. The sentence the whole
 * design rests on — everything the outside world tells the program
 * enters through `pause` — is now checked where it is relied upon.
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
final class Dialogue[Q, A, R, F[+_]] private (topic: Topic, val id: String,
                                     val program: String,
                                     snapshots: Option[Snapshots],
                                     snapshotEvery: Int,
                                     version: Int,
                                     upcasts: Map[Int, Typed.Upcast],
                                     /**
                                      * HOW A JOURNAL BECOMES A PLACE, and the only
                                      * thing about this class that the program's
                                      * shape decides (wf-durable-journal,
                                      * 2026-09-17). An ordinary dialogue folds with
                                      * `Delim.replay`; a WORKFLOW folds with
                                      * `Wf.replay`, which knows not to feed an
                                      * answer to a `patch` that was not there when
                                      * the journal was written. Everything else —
                                      * the envelope, the races, the order of
                                      * advance and append — is the same for both,
                                      * so it is written once.
                                      */
                                     place: Delim.Journal[A] => Delim.Dialogue[Q, A, R, F] ! F)
                                    (using Schema[A]):

  private val typed = Typed[Dialogue.Entry[A]](topic, version, upcasts)
  private val key = id.getBytes("UTF-8")
  private val partition = Topic.route(key, topic.partitions)

  /** what the log remembers, oldest first — with the races that lost
   * and the record that stopped the fold, if either happened */
  def recovered: Dialogue.Recovered[A] =
    var out = Vector.empty[A]
    // RECORDS accepted, which is no longer the same as answers held:
    // a `Continued` resets the journal and does NOT reset this. That
    // is what keeps `expect` sound across a continuation — a writer
    // still standing in the old chapter carries a number this one has
    // already passed, so its record is rejected instead of being read
    // onto a question that is not the one it answered.
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
      out = c.answers.toVector
      accepted = c.accepted
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
                    val wrote = e match
                      case Dialogue.Entry.Answered(p, _, _) => p
                      case Dialogue.Entry.Continued(p, _, _) => p
                    if wrote != program then
                      stopped = Some(Dialogue.Stopped.Mismatch(off, wrote, program))
                      going = false
                    else e match
                      case Dialogue.Entry.Answered(_, expect, a) =>
                        if expect == accepted then
                          out = out :+ a
                          accepted += 1
                        else rejected = rejected :+ Dialogue.Lost(off, expect, accepted)
                      case Dialogue.Entry.Continued(_, expect, seed) =>
                        // EVERYTHING BEFORE THIS IS SUPERSEDED. The
                        // journal becomes the seed alone, so replay
                        // costs one answer however long the history
                        // was; the record count carries on.
                        if expect == accepted then
                          out = Vector(seed)
                          accepted += 1
                        else rejected = rejected :+ Dialogue.Lost(off, expect, accepted)
                  if going then from = off + 1
                case Typed.Decoded.Bad(off, err) =>
                  stopped = Some(Dialogue.Stopped.Damage(off, err))
                  going = false
    seen = from
    Dialogue.Recovered(out.toList, accepted, rejected, stopped)

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
      case None => place(r.answers).map(Right(_))

  /**
   * WHY THE FOLD STOPPED, POINTING AT A LINE (delim-diagnostics-
   * position, 2026-09-17).
   *
   * `Stopped` names an OFFSET, which says where in the log the
   * trouble is and nothing about the code. What an operator woken at
   * three in the morning needs is a LINE.
   *
   * ── THE POSITION DOES NOT TRAVEL IN THE JOURNAL, and the spec
   * assumed it would have to. It does not, because the READER holds
   * the body: replaying the accepted prefix puts this program at the
   * question the bad record was supposed to answer, and `At` has been
   * on every `pause` since delim-diagnostics. So this costs one fold
   * and no format change — no new field, no version bump, no upcast
   * for every journal ever written.
   *
   * ── AND IT IS THE MORE USEFUL LINE ANYWAY. Carrying the WRITER's
   * position would name the code that wrote the record, which is the
   * deploy that already went out and worked. The line that helps is
   * the one in the program that CANNOT fold this journal — the reader
   * is the broken one, and the reader is who is reading this.
   *
   * `None` when nothing stopped. The question is rendered with
   * `toString`, the same way `Statuses` renders it.
   */
  def diagnosis: Option[Dialogue.Diagnosis] ! F =
    val r = recovered
    r.stopped match
      case None => pure(None)
      case Some(why) => place(r.answers).map: p =>
        Some(Dialogue.Diagnosis(why, r.answers.size, p.asking.map(_.toString), p.where))

  /**
   * WHERE IT STANDS AND HOW MANY RECORDS PUT IT THERE, in ONE fold
   * (dialogue-resume-cache, 2026-09-17). `at` answers the first half
   * and `recovered.accepted` the second, and a caller that wants both
   * — every driver does — would otherwise fold twice and could see
   * the two halves disagree if somebody appended in between. The
   * index is what `step` needs, so handing them out together is the
   * shape that cannot be got wrong.
   */
  def standing: Either[Dialogue.Stopped, (Delim.Dialogue[Q, A, R, F], Int)] ! F =
    val r = recovered
    r.stopped match
      case Some(s) => pure(Left(s))
      case None => place(r.answers).map(p => Right((p, r.accepted)))

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
        place(r.answers).flatMap(advance(_, a, r.accepted))

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
      case Delim.Paused.Ask(_, _, _) =>
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
        Dialogue.Chapter(program, topic.end(partition) - 1, r.accepted, r.answers))

  /**
   * CLOSE THIS CHAPTER AND START THE NEXT (dialogue-continue-as,
   * 2026-09-17). `seed` becomes the journal's only answer, so the
   * next replay runs the program over one answer instead of the whole
   * history.
   *
   * `true` means this writer's continuation is the one the fold took.
   * Two workers that both decide to continue from the same position
   * write two records and exactly one wins, by the same `expect` the
   * answers use — the loser's number is already behind and its record
   * changes nothing.
   *
   * It does NOT drive the program: what to do with a fresh chapter is
   * the worker's business, and `Worker`'s `seedOf` is what turns a
   * program's `Next.Continue` into this call.
   */
  def continueAs(seed: A, expect: Int): Boolean =
    val before = seen
    val off = typed.append(partition, key,
      Dialogue.Entry.Continued(program, expect, seed), Ack.Durable)
    written += 1
    val ok = won(before, off)
    if ok then seen = off + 1
    ok

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

  /**
   * HAS ANYBODY WRITTEN HERE SINCE THIS INSTANCE LAST READ
   * (dialogue-resume-cache, 2026-09-17)? One offset read, not a fold
   * — which is the whole point, because a cache that had to fold to
   * find out whether it was stale would have paid the cost it exists
   * to avoid.
   *
   * CONSERVATIVE BY CONSTRUCTION: it asks about the PARTITION, so
   * another dialogue sharing it makes this say "disturbed" when this
   * dialogue was not. The cost of a false "yes" is one replay — the
   * behaviour without a cache at all — and the cost of a false "no"
   * would be a program that has missed an answer. Only one of those
   * two errors is affordable, so the check leans that way.
   */
  def undisturbed: Boolean = seen >= 0 && topic.end(partition) == seen

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
    runUntil[Nothing]((q, at) => oracle(q, at).map(Right(_))).map:
      case Right(r) => r
      case Left(never) => never

  /**
   * THE SAME DRIVE, BUT IT MAY STOP (workflow-suspended-driver,
   * 2026-09-17). An oracle that answers `Left(s)` is saying "nobody
   * here can answer this" — a durable timer that has not fired, a
   * signal nobody has sent, a child still running — and the drive
   * ENDS there and hands `s` back, having journalled everything it
   * did answer.
   *
   * It is one generalisation rather than a second driver because the
   * stopping is the ONLY difference: the warm path, the `expect`
   * race check and the append-after-advance order are the same moves
   * either way.
   */
  def runUntil[S](oracle: (Q, Dialogue.Attempt) => Either[S, A] ! F): Either[S, R] ! F =
    runUntilIn[S, F](oracle)

  /**
   * THE DRIVE'S ROW IS NOT THE PROGRAM'S (workflow-activity-row,
   * 2026-09-17), and the distinction is the whole difference between
   * a model and an engine.
   *
   * A durable program's row must be `Replayable` — no `Async`, no
   * `Writer`, nothing a replay would perform again. But the ORACLE is
   * the half that DOES reach outside: it calls the service, charges
   * the card, writes the file. Sharing one row made the oracle as
   * constrained as the program, so an activity could only do I/O by
   * side-effecting in Scala, past the effect system entirely — which
   * this library exists not to do.
   *
   * So the driver runs in `F + E`: the program's replayable row, plus
   * whatever the activities need. The programs the driver moves are
   * still built in `F` and widened at the seam; the journal, the
   * replay and the race check never see `E`.
   *
   * ONE ROW FOR THE DRIVER, licensed by `Row.Sub` — and getting
   * there took two attempts, both recorded because the second is only
   * defensible against the first. The natural `In[F, G]` CRASHES
   * dotty 3.9 when both rows are abstract (`orDominator`, "Failure to
   * join alternatives F and G"; `ProbeRowCrash` pins it). The
   * complement form `F + E` compiles but cannot express a driver with
   * no effects at all — `Pure + Pure` is `[X] =>> Nothing | Nothing`,
   * which is not `Nothing`, so `!.run` refuses it. `Sub` is
   * membership as SUBTYPING: it resolves where `In` crashes, and
   * `Nothing <:< anything` means a program with no operations rides
   * into any row, so `G = F` and `F = Pure` both work.
   */
  def runUntilIn[S, G[+_]](oracle: (Q, Dialogue.Attempt) => Either[S, A] ! G)
                          (using Row.Sub[F, G]): Either[S, R] ! G =
    val r = recovered
    r.stopped match
      case Some(s) => throw Dialogue.Halted(s)
      case None =>
        place(r.answers).up[G].flatMap(runFromIn[S, G](_, r.accepted)(oracle))
          .map(_._1)

  /**
   * THE WARM PATH, ACROSS CALLS (dialogue-resume-cache, 2026-09-17).
   *
   * `runUntilIn` folds the journal to find the program and then walks
   * warm; this is the walk alone, over a program somebody already
   * holds. It hands back where it ENDED as well as what it produced,
   * because a caller that means to come back needs both — the program
   * and the position, which is what `step` wants next time.
   *
   * THE CALLER OWES THE VALIDITY. This will happily drive a program
   * that no longer matches the log, which is why `Resume` checks
   * `undisturbed` first and why that check is conservative. Written
   * as a separate door rather than a flag so the obligation is
   * visible at the call site.
   */
  def runFromIn[S, G[+_]](from: Delim.Dialogue[Q, A, R, F], at: Int)
                         (oracle: (Q, Dialogue.Attempt) => Either[S, A] ! G)
                         (using Row.Sub[F, G])
                         : (Either[S, R], Delim.Dialogue[Q, A, R, F], Int) ! G =
    def go(p: Delim.Dialogue[Q, A, R, F], index: Int)
          : (Either[S, R], Delim.Dialogue[Q, A, R, F], Int) ! G = p match
      case Delim.Paused.Done(r) => pure((Right(r), p, index))
      // the WARM path: the program is in hand, so no step replays
      case Delim.Paused.Ask(q, _, _) =>
        oracle(q, Dialogue.Attempt(id, index)).flatMap:
          case Left(s) => pure((Left(s), p, index))
          case Right(a) =>
            step(p, a, index).up[G].flatMap:
              case Dialogue.Answered.Advanced(next) => go(next, index + 1)
              case Dialogue.Answered.Lost(actual) => go(actual, index)
              case Dialogue.Answered.NotAsking(next) => go(next, index)
              case Dialogue.Answered.Broken(s) => throw Dialogue.Halted(s)
    go(from, at)

object Dialogue:

  /**
   * AN ORDINARY DURABLE DIALOGUE: the author's questions, answered by
   * the author's oracle, folded with `Delim.replay`.
   */
  def apply[Q, A, R, F[+_]](topic: Topic, id: String, program: String,
                            snapshots: Option[Snapshots] = None,
                            snapshotEvery: Int = 0,
                            version: Int = 1,
                            upcasts: Map[Int, Typed.Upcast] = Map.empty)
                           (body: Delim.Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                           (using Schema[A], Replayable[Delim + F],
                            Delim.OneMachine[F], At): Dialogue[Q, A, R, F] =
    new Dialogue(topic, id, program, snapshots, snapshotEvery, version, upcasts,
      j => Delim.replay[Q, A, R, F](body)(j))

  /**
   * A DURABLE WORKFLOW: the same journal, but it also carries the
   * LIBRARY's questions (`Wf.now`, `uuid`, `random`, `patch`), so a
   * durable program may have a clock and a changeable branch without
   * breaking replay. The fold is `Wf.replay`, which is what makes
   * `patch` right on a journal written before the branch existed.
   *
   * `run` takes the author's oracle; the runtime's questions are
   * answered here and journalled beside the author's, tagged.
   */
  def workflow[Q, A, R, F[+_]](topic: Topic, id: String, program: String,
                               snapshots: Option[Snapshots] = None,
                               snapshotEvery: Int = 0,
                               version: Int = 1,
                               upcasts: Map[Int, Typed.Upcast] = Map.empty)
                              (body: Wf.Asks[Q, A, R, F] ?=> R ! Delim + F)
                              (using Schema[Wf.Ans[A]], Replayable[Delim + F],
                               Delim.OneMachine[F], At)
                              : Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F] =
    new Dialogue(topic, id, program, snapshots, snapshotEvery, version, upcasts,
      j => Wf.replay[Q, A, R, F](body)(j))

  /**
   * The oracle a workflow's drive wants: the author answers their own
   * questions, the runtime answers its own — and when the runtime
   * DECLINES (a timer, a signal, a child), the drive stops and says
   * what it is waiting on.
   */
  def asking[Q, A, F[+_]](oracle: Q => Attempt ?=> A ! F)(using rt: Wf.Runtime)
                         : (Wf.Ask[Q], Attempt) => Either[Wf.Wait, Wf.Ans[A]] ! F =
    (q, at) => q match
      case Left(sys) => rt.answer(sys) match
        case Left(w) => pure(Left(w))
        case Right(sa) => pure(Right(Left(sa)))
      // THE KEY REACHES THE ORACLE (worker-oracle-attempt,
      // 2026-09-17). It used to be dropped here — `(q, _)` — so the
      // idempotency key this class documents could not be used by
      // anybody driving through a `Worker`. It arrives as CONTEXT, so
      // an oracle that does not want it is written exactly as before.
      case Right(own) => oracle(own)(using at).map(v => Right(Right(v)))

  /** the same, for an oracle whose activities live in a wider row */
  def askingIn[Q, A, G[+_]](oracle: Q => Attempt ?=> A ! G)(using rt: Wf.Runtime)
                           : (Wf.Ask[Q], Attempt) => Either[Wf.Wait, Wf.Ans[A]] ! G =
    (q, at) => q match
      case Left(sys) => rt.answer(sys) match
        case Left(w) => pure(Left(w))
        case Right(sa) => pure(Right(Left(sa)))
      case Right(own) => oracle(own)(using at).map(v => Right(Right(v)))

  /**
   * DRIVE A DURABLE WORKFLOW as far as it goes: `Right` is its
   * answer, `Left` is what has to happen before anybody can carry it
   * further. Everything it DID answer is already in the log, so the
   * next process starts where this one stopped.
   */
  extension [Q, A, R, F[+_]](d: Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F])
    def runWorkflow(oracle: Q => Dialogue.Attempt ?=> A ! F)
                   (using Wf.Runtime): Either[Wf.Wait, R] ! F =
      d.runUntil[Wf.Wait](asking(oracle))

    /** the same, with the activities in their own row: the program
     * stays replayable, the oracle may reach outside */
    def runWorkflowIn[G[+_]](oracle: Q => Dialogue.Attempt ?=> A ! G)
                            (using Wf.Runtime, Row.Sub[F, G]): Either[Wf.Wait, R] ! G =
      d.runUntilIn[Wf.Wait, G](askingIn(oracle))

    /**
     * THE SAME DRIVE, FROM A PROGRAM IN HAND, saying where it ended
     * (dialogue-resume-cache, 2026-09-17). Both halves are what a
     * `Resume` needs: it hands the program in and takes the next one
     * back, so a process answering one dialogue n times replays it
     * once rather than n times.
     *
     * The caller owes the validity of `from` — see `Dialogue.runFromIn`.
     */
    def runWorkflowFromIn[G[+_]](from: Delim.Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], at: Int)
                                (oracle: Q => Dialogue.Attempt ?=> A ! G)
                                (using Wf.Runtime, Row.Sub[F, G])
        : (Either[Wf.Wait, R], Delim.Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], Int) ! G =
      d.runFromIn[Wf.Wait, G](from, at)(askingIn(oracle))

  /**
   * WHAT A JOURNAL RECORD IS. Not a bare answer: the two fields
   * beside it are what make a long-lived dialogue safe against a
   * deploy and against a second writer. The class header says why.
   */
  enum Entry[A]:
    case Answered[A](program: String, expect: Int, a: A) extends Entry[A]
    /**
     * THE END OF A CHAPTER (dialogue-continue-as, 2026-09-17).
     * Everything before it is superseded and the journal restarts
     * with `seed` as its only answer, so a cold start replays one
     * answer however long the history was. It carries the same
     * envelope as an answer and for the same reasons: a continuation
     * written by another program stops the fold, and two writers that
     * both decide to continue produce ONE new chapter, because the
     * loser's `expect` no longer matches.
     */
    case Continued[A](program: String, expect: Int, seed: A) extends Entry[A]

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

  /**
   * A STOPPED FOLD, WITH THE READER'S OWN POSITION IN IT
   * (delim-diagnostics-position, 2026-09-17). `why` and `offset` come
   * from the log; `asking` and `where` come from running this
   * program over the part of the journal it DID accept, so the line
   * named is the line in the code that cannot read the rest.
   */
  final case class Diagnosis(why: Stopped, accepted: Int,
                             asking: Option[String], where: Option[String]):
    /** one line for a log or a page */
    override def toString: String =
      val place = where.getOrElse("an unknown position")
      val q = asking.map(a => s" asking $a").getOrElse("")
      s"$why after $accepted answer(s); this program is at $place$q"

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
  final case class Chapter[A](program: String, upTo: Long,
                              accepted: Int, answers: List[A])

  object Chapter:
    given [A](using Schema[A]): Schema[Chapter[A]] = Schema.derived

  /** the journal, the races that lost, and what stopped the fold */
  final case class Recovered[A](answers: List[A],
                                /** records the fold accepted, which
                                 * is what `expect` counts. Equal to
                                 * `answers.size` until a `Continued`
                                 * makes it larger — see the fold. */
                                accepted: Int,
                                rejected: List[Lost],
                                stopped: Option[Stopped]):
    def intact: Boolean = stopped.isEmpty

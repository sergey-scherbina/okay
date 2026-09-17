package okay

/**
 * A DURABLE PROGRAM'S OWN NON-DETERMINISM (dialogue-asks, stage 1b of
 * specs/durable-workflow.md).
 *
 * `Replayable` made the discipline a type: a program you intend to
 * replay may not reach outside except through `pause`. That is
 * correct and, on its own, unusable — every real workflow needs a
 * clock, an id, sometimes a die, and refusing them is not an answer.
 *
 * The answer is that those are QUESTIONS TOO, asked of the runtime
 * instead of the author's oracle, and remembered in the same journal.
 * `Wf.now()` on a first run reads the clock and writes the reading
 * down; on every replay it reads the writing. The program stays a
 * pure function of its journal, which is the only thing that makes
 * replay exact.
 *
 * WHY THE `Either`. A dialogue's question type belongs to its author,
 * so "give me the clock" has nowhere to live in it. The channel is
 * therefore a sum the LIBRARY owns — `Either[Sys, Q]` for questions,
 * `Either[SysA, A]` for answers — and the author never writes it:
 * `Wf.pause(q)` wraps and unwraps `Right`, `Wf.now()` wraps and
 * unwraps `Left`. The two alternatives were weighed in the spec; the
 * decisive one is below.
 *
 * WHAT THE TAG BUYS, AND IT IS THE WHOLE OF `patch`. A journal entry
 * now SAYS whether it answers a library question or an author's. That
 * single bit makes a program CHANGEABLE:
 *
 *   - replaying, the pending question is `Patch(id)` and the next
 *     entry is a `Left(Flag(b))` for it — use `b`;
 *   - replaying, the pending question is `Patch(id)` and the next
 *     entry is a `Right` — the run that wrote this journal did not
 *     have this patch, so the answer is `false` and the entry is NOT
 *     consumed (it still answers the question it was written for);
 *   - live — the answer is `true`, and it is recorded.
 *
 * That is Temporal's `getVersion`, and here it falls out of the
 * tagging rather than being a rule a driver has to remember. Without
 * the tag, an old journal's next answer would be eaten by a question
 * that did not exist when it was written — the same silent
 * mis-mapping the `program` field of `okay.persist.Dialogue.Entry`
 * exists to stop.
 */
object Wf:

  /** the questions the RUNTIME answers, not the author's oracle */
  enum Sys:
    case Now
    case Uuid
    case Random
    /** is this branch on, for THIS run? (Temporal's getVersion) */
    case Patch(id: String)

  /** their answers, tagged so a journal entry says what it answers */
  enum SysA:
    case Millis(v: Long)
    case Text(v: String)
    case Dice(v: Double)
    case Flag(v: Boolean)

  /** the question a durable program asks: the library's, or its own */
  type Ask[Q] = Either[Sys, Q]

  /** the answer it is given, tagged the same way */
  type Ans[A] = Either[SysA, A]

  /** what the author writes as the body's context */
  type Asking[Q, A, R, F[+_]] = Delim.Asking[Ask[Q], Ans[A], R, Delim + F]

  /** where such a program stands */
  type Paused[Q, A, R, F[+_]] = Delim.Dialogue[Ask[Q], Ans[A], R, F]

  /** the journal of such a program */
  type Journal[A] = List[Ans[A]]

  /** a driver answered a question with the wrong KIND of answer —
   * always a bug in the driver, never in the program */
  final class Mismatched(q: Any, a: Any)
    extends RuntimeException(s"a $q was answered with $a")

  // ── the author's doors ───────────────────────────────────────────

  /**
   * Ask the outside world, through the author's own question type —
   * `Delim.pause` with the tag put on and taken off.
   */
  def pause[Q, A, R, F[+_]](q: Q)(using s: Asking[Q, A, R, F], at: At): A ! (Delim + F) =
    Delim.ask[Ask[Q], Ans[A], R, F](Right(q)).map:
      case Right(a) => a
      case other => throw Mismatched(q, other)

  /**
   * THE SAME THING UNDER THE NAME THE LITERATURE USES. An "activity"
   * in a workflow engine is a command performed outside and a result
   * remembered; here that is a question and its answer, and there is
   * no second mechanism. Naming it is the whole feature — the spec
   * listed `perform` as work, and the work turned out to be already
   * done.
   */
  def perform[Q, A, R, F[+_]](cmd: Q)(using Asking[Q, A, R, F], At): A ! (Delim + F) =
    pause(cmd)

  /** the wall clock, once, remembered */
  def now[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): Long ! (Delim + F) =
    sys[Q, A, R, F, Long](Sys.Now):
      case SysA.Millis(v) => v

  /** a fresh id, once, remembered */
  def uuid[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): String ! (Delim + F) =
    sys[Q, A, R, F, String](Sys.Uuid):
      case SysA.Text(v) => v

  /** a die, once, remembered */
  def random[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): Double ! (Delim + F) =
    sys[Q, A, R, F, Double](Sys.Random):
      case SysA.Dice(v) => v

  /**
   * IS THIS BRANCH ON FOR THIS RUN? A dialogue started before the
   * branch existed answers `false` for ever and finishes the way it
   * began; one started after answers `true`. Both decisions live in
   * the journal, so every process agrees.
   */
  def patch[Q, A, R, F[+_]](id: String)
                           (using s: Asking[Q, A, R, F], at: At): Boolean ! (Delim + F) =
    sys[Q, A, R, F, Boolean](Sys.Patch(id)):
      case SysA.Flag(v) => v

  /** one library question, and the shape of answer it accepts — the
   * partial function IS the expected shape, so a driver that answers
   * a clock with a die is a loud `Mismatched` rather than a cast */
  private def sys[Q, A, R, F[+_], X](q: Sys)(f: PartialFunction[SysA, X])
                                    (using s: Asking[Q, A, R, F], at: At): X ! (Delim + F) =
    Delim.ask[Ask[Q], Ans[A], R, F](Left(q)).map:
      case Left(a) if f.isDefinedAt(a) => f(a)
      case other => throw Mismatched(q, other)

  // ── the runtime's side ───────────────────────────────────────────

  /** what answers the LIBRARY's questions. The default reaches for
   * the real clock; a test hands over a scripted one and gets a
   * deterministic run without touching the program. */
  trait Runtime:
    def answer(q: Sys): SysA

  object Runtime:
    /** the real world */
    given live: Runtime with
      def answer(q: Sys): SysA = q match
        case Sys.Now => SysA.Millis(System.currentTimeMillis())
        case Sys.Uuid => SysA.Text(java.util.UUID.randomUUID().toString)
        case Sys.Random => SysA.Dice(scala.util.Random.nextDouble())
        case Sys.Patch(_) => SysA.Flag(true)   // live: the branch is on

    /** a fixed one, for a test that wants to read its own output */
    def scripted(millis: Long, id: String, dice: Double): Runtime = new Runtime:
      def answer(q: Sys): SysA = q match
        case Sys.Now => SysA.Millis(millis)
        case Sys.Uuid => SysA.Text(id)
        case Sys.Random => SysA.Dice(dice)
        case Sys.Patch(_) => SysA.Flag(true)

  /** start a program that may ask the runtime as well as the world */
  def resumable[Q, A, R, F[+_]](body: Asking[Q, A, R, F] ?=> R ! (Delim + F))
                               (using Delim.OneMachine[F], At): Paused[Q, A, R, F] ! F =
    Delim.resumable[Ask[Q], Ans[A], R, F](body)

  /**
   * Run to the end: the LIBRARY's questions are answered by the
   * runtime and the author's by the oracle, and every answer is
   * handed back so the caller can journal it. The pair is what a
   * durable driver appends.
   */
  def drive[Q, A, R, F[+_]](p: Paused[Q, A, R, F])(oracle: Q => A ! F)
                           (using rt: Runtime, om: Delim.OneMachine[F])
                           : (R, Journal[A]) ! F =
    def go(p: Paused[Q, A, R, F], acc: Journal[A]): (R, Journal[A]) ! F = p match
      case Delim.Paused.Done(r) => pure((r, acc))
      case Delim.Paused.Ask(Left(q), _, _) =>
        val a = Left(rt.answer(q))
        Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a).flatMap((next, _) => go(next, acc :+ a))
      case Delim.Paused.Ask(Right(q), _, _) =>
        oracle(q).flatMap: v =>
          val a = Right(v)
          Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a).flatMap((next, _) => go(next, acc :+ a))
    go(p, Nil)

  /**
   * Where a program stands, from its journal — and the ONE place the
   * tag earns the `Either`. A `Patch` whose decision is not in the
   * journal, on a journal that is not yet exhausted, was written by a
   * run that did not have this branch: it answers `false` and does
   * NOT eat the entry, which still answers the question it was
   * written for.
   */
  def replay[Q, A, R, F[+_]](body: Asking[Q, A, R, F] ?=> R ! (Delim + F))
                            (j: Journal[A])
                            (using Delim.OneMachine[F], Replayable[Delim + F], At)
                            : Paused[Q, A, R, F] ! F =
    def go(p: Paused[Q, A, R, F], left: Journal[A]): Paused[Q, A, R, F] ! F = p match
      case Delim.Paused.Done(_) => pure(p)
      case Delim.Paused.Ask(q, _, _) =>
        (q, left) match
          case (_, Nil) => pure(p)                      // caught up: it is live now
          case (Left(Sys.Patch(_)), (r @ Right(_)) :: _) =>
            // the journal has no decision here, and its next entry
            // answers something else: this run predates the branch
            val _ = r
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(Left(SysA.Flag(false)))
              .flatMap((next, _) => go(next, left))
          case (_, a :: rest) =>
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a).flatMap((next, _) => go(next, rest))
    resumable[Q, A, R, F](body).flatMap(go(_, j))

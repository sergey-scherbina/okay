package okay2.persist

import okay2.{!, +, At, Replayable, Row, Shift, Wf, pure}
import okay2.codec.Schema

/**
 * A PAUSED PROGRAM WHOSE JOURNAL IS A TOPIC (okay-persist's
 * Dialogue.scala; durable-dialogue, 2026-09-17): the glue between
 * `Shift.replay` and the durable log. A continuation is a closure and
 * does not survive a restart, so what is kept is the JOURNAL — the
 * answers given so far — and where the program stands is re-derived by
 * running it again over them. Here the journal is a partition of a
 * topic, so "so far" survives the process.
 *
 * THIS IS EVENT SOURCING whose events are the ANSWERS and whose fold is
 * THE PROGRAM ITSELF: there is no `apply(state, event)` to keep in step
 * with the code.
 *
 * A record carries `program` — the identity of the code that wrote it; a
 * record whose program is not the reader's STOPS the fold
 * (`recovered.stopped`), an outage instead of a corruption — and
 * `expect`, how many answers its writer had accepted: the fold accepts a
 * record only at the position its writer expected, so of two processes
 * answering one dialogue exactly one wins (optimistic concurrency in the
 * projection; `recovered.rejected` names the loser).
 *
 * ADVANCE FIRST, THEN JOURNAL: the append sits in the continuation of
 * the advance, so a program that throws on an answer never commits it.
 * `run` calls the oracle with an `Attempt` `(id, index)`, stable across
 * restarts — the key an idempotent external call needs, because a crash
 * between the call and its append re-asks.
 *
 * `Replayable[Shift[Any] + F]` is required, so a body that performs an
 * effect between two pauses does not compile as a durable dialogue. One
 * dialogue = one key = one partition.
 *
 * WARM — `step(p, a)` advances a program in hand by one and journals,
 * O(n) for n answers. COLD — `answer(a)` replays, O(answers so far);
 * with a `Snapshots` and an interval it writes CHAPTERS so a start reads
 * one chapter plus the tail.
 */
final class Dialogue[Q, A, R, F <: Row] private (topic: Topic, val id: String,
                                                 val program: String,
                                                 snapshots: Option[Snapshots],
                                                 snapshotEvery: Int,
                                                 version: Int,
                                                 upcasts: Map[Int, Typed.Upcast],
                                                 /** HOW A JOURNAL BECOMES A PLACE: `Shift.replay`
                                                  * for an ordinary dialogue, `Wf.replay` for a
                                                  * workflow (which knows not to feed an answer to
                                                  * a `patch` that was not there when the journal
                                                  * was written) */
                                                 place: Shift.Journal[A] => Shift.Dialogue[Q, A, R, F] ! F)
                                                (implicit sa: Schema[A], om: Shift.Machine[F]) {

  private val typed = Typed[Dialogue.Entry[A]](topic, version, upcasts)
  private val key = id.getBytes("UTF-8")
  private val partition = Topic.route(key, topic.partitions)

  /** what the log remembers, oldest first — with the races that lost and
   * the record that stopped the fold, if either happened */
  def recovered: Dialogue.Recovered[A] = {
    var out = Vector.empty[A]
    // RECORDS accepted, not answers held: a `Continued` resets the
    // journal and does NOT reset this, which keeps `expect` sound across
    // a continuation
    var accepted = 0
    var rejected = List.empty[Dialogue.Lost]
    var stopped: Option[Dialogue.Stopped] = None

    // the newest chapter, if one was written BY THIS PROGRAM; one that
    // does not decode or belongs to another program is ignored — the log
    // is the truth, a snapshot only a shortcut
    val chapter: Option[Dialogue.Chapter[A]] = snapshots.flatMap { snaps =>
      snaps.latestValue[Dialogue.Chapter[A]](key).flatMap(_._2.toOption).filter(_.program == program)
    }
    chapter.foreach { c =>
      out = c.answers.toVector
      accepted = c.accepted
    }
    var from = chapter.map(_.upTo + 1).getOrElse(topic.begin(partition))
    var going = true
    while (going) {
      typed.read(partition, from, 256) match {
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if (rs.isEmpty) going = false
          else
            for (d <- rs if going) d match {
              case Typed.Decoded.Ok(off, _, k, e) =>
                if (k.sameElements(key)) {
                  if (e.program != program) {
                    stopped = Some(Dialogue.Stopped.Mismatch(off, e.program, program))
                    going = false
                  } else if (e.expect != accepted) rejected = rejected :+ Dialogue.Lost(off, e.expect, accepted)
                  else {
                    e match {
                      case Dialogue.Entry.Answered(_, _, a) => out = out :+ a
                      // EVERYTHING BEFORE THIS IS SUPERSEDED: the journal
                      // becomes the seed alone; the record count carries on
                      case Dialogue.Entry.Continued(_, _, seed) => out = Vector(seed)
                    }
                    accepted += 1
                  }
                }
                if (going) from = off + 1
              case Typed.Decoded.Bad(off, err) =>
                stopped = Some(Dialogue.Stopped.Damage(off, err))
                going = false
            }
      }
    }
    seen = from
    Dialogue.Recovered(out.toList, accepted, rejected, stopped)
  }

  /** the answers accepted so far */
  def journal: Shift.Journal[A] = recovered.answers

  /** where the program stands — itself, folded over its journal; `Left`
   * when the log cannot be folded into a place at all */
  def at: Either[Dialogue.Stopped, Shift.Dialogue[Q, A, R, F]] ! F = {
    val r = recovered
    r.stopped match {
      case Some(s) => pure[F, Either[Dialogue.Stopped, Shift.Dialogue[Q, A, R, F]]](Left(s))
      case None => place(r.answers).map(p => Right(p))
    }
  }

  /**
   * WHY THE FOLD STOPPED, POINTING AT A LINE: replaying the accepted
   * prefix puts this program at the question the bad record was supposed
   * to answer, and `At` names that `pause`. It is the READER's line —
   * the code that cannot fold this journal. `None` when nothing stopped.
   */
  def diagnosis: Option[Dialogue.Diagnosis] ! F = {
    val r = recovered
    r.stopped match {
      case None => pure[F, Option[Dialogue.Diagnosis]](None)
      case Some(why) => place(r.answers).map(p => Some(Dialogue.Diagnosis(why, r.answers.size, p.asking.map(_.toString), p.where)))
    }
  }

  /** where it stands and how many records put it there, in ONE fold */
  def standing: Either[Dialogue.Stopped, (Shift.Dialogue[Q, A, R, F], Int)] ! F = {
    val r = recovered
    r.stopped match {
      case Some(s) => pure[F, Either[Dialogue.Stopped, (Shift.Dialogue[Q, A, R, F], Int)]](Left(s))
      case None => place(r.answers).map(p => Right((p, r.accepted)))
    }
  }

  /** THE COLD PATH: answer the question it is asking knowing only the
   * log — advance, and journal IN THE CONTINUATION of the advance */
  def answer(a: A): Dialogue.Answered[Q, A, R, F] ! F = {
    val r = recovered
    r.stopped match {
      case Some(s) => pure[F, Dialogue.Answered[Q, A, R, F]](Dialogue.Answered.Broken(s))
      case None => place(r.answers).flatMap(advance(_, a, r.accepted))
    }
  }

  /** THE WARM PATH: advance a dialogue you are already holding; `expect`
   * is the position this answer will occupy */
  def step(p: Shift.Dialogue[Q, A, R, F], a: A, expect: Int): Dialogue.Answered[Q, A, R, F] ! F =
    advance(p, a, expect)

  /** the one move both paths make: advance, then journal, then say
   * whether this writer's answer is the one the fold accepted */
  private def advance(p: Shift.Dialogue[Q, A, R, F], a: A, expect: Int): Dialogue.Answered[Q, A, R, F] ! F =
    p match {
      case Shift.Paused.Done(_) => pure[F, Dialogue.Answered[Q, A, R, F]](Dialogue.Answered.NotAsking(p))
      case Shift.Paused.Ask(_, _, _) =>
        // the durable journal is the journal: the in-memory one is empty
        Shift.answer[Q, A, R, F](p, List.empty[A])(a).flatMap { case (next, _) =>
          // ONLY HERE: the advance produced a value, so the program
          // accepts the answer. A throw above never reaches this
          val before = seen
          val off = journalled(a, expect)
          if (won(before, off)) {
            seen = off + 1
            pure[F, Dialogue.Answered[Q, A, R, F]](Dialogue.Answered.Advanced(next))
          } else at.map {
            case Right(actual) => Dialogue.Answered.Lost(actual)
            case Left(s) => Dialogue.Answered.Broken(s)
          }
        }
    }

  /** write the journal so far as a chapter, so a cold start reads one
   * record and a tail. Explicit: a snapshot is an optimisation */
  def snapshot(): Unit = snapshots.foreach { snaps =>
    val r = recovered
    if (r.intact) {
      val _ = snaps.putValue(key, Dialogue.Chapter(program, topic.end(partition) - 1, r.accepted, r.answers))
    }
  }

  /**
   * CLOSE THIS CHAPTER AND START THE NEXT: `seed` becomes the journal's
   * only answer. `true` means this writer's continuation is the one the
   * fold took; of two that continue from one position exactly one wins,
   * by the same `expect` the answers use.
   */
  def continueAs(seed: A, expect: Int): Boolean = {
    val before = seen
    val off = typed.append(partition, key, Dialogue.Entry.Continued(program, expect, seed): Dialogue.Entry[A], Ack.Durable)
    written += 1
    val ok = won(before, off)
    if (ok) seen = off + 1
    ok
  }

  private def journalled(a: A, expect: Int): Long = {
    val off = typed.append(partition, key, Dialogue.Entry.Answered(program, expect, a): Dialogue.Entry[A], Ack.Durable)
    written += 1
    if (snapshotEvery > 0 && written % snapshotEvery == 0) snapshot()
    off
  }

  /** did the fold take the record at this offset? An append that landed
   * exactly where this instance had read to cannot have been overtaken,
   * so no read happens at all; only a gap costs a fold */
  private def won(before: Long, off: Long): Boolean =
    if (before >= 0 && off == before) true
    else !recovered.rejected.exists(_.offset == off)

  /** HAS ANYBODY WRITTEN HERE SINCE THIS INSTANCE LAST READ? One offset
   * read. Conservative: it asks about the PARTITION, so a false "yes"
   * costs one replay and a false "no" cannot happen */
  def undisturbed: Boolean = seen >= 0 && topic.end(partition) == seen

  private var written = 0

  /** the offset this instance has read up to — what makes the
   * won-the-race check free on the warm path */
  private var seen: Long = -1L

  /** run to the end, journaling every answer; the oracle is called once
   * per question, never for one the journal already answered. The
   * `Attempt` is the idempotency key */
  def run(oracle: (Q, Dialogue.Attempt) => A ! F): R ! F =
    runUntil[Nothing]((q, at) => oracle(q, at).map(a => Right(a): Either[Nothing, A])).map {
      case Right(r) => r
      case Left(never) => never
    }

  /** THE SAME DRIVE, BUT IT MAY STOP: an oracle that answers `Left(s)`
   * says "nobody here can answer this", and the drive ENDS there and
   * hands `s` back, having journalled everything it did answer */
  def runUntil[S](oracle: (Q, Dialogue.Attempt) => Either[S, A] ! F): Either[S, R] ! F =
    runUntilIn[S, F](oracle)

  /**
   * THE DRIVE'S ROW IS NOT THE PROGRAM'S: a durable program's row must be
   * `Replayable`, but the ORACLE reaches outside. So the driver runs in
   * any `G` the program's `F` widens to — `G <: F`, rows being
   * contravariant — and the journal, the replay and the race check never
   * see the rest of `G`. (Scala 3 licenses this with `Row.Sub[F, G]` and
   * `up[G]`; here the bound is the licence and the widening is free.)
   */
  def runUntilIn[S, G <: F](oracle: (Q, Dialogue.Attempt) => Either[S, A] ! G): Either[S, R] ! G = {
    val r = recovered
    r.stopped match {
      case Some(s) => throw new Dialogue.Halted(s)
      case None => place(r.answers).flatMap[G, (Either[S, R], Shift.Dialogue[Q, A, R, F], Int)](runFromIn[S, G](_, r.accepted)(oracle)).map(_._1)
    }
  }

  /** THE WARM PATH, ACROSS CALLS: the walk alone, over a program somebody
   * already holds, answering where it ENDED as well as what it produced.
   * THE CALLER OWES THE VALIDITY — `Resume` checks `undisturbed` first */
  def runFromIn[S, G <: F](from: Shift.Dialogue[Q, A, R, F], at: Int)
                          (oracle: (Q, Dialogue.Attempt) => Either[S, A] ! G): (Either[S, R], Shift.Dialogue[Q, A, R, F], Int) ! G = {
    // trampolined: each next step is deferred into the program's flatMap
    def go(p: Shift.Dialogue[Q, A, R, F], index: Int): (Either[S, R], Shift.Dialogue[Q, A, R, F], Int) ! G = p match {
      case Shift.Paused.Done(r) => pure[G, (Either[S, R], Shift.Dialogue[Q, A, R, F], Int)]((Right(r), p, index))
      // the WARM path: the program is in hand, so no step replays
      case Shift.Paused.Ask(q, _, _) =>
        oracle(q, Dialogue.Attempt(id, index)).flatMap[G, (Either[S, R], Shift.Dialogue[Q, A, R, F], Int)] {
          case Left(s) => pure[G, (Either[S, R], Shift.Dialogue[Q, A, R, F], Int)]((Left(s), p, index))
          case Right(a) =>
            step(p, a, index).flatMap[G, (Either[S, R], Shift.Dialogue[Q, A, R, F], Int)] {
              case Dialogue.Answered.Advanced(next) => go(next, index + 1)
              case Dialogue.Answered.Lost(actual) => go(actual, index)
              case Dialogue.Answered.NotAsking(next) => go(next, index)
              case Dialogue.Answered.Broken(s) => throw new Dialogue.Halted(s)
            }
        }
    }
    go(from, at)
  }
}

object Dialogue {

  /** AN ORDINARY DURABLE DIALOGUE: the author's questions, answered by
   * the author's oracle, folded with `Shift.replay` */
  def apply[Q, A, R, F <: Row](topic: Topic, id: String, program: String,
                               snapshots: Option[Snapshots] = None,
                               snapshotEvery: Int = 0,
                               version: Int = 1,
                               upcasts: Map[Int, Typed.Upcast] = Map.empty)
                              (body: Shift.Asking.Aux[Q, A, R, F] => R ! (Shift[Any] + F))
                              (implicit sa: Schema[A], rp: Replayable[Shift[Any] + F], om: Shift.Machine[F], at: At): Dialogue[Q, A, R, F] =
    new Dialogue[Q, A, R, F](topic, id, program, snapshots, snapshotEvery, version, upcasts,
      j => Shift.replay[Q, A, R, F](body)(j))

  /** A DURABLE WORKFLOW: the same journal, also carrying the LIBRARY's
   * questions (`Wf.now`, `uuid`, `random`, `patch`, the timers, signals
   * and children); the fold is `Wf.replay` */
  def workflow[Q, A, R, F <: Row](topic: Topic, id: String, program: String,
                                  snapshots: Option[Snapshots] = None,
                                  snapshotEvery: Int = 0,
                                  version: Int = 1,
                                  upcasts: Map[Int, Typed.Upcast] = Map.empty)
                                 (body: Wf.Asks[Q, A, R, F] => R ! (Shift[Any] + F))
                                 (implicit sa: Schema[Wf.Ans[A]], rp: Replayable[Shift[Any] + F], om: Shift.Machine[F], at: At)
                                 : Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F] =
    new Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F](topic, id, program, snapshots, snapshotEvery, version, upcasts,
      j => Wf.replay[Q, A, R, F](body)(j))

  /**
   * A WORKFLOW'S JOURNAL ENTRY AS A SCHEMA: `Wf.Ans[A]` is an `Either`,
   * and okay2-codec derives no standard type. This is that `Either` as
   * the sum Scala 3's derivation makes of it — a `Left` and a `Right`
   * case, each carrying `value` — so a workflow's answers need one line:
   * `implicit val s: Schema[Wf.Ans[A]] = Dialogue.answerSchema[A]`.
   */
  def answerSchema[A](implicit sa: Schema[A]): Schema[Wf.Ans[A]] = {
    import AnswerCodec._
    Schema.wrap[Wf.Ans[A], Rep[A]](
      {
        case Left(v) => scala.util.Left(v)
        case Right(v) => scala.util.Right(v)
      },
      {
        case scala.util.Left(v) => Left[A](v)
        case scala.util.Right(v) => Right[A](v)
      })(rep[A])
  }

  private object AnswerCodec {
    sealed trait Rep[A]
    final case class Left[A](value: Wf.SysA) extends Rep[A]
    final case class Right[A](value: A) extends Rep[A]
    implicit def rep[A](implicit sa: Schema[A]): Schema[Rep[A]] = Schema.derived
  }

  /** the oracle a workflow's drive wants: the author answers their own
   * questions, the runtime its own — and when the runtime DECLINES (a
   * timer, a signal, a child) the drive stops and says what it waits on.
   * The `Attempt` reaches the author's oracle as its second argument */
  def asking[Q, A, G <: Row](oracle: (Q, Attempt) => A ! G)(implicit rt: Wf.Runtime)
      : (Wf.Ask[Q], Attempt) => Either[Wf.Wait, Wf.Ans[A]] ! G =
    (q, at) => q match {
      case Left(sys) => rt.answer(sys) match {
        case Left(w) => pure[G, Either[Wf.Wait, Wf.Ans[A]]](Left(w))
        case Right(sa) => pure[G, Either[Wf.Wait, Wf.Ans[A]]](Right(Left(sa)))
      }
      case Right(own) => oracle(own, at).map(v => Right(Right(v)))
    }

  /** DRIVE A DURABLE WORKFLOW as far as it goes: `Right` is its answer,
   * `Left` what has to happen before anybody can carry it further */
  implicit final class WorkflowOps[Q, A, R, F <: Row](private val d: Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F]) extends AnyVal {
    def runWorkflow(oracle: (Q, Attempt) => A ! F)(implicit rt: Wf.Runtime): Either[Wf.Wait, R] ! F =
      d.runUntil[Wf.Wait](asking[Q, A, F](oracle))

    /** the same, with the activities in their own row */
    def runWorkflowIn[G <: F](oracle: (Q, Attempt) => A ! G)(implicit rt: Wf.Runtime): Either[Wf.Wait, R] ! G =
      d.runUntilIn[Wf.Wait, G](asking[Q, A, G](oracle))

    /** the same drive, from a program in hand, saying where it ended */
    def runWorkflowFromIn[G <: F](from: Shift.Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], at: Int)
                                 (oracle: (Q, Attempt) => A ! G)(implicit rt: Wf.Runtime)
        : (Either[Wf.Wait, R], Shift.Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], Int) ! G =
      d.runFromIn[Wf.Wait, G](from, at)(asking[Q, A, G](oracle))
  }

  /** WHAT A JOURNAL RECORD IS: an answer, or the end of a chapter, each
   * with the program that wrote it and the position it expected */
  sealed trait Entry[A] {
    def program: String
    def expect: Int
  }

  object Entry {
    final case class Answered[A](program: String, expect: Int, a: A) extends Entry[A]
    /** THE END OF A CHAPTER: everything before it is superseded and the
     * journal restarts with `seed` as its only answer */
    final case class Continued[A](program: String, expect: Int, seed: A) extends Entry[A]
    implicit def schema[A](implicit sa: Schema[A]): Schema[Entry[A]] = Schema.derived
  }

  /** why the fold stopped: the log says something this reader must not
   * guess about */
  sealed trait Stopped
  object Stopped {
    /** a record that did not decode */
    final case class Damage(offset: Long, error: String) extends Stopped
    /** a record written by a DIFFERENT program */
    final case class Mismatch(offset: Long, found: String, expected: String) extends Stopped
  }

  /** A STOPPED FOLD, WITH THE READER'S OWN POSITION IN IT */
  final case class Diagnosis(why: Stopped, accepted: Int, asking: Option[String], where: Option[String]) {
    /** one line for a log or a page */
    override def toString: String = {
      val place = where.getOrElse("an unknown position")
      val q = asking.map(a => s" asking $a").getOrElse("")
      s"$why after $accepted answer(s); this program is at $place$q"
    }
  }

  /** a record whose writer expected to be at another position */
  final case class Lost(offset: Long, expect: Int, had: Int)

  /** what an attempt to answer did */
  sealed trait Answered[Q, A, R, F <: Row]
  object Answered {
    /** this writer's answer was accepted; here is where it stands */
    final case class Advanced[Q, A, R, F <: Row](to: Shift.Dialogue[Q, A, R, F]) extends Answered[Q, A, R, F]
    /** another writer got there first; `to` is where it ACTUALLY stands */
    final case class Lost[Q, A, R, F <: Row](to: Shift.Dialogue[Q, A, R, F]) extends Answered[Q, A, R, F]
    /** nobody was asking, so nothing was written */
    final case class NotAsking[Q, A, R, F <: Row](to: Shift.Dialogue[Q, A, R, F]) extends Answered[Q, A, R, F]
    /** the log could not be folded into a place at all */
    final case class Broken[Q, A, R, F <: Row](why: Stopped) extends Answered[Q, A, R, F]
  }

  /** the oracle's idempotency key: the journal's own position */
  final case class Attempt(id: String, index: Int)

  /** `run` met a log it cannot fold */
  final class Halted(val why: Stopped) extends RuntimeException(s"dialogue halted: $why")

  /** A JOURNAL PREFIX, WRITTEN DOWN: the answers up to `upTo`, with the
   * program that wrote it */
  final case class Chapter[A](program: String, upTo: Long, accepted: Int, answers: List[A])

  object Chapter {
    implicit def schema[A](implicit sa: Schema[A]): Schema[Chapter[A]] = Schema.derived
  }

  /** the journal, the races that lost, and what stopped the fold */
  final case class Recovered[A](answers: List[A],
                                /** records the fold accepted — what `expect` counts */
                                accepted: Int,
                                rejected: List[Lost],
                                stopped: Option[Stopped]) {
    def intact: Boolean = stopped.isEmpty
  }
}

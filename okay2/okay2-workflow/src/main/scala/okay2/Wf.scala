package okay2

import okay2.Proc.{Path, Ran, Stopped, Walked}

/**
 * A DURABLE PROGRAM'S OWN NON-DETERMINISM — the Scala 3 core's
 * okay-workflow `Wf`. A clock, an id and a die are QUESTIONS TOO, asked
 * of the runtime instead of the author's oracle and remembered in the
 * same journal, so the program stays a pure function of its journal.
 * The channel is a sum the LIBRARY owns — `Either[Sys, Q]` for
 * questions, `Either[SysA, A]` for answers — and its tag is the whole of
 * `patch`: a `Patch` whose decision is not in an old journal answers
 * `false` WITHOUT eating the entry that follows (Temporal's
 * `getVersion`, falling out of the tagging).
 */
object Wf {

  /** the questions the RUNTIME answers, not the author's oracle */
  sealed trait Sys
  object Sys {
    case object Now extends Sys
    case object Uuid extends Sys
    case object Random extends Sys
    /** is this branch on, for THIS run? */
    final case class Patch(id: String) extends Sys
    /** wake me at this wall-clock instant */
    final case class Timer(untilMillis: Long) extends Sys
    /** wake me when this named signal arrives */
    final case class Signal(name: String) extends Sys
    /** wake me when this child dialogue finishes */
    final case class Child(id: String) extends Sys
    /** has somebody asked this run to stop? cooperative on purpose */
    case object Cancelled extends Sys
  }

  /** their answers, tagged so a journal entry says what it answers */
  sealed trait SysA
  object SysA {
    final case class Millis(v: Long) extends SysA
    final case class Text(v: String) extends SysA
    final case class Dice(v: Double) extends SysA
    final case class Flag(v: Boolean) extends SysA
    /** the deadline passed */
    case object Elapsed extends SysA
    /** the signal arrived, or the child finished, with this payload */
    final case class Got(v: String) extends SysA
  }

  /** what a run is waiting for when nobody present can answer it */
  sealed trait Wait
  object Wait {
    final case class Until(millis: Long) extends Wait
    final case class Signal(name: String) extends Wait
    final case class Child(id: String) extends Wait
  }

  /** where a drive stopped */
  sealed trait Step[Q, R]
  object Step {
    final case class Done[Q, R](value: R) extends Step[Q, R]
    /** the AUTHOR's question — an oracle, a person or an API answers */
    final case class Asking[Q, R](q: Q) extends Step[Q, R]
    /** nobody here can answer it yet; this says who can */
    final case class Waiting[Q, R](on: Wait) extends Step[Q, R]
  }

  type Ask[Q] = Either[Sys, Q]
  type Ans[A] = Either[SysA, A]

  /** the dialogue evidence such a program runs under */
  type Asking[Q, A, R, F <: Row] = Delim.Asking.Aux[Ask[Q], Ans[A], R, F]

  /** where such a program stands */
  type Paused[Q, A, R, F <: Row] = Delim.Dialogue[Ask[Q], Ans[A], R, F]

  /** the journal of such a program */
  type Journal[A] = List[Ans[A]]

  /** a driver answered a question with the wrong KIND of answer */
  final class Mismatched(q: Any, a: Any) extends RuntimeException(s"a $q was answered with $a")

  /**
   * THE EVIDENCE CARRIES THE DOORS: a class that knows its four types
   * and offers every door as a method, so a body names the types once,
   * in its own signature, and no call site repeats them.
   */
  final class Asks[Q, A, R, F <: Row] private[okay2] (private[okay2] val in: Asking[Q, A, R, F]) {
    /** ask the outside world, through the author's own question type */
    def pause(q: Q)(implicit at: At): A ! (Delim + F) = Wf.pause[Q, A, R, F](q)(in, at)
    /** the same under the name the literature uses: an activity */
    def perform(cmd: Q)(implicit at: At): A ! (Delim + F) = pause(cmd)
    /** the wall clock, once, remembered */
    def now(implicit at: At): Long ! (Delim + F) = Wf.now[Q, A, R, F](in, at)
    /** a fresh id, once, remembered */
    def uuid(implicit at: At): String ! (Delim + F) = Wf.uuid[Q, A, R, F](in, at)
    /** a die, once, remembered */
    def random(implicit at: At): Double ! (Delim + F) = Wf.random[Q, A, R, F](in, at)
    /** is this branch on for THIS run? */
    def patch(id: String)(implicit at: At): Boolean ! (Delim + F) = Wf.patch[Q, A, R, F](id)(in, at)
    /** has somebody asked this run to stop, and why? */
    def cancelled(implicit at: At): Option[String] ! (Delim + F) = Wf.cancelled[Q, A, R, F](in, at)
    /** SLEEP, DURABLY: the deadline is computed from `now`, so it is
     * JOURNALLED and a replay wakes at the instant the first run chose */
    def sleep(millis: Long)(implicit at: At): Unit ! (Delim + F) = now.flatMap(t => Wf.timer[Q, A, R, F](t + millis)(in, at))
    /** wait for a named signal from outside; the payload is its value */
    def awaitSignal(name: String)(implicit at: At): String ! (Delim + F) = Wf.signal[Q, A, R, F](name)(in, at)
    /** wait for a child dialogue to finish, and take its answer */
    def awaitChild(id: String)(implicit at: At): String ! (Delim + F) = Wf.child[Q, A, R, F](id)(in, at)
  }

  // ── the author's doors ───────────────────────────────────────────

  def pause[Q, A, R, F <: Row](q: Q)(implicit s: Asking[Q, A, R, F], at: At): A ! (Delim + F) =
    Delim.pause(s)(Right(q): Ask[Q]).map {
      case Right(a) => a
      case other => throw new Mismatched(q, other)
    }

  def perform[Q, A, R, F <: Row](cmd: Q)(implicit s: Asking[Q, A, R, F], at: At): A ! (Delim + F) = pause(cmd)

  def now[Q, A, R, F <: Row](implicit s: Asking[Q, A, R, F], at: At): Long ! (Delim + F) =
    sys[Q, A, R, F, Long](Sys.Now) { case SysA.Millis(v) => v }

  def uuid[Q, A, R, F <: Row](implicit s: Asking[Q, A, R, F], at: At): String ! (Delim + F) =
    sys[Q, A, R, F, String](Sys.Uuid) { case SysA.Text(v) => v }

  def random[Q, A, R, F <: Row](implicit s: Asking[Q, A, R, F], at: At): Double ! (Delim + F) =
    sys[Q, A, R, F, Double](Sys.Random) { case SysA.Dice(v) => v }

  /** is this branch on for THIS run? both decisions live in the journal */
  def patch[Q, A, R, F <: Row](id: String)(implicit s: Asking[Q, A, R, F], at: At): Boolean ! (Delim + F) =
    sys[Q, A, R, F, Boolean](Sys.Patch(id)) { case SysA.Flag(v) => v }

  /** the three that suspend: ordinary `Sys` questions the runtime
   * declines to answer in place */
  def timer[Q, A, R, F <: Row](untilMillis: Long)(implicit s: Asking[Q, A, R, F], at: At): Unit ! (Delim + F) =
    sys[Q, A, R, F, Unit](Sys.Timer(untilMillis)) { case SysA.Elapsed => () }

  def signal[Q, A, R, F <: Row](name: String)(implicit s: Asking[Q, A, R, F], at: At): String ! (Delim + F) =
    sys[Q, A, R, F, String](Sys.Signal(name)) { case SysA.Got(v) => v }

  def child[Q, A, R, F <: Row](id: String)(implicit s: Asking[Q, A, R, F], at: At): String ! (Delim + F) =
    sys[Q, A, R, F, String](Sys.Child(id)) { case SysA.Got(v) => v }

  def cancelled[Q, A, R, F <: Row](implicit s: Asking[Q, A, R, F], at: At): Option[String] ! (Delim + F) =
    sys[Q, A, R, F, Option[String]](Sys.Cancelled) {
      case SysA.Text(why) => Some(why)
      case SysA.Flag(false) => None
    }

  /** one library question, and the shape of answer it accepts — the
   * partial function IS the expected shape, so a wrong answer is a loud
   * `Mismatched` rather than a cast */
  private def sys[Q, A, R, F <: Row, X](q: Sys)(f: PartialFunction[SysA, X])(implicit s: Asking[Q, A, R, F], at: At): X ! (Delim + F) =
    Delim.pause(s)(Left(q): Ask[Q]).map {
      case Left(a) if f.isDefinedAt(a) => f(a)
      case other => throw new Mismatched(q, other)
    }

  // ── the runtime's side ───────────────────────────────────────────

  /** what a workflow that bounds its own history returns: Temporal's
   * `continueAsNew` as a RESULT — the seed is the next journal's first
   * answer */
  sealed trait Next[+S, +R]
  object Next {
    final case class Continue[S](seed: S) extends Next[S, Nothing]
    final case class Done[R](value: R) extends Next[Nothing, R]
    /** the seed, if this result is a continuation */
    def seed[S, R](n: Next[S, R]): Option[S] = n match {
      case Continue(s) => Some(s)
      case Done(_) => None
    }
  }

  /** what answers the LIBRARY's questions; `Left` is "I cannot answer
   * this now, and here is who can" */
  trait Runtime {
    def answer(q: Sys): Either[Wait, SysA]
  }

  object Runtime {
    /** the real world */
    implicit val live: Runtime = new Runtime {
      def answer(q: Sys): Either[Wait, SysA] = q match {
        case Sys.Now => Right(SysA.Millis(System.currentTimeMillis()))
        case Sys.Uuid => Right(SysA.Text(freshUuid()))
        case Sys.Random => Right(SysA.Dice(scala.util.Random.nextDouble()))
        case Sys.Patch(_) => Right(SysA.Flag(true))
        case Sys.Timer(t) => Left(Wait.Until(t))
        case Sys.Signal(n) => Left(Wait.Signal(n))
        case Sys.Child(id) => Left(Wait.Child(id))
        case Sys.Cancelled => Right(SysA.Flag(false))
      }
    }

    /** a version-4 UUID from `scala.util.Random`, with the version and
     * variant bits set as RFC 9562 has them: `UUID.randomUUID` needs
     * `java.security.SecureRandom`, which neither Scala.js nor Scala
     * Native provides (okay2-cross). A run id has to be FRESH, not
     * unguessable — `Uid.system` draws the same way */
    def freshUuid(): String = {
      val hi = (scala.util.Random.nextLong() & ~0xF000L) | 0x4000L
      val lo = (scala.util.Random.nextLong() & 0x3FFFFFFFFFFFFFFFL) | Long.MinValue
      new java.util.UUID(hi, lo).toString
    }

    /** a fixed one, for a test that wants to read its own output */
    def scripted(millis: Long, id: String, dice: Double): Runtime = new Runtime {
      def answer(q: Sys): Either[Wait, SysA] = q match {
        case Sys.Now => Right(SysA.Millis(millis))
        case Sys.Uuid => Right(SysA.Text(id))
        case Sys.Random => Right(SysA.Dice(dice))
        case Sys.Patch(_) => Right(SysA.Flag(true))
        case Sys.Timer(t) => Left(Wait.Until(t))
        case Sys.Signal(n) => Left(Wait.Signal(n))
        case Sys.Child(i) => Left(Wait.Child(i))
        case Sys.Cancelled => Right(SysA.Flag(false))
      }
    }

    /** the same runtime, but this run can be told to stop; `why` is read
     * when the program ASKS */
    def cancellable(rt: Runtime)(why: => Option[String]): Runtime = new Runtime {
      def answer(q: Sys): Either[Wait, SysA] = q match {
        case Sys.Cancelled => Right(why.fold[SysA](SysA.Flag(false))(SysA.Text(_)))
        case other => rt.answer(other)
      }
    }
  }

  /** start a program that may ask the runtime as well as the world */
  def resumable[Q, A, R, F <: Row](body: Asks[Q, A, R, F] => R ! (Delim + F))(implicit om: Delim.OneMachine[F], at: At): Paused[Q, A, R, F] ! F =
    Delim.resumable[Ask[Q], Ans[A], R, F](in => body(new Asks[Q, A, R, F](in)))(om, at)

  /** run to the end: the library's questions by the runtime, the
   * author's by the oracle, every answer handed back for the journal */
  def drive[Q, A, R, F <: Row](p: Paused[Q, A, R, F])(oracle: Q => A ! F)(implicit rt: Runtime, om: Delim.OneMachine[F]): (Step[Q, R], Journal[A]) ! F =
    loop[Q, A, R, F](p, Nil)(q => Some(oracle(q)))

  /** THE WORKER'S PRIMITIVE: advance as far as the runtime alone can
   * take it; the author's questions come back as `Asking` */
  def advance[Q, A, R, F <: Row](p: Paused[Q, A, R, F])(implicit rt: Runtime, om: Delim.OneMachine[F]): (Step[Q, R], Journal[A]) ! F =
    loop[Q, A, R, F](p, Nil)(_ => None)

  private def loop[Q, A, R, F <: Row](p: Paused[Q, A, R, F], acc: Journal[A])(own: Q => Option[A ! F])
                                     (implicit rt: Runtime, om: Delim.OneMachine[F]): (Step[Q, R], Journal[A]) ! F =
    p match {
      case Delim.Paused.Done(r) => pure[F, (Step[Q, R], Journal[A])]((Step.Done(r), acc))
      case Delim.Paused.Ask(Left(q), _, _) =>
        rt.answer(q) match {
          case Left(w) => pure[F, (Step[Q, R], Journal[A])]((Step.Waiting(w), acc))
          case Right(sa) =>
            val a: Ans[A] = Left(sa)
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a)(om).flatMap { case (next, _) => loop[Q, A, R, F](next, acc :+ a)(own) }
        }
      case Delim.Paused.Ask(Right(q), _, _) =>
        own(q) match {
          case None => pure[F, (Step[Q, R], Journal[A])]((Step.Asking(q), acc))
          case Some(prog) => prog.flatMap { v =>
            val a: Ans[A] = Right(v)
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a)(om).flatMap { case (next, _) => loop[Q, A, R, F](next, acc :+ a)(own) }
          }
        }
    }

  /** where a program stands, from its journal — takes NO runtime: the
   * journal is the only source it has */
  def replay[Q, A, R, F <: Row](body: Asks[Q, A, R, F] => R ! (Delim + F))(j: Journal[A])
                               (implicit om: Delim.OneMachine[F], rp: Replayable[Delim + F], at: At): Paused[Q, A, R, F] ! F =
    replaying[Q, A, R, F](body)(j).map(_._1)

  /** the same walk, saying what it answered on the way: a patch's id
   * lives in the QUESTION, so only running the program pairs answers
   * with the branches they decided */
  def replaying[Q, A, R, F <: Row](body: Asks[Q, A, R, F] => R ! (Delim + F))(j: Journal[A])
                                  (implicit om: Delim.OneMachine[F], rp: Replayable[Delim + F], at: At): (Paused[Q, A, R, F], List[(Ask[Q], Ans[A])]) ! F = {
    val _ = rp
    def go(p: Paused[Q, A, R, F], left: Journal[A], seen: List[(Ask[Q], Ans[A])]): (Paused[Q, A, R, F], List[(Ask[Q], Ans[A])]) ! F = p match {
      case Delim.Paused.Done(_) => pure[F, (Paused[Q, A, R, F], List[(Ask[Q], Ans[A])])]((p, seen.reverse))
      case Delim.Paused.Ask(q, _, _) =>
        (q, left) match {
          case (_, Nil) => pure[F, (Paused[Q, A, R, F], List[(Ask[Q], Ans[A])])]((p, seen.reverse))
          case (Left(Sys.Patch(_)), Right(_) :: _) =>
            // this run predates the branch: false, and the entry is kept
            val no: Ans[A] = Left(SysA.Flag(false))
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(no)(om).flatMap { case (next, _) => go(next, left, (q, no) :: seen) }
          case (_, a :: rest) =>
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a)(om).flatMap { case (next, _) => go(next, rest, (q, a) :: seen) }
        }
    }
    resumable[Q, A, R, F](body).flatMap(go(_, j, Nil))
  }

  // ── the STATIC half: the same workflow as a term ──────────────────

  /** the signature a durable procedure's term is built over */
  sealed trait Asked[Q, A] extends Row { type Op[+R] = Question[Q, A, R] }

  /**
   * THE QUESTIONS A DURABLE PROCEDURE ASKS, indexed by the ANSWER each
   * expects — the core's GADT. Its three readings are METHODS here
   * (`tag`, `read`, `door`): scalac 2 would not refine the answer type
   * from a match on the cases, so each case says its own.
   */
  sealed abstract class Question[Q, A, +R] {
    /** the question in the journal's own spelling */
    def tag: Ask[Q]
    /** one answer, read back at the question's own type — a pair that
     * does not fit is DATA, because a fold that throws cannot be a
     * deploy check */
    private[okay2] def read(a: Ans[A]): Either[String, R]
    /** the question as the monadic door that already exists — spelled
     * `Free[...]`: the `!` alias is invariant to the variance check,
     * where Free itself is covariant in its answer */
    private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): Free[Delim + Pure, R]
    private[okay2] def isPatch: Boolean = false
    protected def no(a: Ans[A]): Either[String, Nothing] = Left(s"$this cannot take $a")
  }

  object Question {
    final case class Ask[Q, A](q: Q) extends Question[Q, A, A] {
      def tag: Wf.Ask[Q] = Right(q)
      private[okay2] def read(a: Ans[A]): Either[String, A] = a match { case Right(v) => Right(v); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): A ! (Delim + Pure) = w.pause(q)
    }
    final case class Now[Q, A]() extends Question[Q, A, Long] {
      def tag: Wf.Ask[Q] = Left(Sys.Now)
      private[okay2] def read(a: Ans[A]): Either[String, Long] = a match { case Left(SysA.Millis(v)) => Right(v); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): Long ! (Delim + Pure) = w.now
    }
    final case class Uuid[Q, A]() extends Question[Q, A, String] {
      def tag: Wf.Ask[Q] = Left(Sys.Uuid)
      private[okay2] def read(a: Ans[A]): Either[String, String] = a match { case Left(SysA.Text(v)) => Right(v); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): String ! (Delim + Pure) = w.uuid
    }
    final case class Random[Q, A]() extends Question[Q, A, Double] {
      def tag: Wf.Ask[Q] = Left(Sys.Random)
      private[okay2] def read(a: Ans[A]): Either[String, Double] = a match { case Left(SysA.Dice(v)) => Right(v); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): Double ! (Delim + Pure) = w.random
    }
    final case class Patched[Q, A](id: String) extends Question[Q, A, Boolean] {
      def tag: Wf.Ask[Q] = Left(Sys.Patch(id))
      private[okay2] def read(a: Ans[A]): Either[String, Boolean] = a match { case Left(SysA.Flag(v)) => Right(v); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): Boolean ! (Delim + Pure) = w.patch(id)
      private[okay2] override def isPatch: Boolean = true
    }
    final case class Timer[Q, A](untilMillis: Long) extends Question[Q, A, Unit] {
      def tag: Wf.Ask[Q] = Left(Sys.Timer(untilMillis))
      private[okay2] def read(a: Ans[A]): Either[String, Unit] = a match { case Left(SysA.Elapsed) => Right(()); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): Unit ! (Delim + Pure) = Wf.timer[Q, A, Rr, Pure](untilMillis)(w.in, at)
    }
    final case class Signalled[Q, A](name: String) extends Question[Q, A, String] {
      def tag: Wf.Ask[Q] = Left(Sys.Signal(name))
      private[okay2] def read(a: Ans[A]): Either[String, String] = a match { case Left(SysA.Got(v)) => Right(v); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): String ! (Delim + Pure) = w.awaitSignal(name)
    }
    final case class Childed[Q, A](id: String) extends Question[Q, A, String] {
      def tag: Wf.Ask[Q] = Left(Sys.Child(id))
      private[okay2] def read(a: Ans[A]): Either[String, String] = a match { case Left(SysA.Got(v)) => Right(v); case _ => no(a) }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): String ! (Delim + Pure) = w.awaitChild(id)
    }
    final case class Cancelled[Q, A]() extends Question[Q, A, Option[String]] {
      def tag: Wf.Ask[Q] = Left(Sys.Cancelled)
      private[okay2] def read(a: Ans[A]): Either[String, Option[String]] = a match {
        case Left(SysA.Text(why)) => Right(Some(why))
        case Left(SysA.Flag(false)) => Right(None)
        case _ => no(a)
      }
      private[okay2] def door[Rr](w: Asks[Q, A, Rr, Pure])(implicit at: At): Option[String] ! (Delim + Pure) = w.cancelled
    }
  }

  /** a durable procedure: a `Proc` over the questions above */
  type Proc[Q, A, X, Y] = okay2.Proc[Asked[Q, A], X, Y]

  object Proc {
    import okay2.Proc.op

    /** ask the outside world, through the author's own question type */
    def ask[Q, A, X](q: X => Q): Wf.Proc[Q, A, X, A] = asking[Q, A, X]("ask")(q)

    /** the same, under a name the picture can show */
    def asking[Q, A, X](name: String)(q: X => Q): Wf.Proc[Q, A, X, A] = op[Asked[Q, A], X, A](name)(x => Question.Ask[Q, A](q(x)))

    /** the same under the name the literature uses */
    def perform[Q, A, X](cmd: X => Q): Wf.Proc[Q, A, X, A] = ask[Q, A, X](cmd)

    def now[Q, A, X]: Wf.Proc[Q, A, X, Long] = op[Asked[Q, A], X, Long]("now")(_ => Question.Now[Q, A]())
    def uuid[Q, A, X]: Wf.Proc[Q, A, X, String] = op[Asked[Q, A], X, String]("uuid")(_ => Question.Uuid[Q, A]())
    def random[Q, A, X]: Wf.Proc[Q, A, X, Double] = op[Asked[Q, A], X, Double]("random")(_ => Question.Random[Q, A]())
    def patch[Q, A, X](id: String): Wf.Proc[Q, A, X, Boolean] = op[Asked[Q, A], X, Boolean](s"patch:$id")(_ => Question.Patched[Q, A](id))
    def timer[Q, A]: Wf.Proc[Q, A, Long, Unit] = op[Asked[Q, A], Long, Unit]("timer")(t => Question.Timer[Q, A](t))

    /** SLEEP, DURABLY — `now >>> arr(_ + millis) >>> timer`, so the
     * deadline is journalled, not computed on the fly */
    def sleep[Q, A, X](millis: Long): Wf.Proc[Q, A, X, Unit] = {
      val Ar = okay2.Proc.procArrow[Asked[Q, A]]
      Ar.compose(timer[Q, A], Ar.compose(Ar.arr((t: Long) => t + millis), now[Q, A, X]))
    }

    def awaitSignal[Q, A, X](name: String): Wf.Proc[Q, A, X, String] = op[Asked[Q, A], X, String](s"signal:$name")(_ => Question.Signalled[Q, A](name))
    def awaitChild[Q, A, X](id: String): Wf.Proc[Q, A, X, String] = op[Asked[Q, A], X, String](s"child:$id")(_ => Question.Childed[Q, A](id))
    def cancelled[Q, A, X]: Wf.Proc[Q, A, X, Option[String]] = op[Asked[Q, A], X, Option[String]]("cancelled")(_ => Question.Cancelled[Q, A]())

    /** THE BRIDGE: a term becomes an ordinary durable program, so every
     * driver, timer, signal and replay works on it unchanged */
    def program[Q, A, R, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X)(implicit w: Asks[Q, A, R, Pure], at: At): Y ! (Delim + Pure) = {
      type L[Z] = Free[Delim + Pure, Z]
      p.foldMap[L](new Static.To[Asked[Q, A], L] {
        def apply[Z](q: Question[Q, A, Z]): Free[Delim + Pure, Z] = q.door(w)
      })(Free.monad[Delim + Pure]).apply(x)
    }

    /** a question in the journal's own spelling */
    def tag[Q, A](q: Question[Q, A, _]): Ask[Q] = q.tag

    /** where a fold of the term over a journal ended */
    sealed trait Standing[Q, A, +Y] {
      /** every question this run is waiting on */
      def pending: Vector[(Path, Question[Q, A, _])] = this match {
        case Standing.Done(_) => Vector.empty
        case Standing.Asking(at, q, _) => Vector((at, q))
        case Standing.Waiting(on, _) => on
      }
    }
    object Standing {
      final case class Done[Q, A, Y](value: Y) extends Standing[Q, A, Y]
      /** waiting, at this path, on this question, with this many
       * records of the journal accepted */
      final case class Asking[Q, A](at: Path, q: Question[Q, A, _], accepted: Int) extends Standing[Q, A, Nothing]
      /** waiting on MORE THAN ONE question — a `Par` with both branches
       * outstanding, in the order the journal will record the answers */
      final case class Waiting[Q, A](on: Vector[(Path, Question[Q, A, _])], accepted: Int) extends Standing[Q, A, Nothing]
    }

    /** the journal does not fit the term, and where */
    final case class Stranded(at: Path, record: Int, why: String) {
      override def toString: String = s"stranded at ${at.show} on record $record: $why"
    }

    /** where a walk stopped: waiting, or stranded */
    private sealed trait Halt[Q, A]
    private final case class Waits[Q, A](on: Vector[(Path, Question[Q, A, _])], used: Int) extends Halt[Q, A]
    private final case class Bad[Q, A](at: Path, record: Int, why: String) extends Halt[Q, A]

    private type Undos[Q, A] = scala.collection.mutable.ArrayBuffer[Wf.Proc[Q, A, Unit, Unit]]
    private def fresh[Q, A]: Undos[Q, A] = scala.collection.mutable.ArrayBuffer.empty

    /** THE FOLD OVER A JOURNAL, and its one subtle rule: a `Patch` whose
     * decision is not in the journal, facing a record that answers
     * something else, answers `false` and does NOT eat the record —
     * `Wf.replaying`'s rule, asserted against it by the tests */
    private final class Walk[Q, A](undos: Undos[Q, A]) extends okay2.Proc.Walker[Asked[Q, A], (Journal[A], Int), Halt[Q, A]] {
      def op[Y](q: Question[Q, A, Y], s: (Journal[A], Int), at: Path): Walked[(Journal[A], Int), Halt[Q, A], Y] = s match {
        case (Nil, used) => Stopped(Waits(Vector((at, q)), used))
        case (j @ (Right(_) :: _), used) if q.isPatch => q.read(Left(SysA.Flag(false))) match {
          case Left(why) => Stopped(Bad(at, used, why))
          case Right(v) => Ran(v, (j, used))
        }
        case (a :: rest, used) => q.read(a) match {
          case Left(why) => Stopped(Bad(at, used, why))
          case Right(v) => Ran(v, (rest, used + 1))
        }
      }
      def completed(undo: okay2.Proc[Asked[Q, A], Unit, Unit]): Unit = { undos += undo; () }
      /** the left branch waits, so the journal is empty: the right
       * branch's first question is knowable now — walked with a
       * THROWAWAY buffer, since nothing it "completes" has happened */
      def parStopped[X, Z](left: Halt[Q, A], right: okay2.Proc[Asked[Q, A], X, Z], x: X, at: Path): Halt[Q, A] = left match {
        case Waits(on, u) =>
          right.walk(x, (Nil: Journal[A], u), at, new Walk[Q, A](fresh[Q, A])) match {
            case Stopped(Waits(more, _)) => Waits(on ++ more, u)
            case Stopped(bad) => bad
            case Ran(_, _) => left
          }
        case bad => bad
      }
    }

    /** WHERE THE PROCEDURE STANDS, DERIVED FROM THE TERM: folds the term
     * over the answers and performs nothing — no row, no monad, no
     * runtime in its signature */
    def walk[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X, journal: Journal[A]): Either[Stranded, Standing[Q, A, Y]] =
      p.walk(x, (journal, 0), Path.root, new Walk[Q, A](fresh[Q, A])) match {
        case Ran(y, _) => Right(Standing.Done[Q, A, Y](y))
        case Stopped(Waits(on, used)) =>
          if (on.sizeIs == 1) Right(Standing.Asking[Q, A](on.head._1, on.head._2, used))
          else Right(Standing.Waiting[Q, A](on, used))
        case Stopped(Bad(at, rec, why)) => Left(Stranded(at, rec, why))
      }

    /** WHAT THIS RUN WOULD HAVE TO UNDO, AS A TERM: every `Undo` whose
     * step completed contributes its compensation, fed, in REVERSE */
    def compensating[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X, journal: Journal[A]): Wf.Proc[Q, A, Unit, Unit] = {
      val buf = fresh[Q, A]
      val _ = p.walk(x, (journal, 0), Path.root, new Walk[Q, A](buf))
      buf.reverseIterator.reduceOption((a, b) => okay2.Proc.andThen(a, b))
        .getOrElse(okay2.Proc.arr[Asked[Q, A], Unit, Unit](identity))
    }

    /** the deploy check: this journal still fits this program */
    def accepts[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X, journal: Journal[A]): Boolean = walk(p)(x, journal).isRight

    /** THE DEPLOY CHECK OVER A WHOLE TOPIC: which runs this term would
     * STRAND, and where — a pure function of the journals */
    def strands[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X)(journals: List[(String, Journal[A])]): Map[String, Stranded] =
      journals.foldLeft(Map.empty[String, Stranded]) { case (acc, (id, j)) =>
        walk(p)(x, j) match {
          case Left(bad) => acc + (id -> bad)
          case Right(_) => acc
        }
      }
  }
}

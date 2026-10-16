package okay.scala2

import okay.{!, +, %, Answers, Erased, Foreign, Run, effect}
import okay.core.{Paths, Req}
import okay.given
import scala.util.control.NonFatal

/**
 * A program over an OPEN row of effects, for Scala 2.13
 * (specs/scala2-facade.md, stage 2).
 *
 * okay's row is a union, `State % Int + Writer % String`, which Scala 2
 * cannot spell. Here the row is an INTERSECTION of phantom capability
 * types, `State[Int] with Writer[String]`, which it can — the shape of
 * ZIO 1's environment `R`. `Eff` is contravariant in it, so a program
 * needing only `State[Int]` is already a program in any wider row, and
 * `flatMap[R1 <: R, B]` lets the compiler find the row two programs
 * share. A handler takes one capability off: `State.run(1)(prog)` turns
 * `Eff[State[Int] with R, A]` into `Eff[R, (Int, A)]`, and scalac 2.13
 * infers `R` from the intersection with no annotation (measured by hand
 * 2026-09-22). `Eff.run` accepts only `Eff[Any, A]`, so a program with
 * an effect left unhandled does not compile.
 *
 * Underneath is okay's program and okay's own handlers; each operation
 * and each handler below is a one-line call into the Scala 3 library.
 */
final class Eff[-R, +A] private (private val body: EffBody[A]) {

  private[scala2] def program: A ! Rows.Top = body.p

  def map[B](f: A => B): Eff[R, B] = Eff.of(body.p.map(f))

  def flatMap[R1 <: R, B](f: A => Eff[R1, B]): Eff[R1, B] =
    Eff.of(body.p.flatMap(a => f(a).program))
}

/** held out of `Eff`'s constructor, as `ProgBody` is out of `Prog`'s */
private[scala2] final class EffBody[+A](val p: A ! Rows.Top) extends AnyVal

/**
 * THE ROW OF THE SCALA 3 SIDE. On the Scala 2 side the row is a phantom
 * intersection that no Scala 3 type follows, so a program needs ONE
 * effect, `Top`, whose operations are any values; the phantom `R`
 * guarantees statically that every operation has a handler before
 * `Eff.run` accepts it. A core program crosses in by `erase` (each of its
 * effects performed at `Top`'s place) and a handler takes its effect off
 * by `moved` (its operations moved, by class, to it beside `Top`) and then
 * the core's own runner; a core program is read back at its row by
 * `typed` (each operation checked to be one of the row's).
 */
private[scala2] object Rows {

  /** the one effect every `Eff` needs: its operations are any values — opaque, so it is a name of its own (its key,
   * `KeyOf`'s by name) and not the `Any` an alias would be read as */
  opaque type Top[X] = Any
  /** a value performed as an operation of `Top`, answering `X` */
  def top[X](op: Any): Top[X] = op
  /** `Top`'s key, one: inside this object `Top` is `Any`, and has no name a derived key could read */
  given topKey: okay.KeyOf[Top] = okay.KeyOf.named[Top]("okay.scala2.Rows.Top")


  /** a core program of the row `F`, its effects at `Top`'s place */
  def erase[A, F[_]](p: A ! F)(using e: Erased[F]): A ! Top = Erased.erase[A, F, Top](p)

  /** `E`'s operations moved to it beside `Top` (its `Member` says which); the others stay at `Top` */
  def moved[E[_], A](p: A ! Top)(using m: Foreign.Member[E]): A ! E + Top =
    p.translate[Top, E + Top]([X] => (op: Top[X]) => m.operation(Foreign.obj(op)) match
      case Some(q) => q.map(answered[X]).at[E + Top]
      case None => effect[Top, X](op).at[E + Top]
    ).at[E + Top]

  /** the program at the core row `F`: each operation checked to be one of F's as it is performed */
  def typed[F[_], A](p: A ! Top)(using m: Foreign.Member[F]): A ! F =
    p.translate[Top, F]([X] => (op: Top[X]) => m.operation(Foreign.obj(op)) match
      case Some(q) => q.map(answered[X])
      case None => throw IllegalStateException(s"okay.scala2: ${named(op)} is not an operation of the row")
    ).at[F]

  /** an operation's answer at its answer type: `X` is erased, and `Member` performed the operation that has it */
  private def answered[X](a: Any): X = a.asInstanceOf[X]

  private[scala2] def named(x: Any): String = if (x == null) "null" else s"${x.getClass.getName} ($x)"

  /** what `run` answers an operation that reached it with: a refusal by name */
  private[scala2] val unhandled: Answers[Top] = new Answers[Top]:
    def handle[X](op: Top[X]): X = throw IllegalStateException(s"okay.scala2: no handler took ${named(op)}")

  /** `Async`'s operations answered on this thread, every other refused */
  private[scala2] val asyncOnly: Answers[Top] = new Answers[Top]:
    private val async = Foreign.Member.of[okay.Async]
    private val answers = summon[Answers[okay.Async]]
    def handle[X](op: Top[X]): X = async.operation(Foreign.obj(op)) match
      case Some(q) => answered[X](q.runWith(using answers))
      case None => unhandled.handle(op)
}

import Rows.{Top, erase, moved, typed}

object Eff {

  private[scala2] def of[R, A](p: A ! Top): Eff[R, A] = new Eff(new EffBody(p))

  def pure[A](a: A): Eff[Any, A] = of(okay.pure[A, Top](a))

  /** every effect handled: the answer */
  def run[A](e: Eff[Any, A]): A = e.program.runWith(using Rows.unhandled)

  /** only `Async` left: run it on this thread (JVM, blocking) */
  def runAsync[A](e: Eff[Async, A]): A = e.program.runWith(using Rows.asyncOnly)

  /** a `Prog` is this row exactly */
  def fromProg[A](p: Prog[A]): Eff[Async & Throws[Throwable], A] = of(erase(p.program))

  def toProg[A](e: Eff[Async & Throws[Throwable], A]): Prog[A] = Prog.of(typed[Prog.Row, A](e.program)(using Prog.member))
}

/** the capability: a mutable cell of type `S` */
sealed trait State[S]

object State {
  // okay's names and okay2's type-argument orders throughout
  // (scala2-facade-okay-names, 2026-09-24): a program written here
  // compiles against okay2 with the same lines.

  /** read the current state */
  def get[S]: Eff[State[S], S] = Eff.of(erase(okay.std.State.get[S]))

  /** replace the state, answering the new one */
  def set[S](s: S): Eff[State[S], S] = Eff.of(erase(okay.std.State.set(s)))

  /** apply `f` to the state, answering the new one */
  def modify[S](f: S => S): Eff[State[S], S] = Eff.of(erase(okay.std.State.modify(f)))

  /** handle it from `s`, the rest of the row forwarded: the final state
   * beside the answer */
  def handle[S, A, R](s: S)(e: Eff[State[S] & R, A]): Eff[R, (S, A)] =
    Eff.of(okay.std.State.handle(s)[A, okay.std.State % S + Top](moved[okay.std.State % S, A](e.program)))

  /** run a program whose only capability is `State[S]` to its final
   * state and answer */
  def run[S, A](s: S)(e: Eff[State[S], A]): (S, A) = Eff.run(handle[S, A, Any](s)(e))
}

/** the capability: an environment of type `E` */
sealed trait Reader[E]

object Reader {
  def ask[E]: Eff[Reader[E], E] = Eff.of(erase(okay.std.Reader.ask[E]))

  /** answer every `ask` with `env`, the rest of the row forwarded */
  def run[E, A, R](env: E)(e: Eff[Reader[E] & R, A]): Eff[R, A] =
    Eff.of(okay.std.Reader.run[E, A, okay.std.Reader % E + Top](env)(moved[okay.std.Reader % E, A](e.program)))
}

/** the capability: an output log of `W` */
sealed trait Writer[W]

object Writer {
  def tell[W](w: W): Eff[Writer[W], Unit] = Eff.of(erase(okay.std.Writer.tell(w)))

  /** everything told, in order, beside the answer, as a Vector */
  def collect[W, A, R](e: Eff[Writer[W] & R, A]): Eff[R, (Vector[W], A)] =
    Eff.of(okay.std.Writer.collect[W, A, okay.std.Writer % W + Top](moved[okay.std.Writer % W, A](e.program)))

  /** everything told, in order, beside the answer (a `Seq`, as okay's
   * `run` answers — here the Vector `collect` builds, by covariance) */
  def run[W, A, R](e: Eff[Writer[W] & R, A]): Eff[R, (Seq[W], A)] = collect[W, A, R](e)
}

/** the capability: failure with an `E`, which stops the program */
sealed trait Throws[E]

object Throws {
  def raise[E, A](e: E): Eff[Throws[E], A] = Eff.of(erase(okay.std.raise[E, A](e)))

  /** a failure as a `Left`, the rest of the row forwarded */
  def runEither[A, E, R](e: Eff[Throws[E] & R, A]): Eff[R, Either[E, A]] =
    Eff.of(okay.std.runEither[A, okay.std.Throws % E + Top, E](moved[okay.std.Throws % E, A](e.program)))
}

/** the capability: suspended (possibly blocking) computation */
sealed trait Async

object Async {
  /** suspend `a`; it runs when the program does — `Async(a)`, as okay2
   * spells it (okay's top-level `async(a)`) */
  def apply[A](a: => A): Eff[Async, A] = Eff.of(erase(okay.async(a)))

  /** the program's failure as DATA: it runs on its own fiber, and
   * whatever it threw arrives as a `Left` — okay's and okay2's
   * `Async.attempt` */
  def attempt[A](prog: => Eff[Async, A]): Eff[Async, Either[Throwable, A]] =
    lift(okay.Async.attempt(core(prog)))

  /** suspend `a`, and a throw inside it is a `Throws[Throwable]` failure
   * (the facade's own: okay spells it `attempt` then `fromEither`) */
  def catching[A](a: => A): Eff[Async & Throws[Throwable], A] =
    Async(try Right(a) catch { case NonFatal(e) => Left(e) })
      .flatMap((r: Either[Throwable, A]) => r.fold(Throws.raise[Throwable, A], Eff.pure))

  // ---- concurrency (stage 5), over the platform's own Scheduler and
  // Timer: a virtual thread per fiber on the JVM

  private[scala2] def core[A](e: Eff[Async, A]): A ! okay.Async = typed[okay.Async, A](e.program)
  private[scala2] def lift[A](p: A ! okay.Async): Eff[Async, A] = Eff.of(erase(p))

  /** start `e` on its own fiber; the answer is the running fiber */
  def fork[A](e: Eff[Async, A]): Eff[Async, Fiber[A]] =
    Async(new Fiber(okay.Async.spawn(core(e))))

  /** both at once, both answers */
  def par[A, B](a: Eff[Async, A], b: Eff[Async, B]): Eff[Async, (A, B)] =
    lift(okay.Async.par(core(a), core(b)))

  /** both at once, the first answer; the other is cancelled */
  def race[A](a: Eff[Async, A], b: Eff[Async, A]): Eff[Async, A] =
    lift(okay.Async.race(core(a), core(b)))

  def sleep(millis: Long): Eff[Async, Unit] = lift(okay.Async.sleep(millis))

  /** `e`'s answer, or None if it takes longer than `millis` (it is cancelled) */
  def timeout[A](millis: Long)(e: Eff[Async, A]): Eff[Async, Option[A]] =
    lift(okay.Async.timeout(millis)(core(e)))
}

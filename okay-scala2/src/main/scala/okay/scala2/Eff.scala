package okay.scala2

import okay.{!, Free}
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
 * Underneath is okay's `Free` and okay's own handlers; each operation
 * and each handler below is a one-line call into the Scala 3 library.
 */
final class Eff[-R, A] private (private val body: EffBody[A]) {

  private[scala2] def program: Free[Rows.Top, A] = body.p

  def map[B](f: A => B): Eff[R, B] = Eff.of(body.p.map(f))

  def flatMap[R1 <: R, B](f: A => Eff[R1, B]): Eff[R1, B] =
    Eff.of(body.p.flatMap(a => f(a).program))
}

/** held out of `Eff`'s constructor, as `ProgBody` is out of `Prog`'s */
private[scala2] final class EffBody[A](val p: Free[Rows.Top, A]) extends AnyVal

private[scala2] object Rows {

  /** the row every `Eff` is STORED at: the real row is the phantom `R` */
  type Top[+X] = Any

  /**
   * THE ONE CAST (operator rule: no cast without necessity). On the
   * Scala 2 side the row is a phantom intersection that no Scala 3 type
   * follows, so a program is stored at `Top` and each handler below
   * re-types it at the concrete union row it handles — and the
   * operations re-type their own single-effect programs to `Top`. It is
   * right because `Free` never looks at its row at run time: handlers
   * split operations by their CLASS (TypeableK), and `Eff`'s `R`
   * guarantees statically that every operation in the tree has a
   * handler before `Eff.run` accepts it. There is no typed route:
   * `Free` is invariant in its row and `R` has no Scala 3 counterpart.
   */
  def coerce[F[+_], G[+_], A](p: Free[F, A]): Free[G, A] = p.asInstanceOf[Free[G, A]]
}

import Rows.{Top, coerce}

object Eff {

  private[scala2] def of[R, A](p: Free[Top, A]): Eff[R, A] = new Eff(new EffBody(p))

  def pure[A](a: A): Eff[Any, A] = of(okay.pure(a))

  /** every effect handled: the answer */
  def run[A](e: Eff[Any, A]): A = okay.!.run(coerce[Top, Nothing, A](e.program))

  /** only `Async` left: run it on this thread (JVM, blocking) */
  def runAsync[A](e: Eff[Async, A]): A = coerce[Top, okay.Async, A](e.program).runWith

  /** a `Prog` is this row exactly */
  def fromProg[A](p: Prog[A]): Eff[Async & Throws[Throwable], A] = of(coerce(p.program))

  def toProg[A](e: Eff[Async & Throws[Throwable], A]): Prog[A] = Prog.of(coerce(e.program))
}

/** the capability: a mutable cell of type `S` */
sealed trait State[S]

object State {
  def get[S]: Eff[State[S], S] = Eff.of(coerce(okay.State.get[S]))
  def put[S](s: S): Eff[State[S], Unit] = Eff.of(coerce(okay.State.set(s).map(_ => ())))
  def modify[S](f: S => S): Eff[State[S], Unit] = Eff.of(coerce(okay.State.modify(f).map(_ => ())))

  /** handle it from `s`: the final state beside the answer */
  def run[S, R, A](s: S)(e: Eff[State[S] & R, A]): Eff[R, (S, A)] =
    Eff.of(coerce(okay.State.handle(s)[A, Top](coerce(e.program))))
}

/** the capability: an environment of type `E` */
sealed trait Reader[E]

object Reader {
  def ask[E]: Eff[Reader[E], E] = Eff.of(coerce(okay.Reader.ask[E]))

  def run[E, R, A](env: E)(e: Eff[Reader[E] & R, A]): Eff[R, A] =
    Eff.of(coerce(okay.Reader.run[E, A, Top](env)(coerce(e.program))))
}

/** the capability: an output log of `W` */
sealed trait Writer[W]

object Writer {
  def tell[W](w: W): Eff[Writer[W], Unit] = Eff.of(coerce(okay.Writer.tell(w)))

  /** everything told, in order, beside the answer */
  def run[W, R, A](e: Eff[Writer[W] & R, A]): Eff[R, (Vector[W], A)] =
    Eff.of(coerce(okay.Writer.collect[W, A, Top](coerce(e.program))))
}

/** the capability: failure with an `E`, which stops the program */
sealed trait Throws[E]

object Throws {
  def raise[E, A](e: E): Eff[Throws[E], A] = Eff.of(coerce(okay.raise[E, A](e)))

  /** a failure as a `Left` */
  def run[E, R, A](e: Eff[Throws[E] & R, A]): Eff[R, Either[E, A]] =
    Eff.of(coerce(okay.runEither[A, Top, E](coerce(e.program))))
}

/** the capability: suspended (possibly blocking) computation */
sealed trait Async

object Async {
  /** suspend `a`; it runs when the program does */
  def delay[A](a: => A): Eff[Async, A] = Eff.of(coerce(okay.async(a)))

  /** suspend `a`, and a throw inside it is a `Throws[Throwable]` failure */
  def attempt[A](a: => A): Eff[Async & Throws[Throwable], A] =
    delay(try Right(a) catch { case NonFatal(e) => Left(e) })
      .flatMap((r: Either[Throwable, A]) => r.fold(Throws.raise[Throwable, A], Eff.pure))

  // ---- concurrency (stage 5), over the platform's own Scheduler and
  // Timer: a virtual thread per fiber on the JVM

  private[scala2] def core[A](e: Eff[Async, A]): A ! okay.Async = coerce(e.program)
  private[scala2] def lift[A](p: A ! okay.Async): Eff[Async, A] = Eff.of(coerce(p))

  /** start `e` on its own fiber; the answer is the running fiber */
  def fork[A](e: Eff[Async, A]): Eff[Async, Fiber[A]] =
    delay(new Fiber(okay.Async.spawn(core(e))))

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

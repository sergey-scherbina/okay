package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * The resource effect, tied to no other effect: acquire inside a
 * scope, and the release is the SCOPE's obligation — it runs when the
 * scope ends, in reverse acquisition order, whatever else the program
 * does (region style, as in the region calculus / cats Resource).
 */
enum Resource[+A]:
  /** acquire a resource; the scope releases it at its end */
  case Acquire[R](make: () => R, release: R => Unit) extends Resource[R]

object Resource {

  /** acquire inside the enclosing Resource.run scope */
  inline def acquire[R](make: => R)(release: R => Unit): R ! Resource =
    effect(Acquire(() => make, release))

  /**
   * The scope: run the region, forwarding the effects F. Every
   * acquired release runs when the scope ends — at its value, or at a
   * JVM exception thrown during a step — in reverse acquisition
   * order. Run this handler OUTERMOST: turn aborts into values inside
   * the scope (runEither before run), so no abortive handler discards
   * the finalizers; a multi-shot handler inside replays only the
   * scope's inner part, so each acquire still releases exactly once.
   * A forwarded F-operation suspends the scope with its finalizers
   * carried into the residual — they run when the residual completes
   * (abandoning the residual abandons them).
   */
  def run[A, F[+_]](a: A ! Resource + F): A ! F = {
    def releaseAll(fin: List[() => Unit]): Unit = fin.foreach(_())

    // a while-loop, not @tailrec: the catch must see the CURRENT
    // finalizer list, which a tailrec parameter would hide from it
    def _loop(fin0: List[() => Unit])(x0: A ! Resource + F): A ! F =
      var fin = fin0
      var x = x0
      try
        while true do x.resume match
          case Pure(a) =>
            val f = fin
            fin = Nil
            releaseAll(f)
            return Pure(a)
          case Effect(e) => <|>[Resource, F](e) match
            case Left(Acquire(mk, rel)) =>
              val r = mk()
              val f = (() => rel(r)) :: fin
              fin = Nil
              releaseAll(f)
              return Pure(r)
            case Right(e) =>
              val f = fin
              return Effect(e).map { a => releaseAll(f); a }
          case Bind(Effect(e), k) => <|>[Resource, F](e) match
            case Left(Acquire(mk, rel)) =>
              val r = mk()
              fin = (() => rel(r)) :: fin
              x = k(r)
            case Right(e) =>
              val f = fin
              return Effect(e).flatMap(y => _loop(f)(k(y)))
        throw MatchError(x)
      catch
        case e: Throwable =>
          releaseAll(fin)
          throw e

    _loop(Nil)(a)
  }
}

/**
 * Bracket over any Handler-able row F (Async, Produce, Zero, ...):
 * acquire, use, release — the use-program runs to completion inside
 * one suspension, so no outer handler can skip or repeat the release;
 * a fiber's cancellation is an interrupt exception, and the finally
 * sees it. For a release scoped to a whole program of arbitrary
 * effects, use the Resource effect above instead.
 */
def bracket[R, A, F[+_] : Handler](acquire: => R)(release: R => Unit)(use: R => A ! F): A ! F =
  pure[F, Unit](()).flatMap: _ =>
    val r = acquire
    try pure(use(r).runWith)
    finally release(r)

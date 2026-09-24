package okay2

import scala.annotation.unused

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}

/** the Reader effect: one operation, asking for the environment R.
 * The test is by CLASS only: a row may hold ONE Reader. `Reader % R`
 * is `Reader[R]`. */
sealed trait Reader[R] extends Row { type Op[+A] = Reader.Op[R, A] }

object Reader {
  sealed trait Op[R, +A]
  final case class Ask[R]() extends Op[R, R]

  implicit def effect[R]: Effect[Reader[R]] = Effect.of[Reader[R]]

  /** the environment */
  def ask[R]: R ! Reader[R] = Free.inject[Reader[R], R](Ask())

  /** answer every ask with r, forwarding the rest of the row */
  def run[R, A, Rw <: Row](r: R)(a: Free[Reader[R] with Rw, A])(implicit @unused d: Distinct[Reader[R] with Rw]): A ! Rw =
    runAt[R, A, Rw](r)(a)

  /** `run` at the handler's own shape */
  def runAt[R, A, F <: Row](r: R)(a: Free[Reader[R] with F, A]): A ! F = {
    // the split as a pattern (okay2-handler-allocs)
    val Mine = Split.at[Reader[R]]
    def _loop(x: Free[Reader[R] with F, A]): A ! F = loop(x)

    @tailrec def loop(x: Free[Reader[R] with F, A]): A ! F = Free.resume(x) match {
      case Return(a) => Return(a)
      case Inject(e) => loop(Bind(Inject[Reader[R] + F, A](e), (x: A) => Return[Reader[R] + F, A](x)))
      case Bind(Inject(Mine(_)), k) => loop(k(r))   // Ask is Reader's one operation
      case Bind(Inject(e), k) => Inject[F, Any](e).flatMap(x => _loop(k(x)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(a)
  }

  /** run a sub-program under a modified environment: every ask inside
   * `p` sees `f(r)`, and the row comes out unchanged */
  def local[R, A, Rw <: Row](f: R => R)(p: Free[Reader[R] with Rw, A])(implicit @unused d: Distinct[Reader[R] with Rw]): A ! (Reader[R] + Rw) =
    localAt[R, A, Rw](f)(p)

  /** `local` at the handler's own shape */
  def localAt[R, A, F <: Row](f: R => R)(p: Free[Reader[R] with F, A]): A ! (Reader[R] + F) =
    ask[R].at[Reader[R] + F].flatMap(r => runAt[R, A, F](f(r))(p).at[Reader[R] + F])
}

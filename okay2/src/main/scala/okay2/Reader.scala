package okay2

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}
import Split.split

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
  def run[R, A, Rw <: Row](r: R)(a: A ! Rw)(implicit rm: Remove[Reader[R], Rw]): A ! rm.Out =
    runAt[R, A, rm.Out](r)(rm.split(a))

  /** `run` at the handler's own shape */
  def runAt[R, A, F <: Row](r: R)(a: A ! (Reader[R] + F)): A ! F = {
    def _loop(x: A ! (Reader[R] + F)): A ! F = loop(x)

    @tailrec def loop(x: A ! (Reader[R] + F)): A ! F = Free.resume(x) match {
      case Return(a) => Return(a)
      case Inject(e) => loop(Bind(Inject[Reader[R] + F, A](e), (x: A) => Return[Reader[R] + F, A](x)))
      case Bind(Inject(e), k) =>
        split[Reader[R], F, Any, Either[A ! (Reader[R] + F), A ! F]](e) {
          case Ask() => Left(k(r))
        } { e => Right(Inject[F, Any](e).flatMap(x => _loop(k(x)))) } match {
          case Left(next) => loop(next)
          case Right(done) => done
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(a)
  }

  /** run a sub-program under a modified environment: every ask inside
   * `p` sees `f(r)`, and the row comes out unchanged */
  def local[R, A, Rw <: Row](f: R => R)(p: A ! Rw)(implicit rm: Remove[Reader[R], Rw]): A ! Rw =
    rm.join(localAt[R, A, rm.Out](f)(rm.split(p)))

  /** `local` at the handler's own shape */
  def localAt[R, A, F <: Row](f: R => R)(p: A ! (Reader[R] + F)): A ! (Reader[R] + F) =
    ask[R].at[Reader[R] + F].flatMap(r => runAt[R, A, F](f(r))(p).at[Reader[R] + F])
}

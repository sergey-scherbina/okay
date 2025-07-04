package okay

import scala.annotation.*

infix type ![A, F[_]] = Eff[A, F]
inline def pure[A, F[_]](a: A): A ! F = Eff.Yield(a)
inline def raise[A, F[_]](e: F[A]): A ! F = Eff.Raise(e)
inline def handler[F[_]](using h: Handler[F]) = h

trait Handler[F[_]]:
  type Result[A] = (A, Handler[F])
  def apply[A, B](a: F[A], k: A => B): Result[B]
  def apply[A](a: A | F[A]): Result[A | F[A]]

enum Eff[A, F[_]] {
  case Yield(a: A)
  case Raise(a: F[A])

  private case Continue[A, B, F[_]](a: Eff[A, F],
                                    f: A => Eff[B, F])
    extends Eff[B, F]

  final def flatMap[B](f: A => Eff[B, F]): Eff[B, F] = this match
    case Continue(a, g) => Continue(a, g(_).flatMap(f))
    case a => Continue(a, f)

  inline final def map[B](f: A => B): Eff[B, F] =
    flatMap(f.andThen(Yield(_)))

  @tailrec final def run: (h: Handler[F]) ?=> h.Result[A | F[A]] = this match
    case Yield(a) => handler(a)
    case Raise(e) => handler(e)
    case Continue(c, f) => c match
      case Continue(a, g) => a.flatMap(g(_).flatMap(f)).run
      case Yield(a) => f(a).run
      case Raise(e) => handler(e, f) match
        case (a, h) => a.run(using h)

  final inline def handle(handler: Handler[F]): handler.Result[A | F[A]] =
    run(using handler)

  @tailrec final def apply(n: Long = 0): Handler[F] ?=> Eff[A, F] =
    if (n <= 0) this else this match
      case Continue(Continue(a, f), g) => a.flatMap(f(_).flatMap(g))(n - 1)
      case Continue(Yield(a), f) => f(a)(n - 1)
      case Continue(Raise(e), f) => handler(e, f) match
        case (a, h) => a(n - 1)(using h)
      case _ => this

  final inline def end: Boolean = this match
    case Continue(_, _) => true
    case _ => false

  @tailrec final def ? : Eff[?, F] = this match
    case a@Yield(_) => a
    case e@Raise(_) => e
    case Continue(a, _) => a match
      case Continue(a, _) => a.?
      case _ => a.?

}

type Pure[A] = A

trait Impure extends Handler[Pure]:
  inline override def apply[A](a: A): Result[A] = impure(a) -> this
  inline override def apply[A, B](a: A, k: A => B): Result[B] = k(impure(a)) -> this
  def impure[A](a: A): A

given Impure with
  inline override def impure[A](a: A): A = a

type Produce[A] = A ! Pure
def produce[A](a: A): Produce[A] = raise(a)

given Put[Produce] with
  override inline def put[A](a: A): A \ Produce[A] =
    shift(produce(a).flatMap(_))


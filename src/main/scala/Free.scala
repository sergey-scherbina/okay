package okay

import scala.annotation.tailrec

object Free {
  def pure[F[+_], A](a: A): Free[F, A] = Pure(a)
  def inject[F[+_], A](a: F[A]): Free[F, A] = Inject(a)
  given [F[+_]]: Monad[Free[F, *]] with
    override inline def pure[A](a: A): Free[F, A] = Pure(a)
    extension [A](a: Free[F, A])
      override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
}

enum Free[F[+_], A] {
  case Pure(a: A)
  case Inject(a: F[A])
  case Bind[F[+_], A, B](a: Free[F, A],
                                 f: A => Free[F, B]) extends Free[F, B]
  final def flatMap[B](f: A => Free[F, B]): Free[F, B] = Bind(this, f)
  inline def map[B](f: A => B): Free[F, B] = flatMap(f.andThen(Pure(_)))

  @tailrec final def fold[R](h: [X] => F[X] => (X => Free[F, A]) => R)
                            (p: A => R): R = this match
    case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).fold(h)(p)
    case Bind(Pure(a), f) => f(a).fold(h)(p)
    case Bind(Inject(a), f) => h(a)(f)
    case Inject(a) => h(a)(Pure(_))
    case Pure(a) => p(a)

  def run[M[_] : Monad as M](f: F ==> M): M[A] =
    fold([X] => a => k => f(a).flatMap(k(_).run(f)))(M.pure)
}

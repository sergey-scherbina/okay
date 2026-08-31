package okay

import scala.annotation.tailrec

/**
 * The freer monad (Kiselyov–Ishii 2015, "Freer Monads, More Extensible
 * Effects"): free over any signature F with no Functor requirement,
 * because Bind keeps the continuation as a plain function. Left-nested
 * binds are rebalanced by tail-recursive rotations in fold — which
 * also answers the "reflection without remorse" concern (van der
 * Ploeg–Kiselyov 2014): stepping a program one operation at a time
 * measures within ~8% of running it in bulk here (HandlerBenchmark),
 * so the type-aligned queue of that paper is not needed.
 */
object Free {
  /** a value as a tree */
  inline def pure[F[+_], A](a: A): Free[F, A] = Pure(a)

  /** an operation as a tree */
  inline def inject[F[+_], A](a: F[A]): Free[F, A] = Inject(a)

  /** Free[F, *] is a Monad for every signature F, with no constraint on F */
  given [F[+_]]: Monad[Free[F, *]] with
    override inline def pure[A](a: A): Free[F, A] = Pure(a)
    extension [A](a: Free[F, A])
      override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
}

enum Free[F[+_], A] {
  /** a finished computation */
  case Pure(a: A)

  /** a single operation of the signature F */
  case Inject(a: F[A])

  /** sequencing: run a, then feed its value to the plain-function continuation f */
  case Bind[F[+_], A, B](a: Free[F, A],
                         f: A => Free[F, B]) extends Free[F, B]

  /** sequencing is a data node: nothing runs until an interpreter walks the tree */
  inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = Bind(this, f)

  inline def map[B](f: A => B): Free[F, B] = flatMap(a => Pure(f(a)))

  /**
   * the eliminator: p interprets values, h interprets operations
   * together with their continuations. Left-nested binds are rotated
   * tail-recursively on the way — sound by the monad associativity
   * law, and linear-time amortized for programs built by foldLeft.
   */
  @tailrec final def fold[B](p: A => B)
                            (h: [X] => F[X] => (X => Free[F, A]) => B): B =
    this match
      case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).fold(p)(h)
      case Bind(Pure(a), f) => f(a).fold(p)(h)
      case Bind(Inject(a), f) => h(a)(f)
      case Inject(a) => h(a)(Pure(_))
      case Pure(a) => p(a)

  /** interpret into F's own Monad, operation by operation */
  final def run(using M: Monad[F]): F[A] =
    fold(M.pure)([X] => a => k => a.flatMap(k(_).run))

  /** interpret through a natural transformation into any monad M */
  final def run[M[_] : Monad as M](f: F ==> M): M[A] =
    fold(M.pure)([X] => a => k => f(a).flatMap(k(_).run(f)))

}

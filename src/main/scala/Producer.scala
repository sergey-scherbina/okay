package okay

import scala.util.chaining.*

type Produce[A] = Pure[A]
type Producer[A] = A ! Produce
type Codata[+A]  = A ! Produce   // Free[Pure, A] — trampolined free monad on the identity functor
inline def produce[A](a: A): Producer[A] = effect(a)

given Put[Producer] with
  final override inline def put[A](a: A): A \ Producer[A] =
    shift(produce(a).flatMap(_))

object Producer {
  def log(prefix: String = "", suffix: String = "\n"): Eval[Produce] = new:
    inline def eval[A](a: A): A = a.tap(_.pipe(prefix + _ + suffix).tap(print))
}

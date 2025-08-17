package okay

import scala.util.chaining.*

type Producer[A] = A ! Pure
inline def produce[A](a: A): Producer[A] = effect(a)

given Put[Producer] with
  final override inline def put[A](a: A): A \ Producer[A] =
    shift(produce(a).flatMap(_))

object Producer {
  def log(prefix: String = "", suffix: String = "\n"): Eval[Pure] = new:
    inline def apply[A](a: A): A = a.tap(_.pipe(prefix + _ + suffix).tap(print))
}

package okay

import scala.annotation.tailrec
import scala.util.chaining.*

type Producer[A] = A ! Pure
inline def produce[A](a: A): Producer[A] = effect(a)

given Put[Producer] with
  final override inline def put[A](a: A): A \ Producer[A] =
    shift(produce(a).flatMap(_))

object Producer {
  @tailrec def run[A](e: Producer[A]): !.Handler[Pure] ?=> A = e.foldF(identity)(run)

  def log(prefix: String = "", suffix: String = "\n"): !.Handler[Pure] = new:
    inline def apply[A, B](a: A): A \ B = _(a.tap(_.pipe(prefix + _ + suffix).tap(print)))
}

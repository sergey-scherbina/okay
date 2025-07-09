package okay

import scala.annotation.tailrec
import scala.util.chaining.*

type Producer[A] = A ! Pure
inline def produce[A](a: A): Producer[A] = raise(a)

given Put[Producer] with
  final override inline def put[A](a: A): A \ Producer[A] =
    shift(produce(a).flatMap(_))

extension [A](p: Producer[A])
  inline def run: Effect[Pure] ?=> A = Producer.run(p)

object Producer {
  @tailrec def run[A](e: Producer[A]): Effect[Pure] ?=> A = e.fold(identity)(run)

  def log(prefix: String = "", suffix: String = "\n"): Effect[Pure] = new:
    inline def apply[A](a: A): A = a.tap(_.pipe(prefix + _ + suffix).tap(print))
}

package okay2

/** the test signature: an operation that IS its answer — the Scala 2
 * spelling of `enum Produce[+A] derives Effect` with its handler */
sealed trait Produce extends Row { type Op[+A] = Produce.Emit[A] }

object Produce {
  final case class Emit[+A](a: A)

  implicit val effect: Effect[Produce] = Effect.of[Produce]

  /** each operation answers with its own value */
  implicit val handler: Handler[Produce] = new Handler[Produce] {
    def handle[A](a: Emit[A]): A = a.a
  }

  def produce[A](a: A): A ! Produce = Free.inject[Produce, A](Emit(a))
}

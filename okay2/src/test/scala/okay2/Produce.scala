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

/** a DEFERRED test effect: the operation carries a thunk the handler
 * runs — the shape of the Scala 3 core's `Async.Run`, for tests that
 * assert WHEN a side effect happens (`Produce.Emit` takes a value, so
 * its argument runs at construction) */
sealed trait Later extends Row { type Op[+A] = Later.Run[A] }

object Later {
  final case class Run[+A](f: () => A)

  implicit val effect: Effect[Later] = Effect.of[Later]

  implicit val handler: Handler[Later] = new Handler[Later] {
    def handle[A](a: Run[A]): A = a.f()
  }

  def later[A](f: => A): A ! Later = Free.inject[Later, A](Run(() => f))
}

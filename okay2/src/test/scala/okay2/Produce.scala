package okay2

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

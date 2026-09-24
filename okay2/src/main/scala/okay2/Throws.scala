package okay2


/**
 * The Throws effect fails with any E, and a handler decides what
 * failing means: runEither reifies it into Either, runUnsafe into a
 * JVM throw — so the JVM exception mechanism is just one of the
 * handlers. The test is by CLASS only: a row may hold ONE Throws.
 * `Throws % E` is `Throws[E]`.
 */
sealed trait Throws[E] extends Row { type Op[+A] = Throws.Op[E, A] }

object Throws {
  sealed trait Op[E, +A]
  /** the failure: answers nothing, so it is an operation at EVERY answer type */
  final case class Raise[E](e: E) extends Op[E, Nothing]

  implicit def effect[E]: Effect[Throws[E]] = Effect.of[Throws[E]]

  /** perform the failure */
  def raise[E, A](e: E): A ! Throws[E] = Free.inject[Throws[E], A](Raise(e))

  /** handle Throws by aborting into Either, forwarding the rest of the row */
  def runEither[A, E, R <: Row](a: Free[Throws[E] with R, A])(implicit d: Distinct[Throws[E] with R]): Either[E, A] ! R =
    runEitherAt[A, E, R](a)

  /** `runEither` at the handler's own shape */
  def runEitherAt[A, E, F <: Row](a: Free[Throws[E] with F, A])(implicit d: Distinct[Throws[E] with F]): Either[E, A] ! F =
    Effects.handle[Throws[E], F](a)(a => pure[F, Either[E, A]](Right(a)))(
      new Interpr[Throws[E], Either[E, A] ! F] {
        def apply[X](e: Op[E, X]): Cont[X, Either[E, A] ! F, Either[E, A] ! F] = e match {
          case Raise(err) => shift[X, Either[E, A] ! F, Either[E, A] ! F](_ => pure[F, Either[E, A]](Left(err)))
        }
      })

  /** handle Abort into Option, forwarding the rest of the row */
  def runOption[A, R <: Row](a: Free[Abort with R, A])(implicit d: Distinct[Abort with R]): Option[A] ! R =
    runEitherAt[A, Unit, R](a).map(_.toOption)

  /** handle Throws by actually throwing: the JVM is the handler */
  def runUnsafe[A, E <: Throwable, R <: Row](a: Free[Throws[E] with R, A])(implicit d: Distinct[Throws[E] with R]): A ! R =
    runUnsafeAt[A, E, R](a)

  /** `runUnsafe` at the handler's own shape */
  def runUnsafeAt[A, E <: Throwable, F <: Row](a: Free[Throws[E] with F, A])(implicit d: Distinct[Throws[E] with F]): A ! F =
    Effects.handle[Throws[E], F](a)(a => pure[F, A](a))(
      new Interpr[Throws[E], A ! F] {
        def apply[X](e: Op[E, X]): Cont[X, A ! F, A ! F] = e match {
          case Raise(err) => shift[X, A ! F, A ! F](_ => throw err)
        }
      })
}

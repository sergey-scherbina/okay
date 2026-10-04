package okay2

import scala.annotation.unused


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
          case Raise(err) => Cont.shiftLeaf[X, Either[E, A] ! F, Either[E, A] ! F](_ => pure[F, Either[E, A]](Left(err)))
        }
      })

  /** the handler as a value, level 1: `p.handle(Throws.either[E])` answers `Either[E, A]` */
  def either[E]: Handler[Throws[E], Handler.Or[E]#L] = new Handler.Full[Throws[E], Any, Handler.Or[E]#L, Handler.Nothing] {
    def run[A, F <: Row](p: Free[Throws[E] with F, A])(implicit @unused ev: A <:< Any, d: Distinct[Throws[E] with F], @unused n: Handler.Nothing[F]): Either[E, A] ! F =
      runEitherAt[A, E, F](p)
  }

  /** `Abort` as a value: `p.handle(Throws.option)` answers `Option[A]` */
  val option: Handler[Abort, Option] = new Handler.Full[Abort, Any, Option, Handler.Nothing] {
    def run[A, F <: Row](p: Free[Abort with F, A])(implicit @unused ev: A <:< Any, d: Distinct[Abort with F], @unused n: Handler.Nothing[F]): Option[A] ! F =
      runEitherAt[A, Unit, F](p).map(_.toOption)
  }

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
          case Raise(err) => Cont.shiftLeaf[X, A ! F, A ! F](_ => throw err)
        }
      })
}

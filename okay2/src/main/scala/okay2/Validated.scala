package okay2

/**
 * EVERY ERROR, NOT THE FIRST. `Throws` is monadic, so it stops at the
 * first error — right where a later step needs an earlier answer, wrong
 * for a configuration being read or a form being filled: a user who
 * mistyped four fields should be told about four. The answer is the
 * rung below the monad: an APPLICATIVE cannot bind one leaf's answer
 * into another's body, so it cannot stop early, so it collects.
 *
 *   traverse(fields)(check)        // at Either: the first problem
 *   traverse(fields)(check)        // at Validated: all of them
 *
 * THERE IS DELIBERATELY NO `Monad[Validated]`: the monad-applicative
 * consistency law would make `app` agree with the `flatMap` derivation,
 * which stops at the first error — every `traverse` would quietly turn
 * back into the behaviour this type exists to refuse. When a later step
 * needs an earlier answer, `andThen` says so at the call site.
 */
sealed trait Validated[+E, +A] {
  import Validated._

  /** the Either road back, for a caller that wants to branch */
  def toEither: Either[E, A] = this match {
    case Valid(a) => Right(a)
    case Invalid(e) => Left(e)
  }

  def isValid: Boolean = this match {
    case Valid(_) => true
    case Invalid(_) => false
  }

  /** the value, or a stand-in */
  def getOrElse[B >: A](b: => B): B = this match {
    case Valid(a) => a
    case Invalid(_) => b
  }

  /** THE SHORT-CIRCUITING STEP, named so the reader sees it: `flatMap`
   * in all but the name, and the name is the design — spelled
   * `flatMap` it would invite a Monad, and with it every `traverse`
   * would stop at the first error by law */
  def andThen[E1 >: E, B](f: A => Validated[E1, B]): Validated[E1, B] = this match {
    case Valid(a) => f(a)
    case Invalid(e) => Invalid(e)
  }
}

object Validated {
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]

  /** the value road */
  def valid[E, A](a: A): Validated[E, A] = Valid(a)

  /** the error road */
  def invalid[E, A](e: E): Validated[E, A] = Invalid(e)

  /** an Either read as a validation — the door in */
  def fromEither[E, A](e: Either[E, A]): Validated[E, A] = e match {
    case Right(a) => Valid(a)
    case Left(err) => Invalid(err)
  }

  /**
   * The instance, and `app` is the whole point: two `Invalid`s COMBINE
   * where `Either` would keep the first. `select` is a REAL one: a
   * `Right` scrutinee is already the answer and the handler is SKIPPED,
   * its checks never reported; a FAILED scrutinee does not run the
   * handler either (the `selective` package's `Validation`).
   */
  implicit def selective[E](implicit S: Semigroup[E]): Selective[({ type L[A] = Validated[E, A] })#L] =
    new Selective[({ type L[A] = Validated[E, A] })#L] {
      def pure[A](a: A): Validated[E, A] = Valid(a)

      override def fmap[A, B](v: Validated[E, A], f: A => B): Validated[E, B] = v match {
        case Valid(a) => Valid(f(a))
        case Invalid(e) => Invalid(e)
      }

      def app[A, B](f: Validated[E, A => B], a: Validated[E, A]): Validated[E, B] = (f, a) match {
        case (Valid(g), Valid(x)) => Valid(g(x))
        case (Invalid(e1), Invalid(e2)) => Invalid(S.combine(e1, e2))
        case (Invalid(e1), _) => Invalid(e1)
        case (_, Invalid(e2)) => Invalid(e2)
      }

      def select[A, B](e: Validated[E, Either[A, B]], f: => Validated[E, A => B]): Validated[E, B] = e match {
        case Valid(Right(b)) => Valid(b)              // the handler is SKIPPED
        case Valid(Left(a)) => f match {
          case Valid(g) => Valid(g(a))
          case Invalid(err) => Invalid(err)
        }
        case Invalid(err) => Invalid(err)
      }
    }
}

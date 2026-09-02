package okay

import scala.reflect.*
import scala.util.*
import scala.annotation.implicitNotFound

/**
 * Errors on two levels, with the bridges between them.
 *
 * The Throws effect fails with any E, and a handler decides what
 * failing means: runEither reifies it into Either, runThrows into the
 * throws union, runUnsafe into a JVM throw — so the JVM exception
 * mechanism is just one of the handlers. A throws E is the direct-style
 * union of a value and a JVM-compatible error (E <: Unsafe, so that
 * erasure can tell them apart), and catching reflects such a
 * computation back into the effect, closing the circle.
 */

/** the effect of failing with E */
case class Throws[E, +A](e: E)

/** perform the failure */
inline def raise[E, A](e: E): A ! Throws % E = effect(Throws(e))

/** handle Throws by aborting into Either, forwarding the effects F */
inline def runEither[A, F[+_], E](a: A ! Throws % E + F): Either[E, A] ! F =
  Effects[Free].handle[Throws % E, F, A, Either[E, A]](a)(a => pure(Right(a))):
    [X] => e => shift(_ => pure(Left(e.e)))

/** handle Throws into the throws union (an Either already is one) */
inline def runThrows[A, F[+_], E <: Unsafe](a: A ! Throws % E + F): (A throws E) ! F =
  runEither(a).map(e => e)

/** handle Throws by actually throwing: the JVM is the handler */
inline def runUnsafe[A, F[+_], E <: Unsafe](a: A ! Throws % E + F): A ! F =
  Effects[Free].handle[Throws % E, F, A, A](a)(a => pure(a)):
    [X] => e => shift(_ => throw e.e)

/** reflect a direct-style computation into the effect */
inline def catching[A, E <: Unsafe : Typeable](a: => A throws E): A ! Throws % (E | Unsafe) =
  unsafe(a).wrap match
    case Left(e) => raise(e)
    case Right(x) => pure(x)

/** no error: A throws Safe always holds a value */
type Safe = Nothing

/** any JVM error */
type Unsafe = Throwable

/**
 * The direct-style union of a value and a JVM-compatible error:
 * a plain A, a raw error E, an Either or a Try are all accepted as
 * they are (see the Conversion givens below) and only normalized
 * on elimination by wrap.
 */
opaque infix type throws[+A, +E <: Unsafe] =
  A | E | Either[E, A] | Try[A]

/**
 * for-comprehension in the direct style — `import throws.*` where
 * used. The extensions live in the companion of the union on purpose:
 * at the package level they would capture foreign flatMaps through
 * the Conversion givens below, while a bare `.map` is contested by
 * Comonad[Pure] (the identity functor maps everything) — the local
 * import wins over both.
 */
object throws {
  /** the union absorbs values, errors, Either and Try as they are */
  given [A, E <: Throwable] => Conversion[A, A throws E] = identity
  given [A, E <: Throwable] => Conversion[E, A throws E] = identity
  given [A, E <: Throwable] => Conversion[Either[E, A], A throws E] = identity
  given [A, E <: Throwable] => Conversion[Try[A], A throws E] = identity
  given [A, E <: Throwable] => Conversion[A throws E, Either[E | Unsafe, A]] = _.wrap

  extension [A, E <: Unsafe](a: A throws E)
    // The type ARGUMENTS of Either and Try are erased, so these tests
    // are by class only — and complete anyway, because the union `A
    // throws E` is built from exactly these four shapes: an Either in
    // it can only be an Either[E, A]. `@unchecked` marks precisely
    // that, at the test it applies to, rather than silencing the file.
    def flatMap[B](f: A => B throws E): B throws E = a match
      case e: (Either[E, A] @unchecked) => e.fold(e => e, f)
      case e: (Try[A] @unchecked) => e.fold(t => Failure(t), f)
      case e: (E @unchecked) => e
      case x: (A @unchecked) => f(x)
    inline def map[B](f: A => B): B throws E = flatMap(a => f(a))
}

/** evaluate, catching a thrown E as is and any other Throwable as a Failure */
inline def unsafe[A, E <: Unsafe : Typeable](a: => A throws E): A throws E =
  try a catch {
    case e: E => e
    case e => Failure(e)
  }

extension [A, E <: Unsafe](a: A throws E)
  /** normalize to Either */
  inline def ?? : Either[E | Unsafe, A] = wrap
  def wrap: Either[E | Unsafe, A] = a match {
    // by class, and complete by the union's construction — see flatMap
    case e: (Either[E, A] @unchecked) => e
    case e: (Try[A] @unchecked) => e.toEither
    case e: (E @unchecked) => Left(e)
    case x: (A @unchecked) => Right(x)
  }

  /** the value, mending an error by f */
  inline def ?(f: E | Unsafe => A): A = handle(f)
  inline def handle(f: E | Unsafe => A): A = wrap match {
    case Left(e) => f(e)
    case Right(x) => x
  }

  /** the value, or the error thrown */
  inline def ? : A = unwrap
  @scala.throws[Unsafe]("unwrap unsafe")
  def unwrap: A = a match {
    // by class, and complete by the union's construction — see flatMap
    case e: (Either[E, A] @unchecked) => e.fold(throw _, identity)
    case e: (Try[A] @unchecked) => e.get
    case e: (E @unchecked) => throw e
    case x: (A @unchecked) => x
  }

/** by class only: the payload `e: E` is erased in the type, so a row
 * may hold ONE Throws — see typeableKByClass */
given throwsK[E]: TypeableK[Throws % E] = typeableKByClass(classOf[Throws[?, ?]])

/**
 * The seam direct-try stands on: how a monad CATCHES a JVM throw
 * from the computation's own code. Strict monads (Option, Either,
 * List) run at construction, so a try around the value covers
 * everything; a Free row runs LATER under its handlers, so the
 * instance guards the continuations through the tree — a throw from
 * a pure segment between effects lands in the handler, while a
 * throw from inside an effect's HANDLER stays that handler's
 * business (stated, not hidden). The instances are NAMED, not a
 * catch-all: a lazy monad (a Cont diagonal, Eff) given the strict
 * instance would try the CONSTRUCTION and never the run, and the
 * catch would silently never fire — so an F without an instance is
 * a compile error that says so, and a strict monad of your own
 * declares itself in one line: `given CanTry[M] = CanTry.strict`.
 */
@implicitNotFound("no CanTry[${F}]: `try` in a direct block needs to know how ${F} catches a throw.\nStrict monads (Option, Either, List, Vector, Try) and Free rows have instances;\nfor a strict monad of your own declare `given CanTry[${F}] = CanTry.strict` — a LAZY monad\n(a Cont diagonal, Eff) has no honest instance: its body runs after the try, catch in the run instead.")
trait CanTry[F[_]]:
  def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A]

object CanTry:
  import okay.!.*
  /** strict monads: the whole computation happens at construction */
  def strict[F[_]]: CanTry[F] = new:
    def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A] =
      try fa catch case e: Throwable => h(e)

  given option: CanTry[Option] = strict
  given either: [E] => CanTry[[X] =>> Either[E, X]] = strict
  given list: CanTry[List] = strict
  given vector: CanTry[Vector] = strict
  given tries: CanTry[Try] = strict
  /** Free rows: guard construction AND every continuation step */
  given rows: [Fx[+_]] => CanTry[[X] =>> X ! Fx] = new:
    def tryIn[A](fa: => A ! Fx)(h: Throwable => A ! Fx): A ! Fx =
      def step(p: () => A ! Fx): A ! Fx =
        (try Right(p().resume) catch case e: Throwable => Left(e)) match
          case Left(e) => h(e)
          case Right(Pure(a)) => Free.Pure(a)
          case Right(Effect(op)) => Free.Inject(op)
          case Right(Bind(Effect(op), k)) =>
            Free.Bind(Free.Inject(op), x => step(() => k(x)))
      step(() => fa)

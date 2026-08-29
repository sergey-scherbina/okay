package okay

import scala.reflect.*
import scala.util.*

/**
 * Errors on two levels, with the bridges between them. The Exc effect
 * fails with any E, and a handler decides what failing means: runExc
 * reifies it into Either, runThrows into the throws union, runUnsafe
 * into a JVM throw — so the JVM exception mechanism is just one of the
 * handlers. A throws E is the direct-style union of a value and a
 * JVM-compatible error (E <: Unsafe, so that erasure can tell them
 * apart), and catching reflects such a computation back into the
 * effect, closing the circle.
 */

/** the effect of failing with E */
case class Exc[E, +A](e: E)

inline def raise[E, A](e: E): A ! Exc % E = effect(Exc(e))

/** handle Exc by aborting into Either, forwarding the effects F */
def runExc[A, F[+_], E](a: A ! Exc % E + F): Either[E, A] ! F =
  summon[Effects[Free]].handle[Exc % E, F, A, Either[E, A]](a)(a => pure(Right(a))):
    [X] => e => shift(_ => pure(Left(e.e)))

/** handle Exc into the throws union (an Either already is one) */
def runThrows[A, F[+_], E <: Unsafe](a: A ! Exc % E + F): (A throws E) ! F =
  runExc(a).map(e => e)

/** handle Exc by actually throwing: the JVM is the handler */
def runUnsafe[A, F[+_], E <: Unsafe](a: A ! Exc % E + F): A ! F =
  summon[Effects[Free]].handle[Exc % E, F, A, A](a)(a => pure(a)):
    [X] => e => shift(_ => throw e.e)

/** reflect a direct-style computation into the effect */
inline def catching[A, E <: Unsafe : Typeable](a: => A throws E): A ! Exc % (E | Unsafe) =
  unsafe(a).wrap match
    case Left(e) => raise(e)
    case Right(x) => pure(x)

type Safe = Nothing
type Unsafe = Throwable

/** the direct-style union of a value and a JVM-compatible error */
opaque infix type throws[+A, +E <: Unsafe] =
  A | E | Either[E, A] | Try[A]

extension [A, E <: Unsafe](a: A throws E)
  inline def ?? : Either[E | Unsafe, A] = wrap
  def wrap: Either[E | Unsafe, A] = a match {
    case e: Either[E, A] => e
    case e: Try[A] => e.toEither
    case e: E => Left(e)
    case x: A => Right(x)
  }
  inline def ?(f: E | Unsafe => A): A = handle(f)
  inline def handle(f: E | Unsafe => A): A = wrap match {
    case Left(e) => f(e)
    case Right(x) => x
  }
  inline def ? : A = unwrap
  @scala.throws[Unsafe]("unwrap unsafe")
  def unwrap: A = a match {
    case e: Either[E, A] => e.fold(throw _, identity)
    case e: Try[A] => e.get
    case e: E => throw e
    case x: A => x
  }
inline def unsafe[A, E <: Unsafe : Typeable](a: => A throws E): A throws E =
  try a catch {
    case e: E => e
    case e => Failure(e)
  }

given [A, E <: Throwable] => Conversion[A, A throws E] = identity
given [A, E <: Throwable] => Conversion[E, A throws E] = identity
given [A, E <: Throwable] => Conversion[Either[E, A], A throws E] = identity
given [A, E <: Throwable] => Conversion[Try[A], A throws E] = identity
given [A, E <: Throwable] => Conversion[A throws E, Either[E | Unsafe, A]] = _.wrap

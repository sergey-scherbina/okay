package okay

import scala.reflect.*
import scala.util.*

infix type +[A, B] = A | B
type Safe = Nothing
type Unsafe = Throwable
opaque infix type throws[+A, +E <: Unsafe] =
  A | E | Either[E, A] | Try[A]

extension [A, E <: Unsafe](a: A throws E)
  inline def ?? : Either[E + Unsafe, A] = wrap
  def wrap: Either[E + Unsafe, A] = a match {
    case e: Either[E, A] => e
    case e: Try[A] => e.toEither
    case e: E => Left(e)
    case x: A => Right(x)
  }
  inline def ?(f: E + Unsafe => A): A = handle(f)
  inline def handle(f: E + Unsafe => A): A = wrap match {
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

def unsafe[A, E <: Unsafe : Typeable](a: => A throws E): A throws E =
  try a catch {
    case e: E => e
    case e => Failure(e)
  }

given [A, E <: Throwable] => Conversion[A, A throws E] = identity
given [A, E <: Throwable] => Conversion[E, A throws E] = identity
given [A, E <: Throwable] => Conversion[Either[E, A], A throws E] = identity
given [A, E <: Throwable] => Conversion[Try[A], A throws E] = identity
given [A, E <: Throwable] => Conversion[A throws E, Either[E + Unsafe, A]] = _.wrap

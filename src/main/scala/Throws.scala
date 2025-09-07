package okay

import scala.reflect.*
import scala.util.*

type Safe = Nothing
opaque infix type throws [+A, +E <: Throwable] =
  A | E | Either[E, A] | Try[A]

def unsafe[A, E <: Throwable : Typeable](a: => A throws E): A throws E =
  try a catch {
    case e: E => e
    case e => Failure(e)
  }

given [A, E <: Throwable] => Conversion[A, A throws E] = identity
given [A, E <: Throwable] => Conversion[E, A throws E] = identity
given [A, E <: Throwable] => Conversion[Either[E, A], A throws E] = identity
given [A, E <: Throwable] => Conversion[Try[A], A throws E] = identity

package okay

import scala.annotation.tailrec
import okay.!.*

case class Exc[E, +A](e: E)

inline def raise[E, A](e: Exc[E, A]): A ! Exc % E = effect(e)

def runExc[A, F[+_], E](a: A ! Exc % E + F): Either[E, A] ! F = a match {
  case Pure(a) => pure(Right(a))
  case Effect(Exc(e: E)) => pure(Left(e))
  case a => a.map(runExc)
}

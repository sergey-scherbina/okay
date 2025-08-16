package okay

import scala.annotation.tailrec

case class Exc[E, +A](e: E)

inline def raise[E, A](e: Exc[E, A]): A ! Exc % E = effect(e)

def runExc[A, F[+_], E](a: A ! Exc % E + F): Either[E, A] ! F = a match {
  case !.Pure(a) => Eff.pure(Right(a))
  case !.Effect(Exc(e: E)) => Eff.pure(Left(e))
  case a => a.map(runExc)
}

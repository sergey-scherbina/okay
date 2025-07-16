package okay

case class Exc[E, +A](e: E)

inline def raise[E, A](e: Exc[E, A]): A ! Exc % E = effect(e)

inline def runExc[E, A](a: A ! Exc % E): Either[E, A] = a match {
  case !.Pure(a: A) => Right(a)
  case !.Effect(Exc(e: E), _) => Left(e)
}

def runExc[A, F[+_], E](a: A ! Exc % E + F): Either[E, A] ! F = a match {
  case !.Pure(a) => Eff.pure(Right(a))
  case !.Effect(Exc(e: E), k) => Eff.pure(Left(e))
  case e => e.map(runExc)
}

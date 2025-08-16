package okay

import scala.annotation.tailrec
import scala.util.control.TailCalls.*

infix type %[F[_, _], S] = F[S, *]

enum State[S, +A] {
  case Get() extends State[S, S]
  case Put(s: S) extends State[S, S]
}

object State {
  inline def getState[S]: S ! State % S = effect(Get())
  inline def putState[S](s: S): S ! State % S = effect(Put(s))

  extension [A](a: A)
    def state[S]: A ! State % S = Eff.pure(a)

  def handleState[S, A, F[+_]](s: S)(a: A ! State % S + F): (S, A) ! F = {
    def _loop(s: S)(x: A ! State % S + F): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! State % S + F): (S, A) ! F = x match
      case !.Continue(!.Continue(a, h), k) => loop(s)(a.flatMap(h(_).flatMap(k)))
      case !.Continue(!.Pure(a), k) => loop(s)(k(a))
      case !.Pure(a) => pure((s, a))
      case !.Effect(e) => Eff.<|>[State[S, *], F](e) match
        case Left(Get()) => pure((s, s))
        case Left(Put(s)) => pure((s, s))
        case Right(e) => effect(e).map((s, _))
      case !.Continue(!.Effect(e), k) => Eff.<|>[State[S, *], F](e) match
        case Left(Get()) => loop(s)(k(s))
        case Left(Put(s)) => loop(s)(k(s))
        case Right(e) => effect(e).flatMap(x => _loop(s)(k(x)))

    loop(s)(a)
  }

  inline def runState[S, A](s: S)(a: A ! State % S): (S, A) =
    Eff.run(handleState(s)(a))

  def index[A](seq: Seq[A], begin: Long = 0): (Long, Seq[(Long, A)]) = runState(begin):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- getState; _ <- putState(n + 1) yield (n, a) +: xs

}

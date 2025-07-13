package okay

import scala.annotation.tailrec

infix type %[F[_, _], S] = F[S, *]

enum State[S, A] {
  case Get() extends State[S, S]
  case Put(s: S) extends State[S, S]
}

object State {
  inline def getState[S]: S ! State % S = effect(Get())
  inline def putState[S](s: S): S ! State % S = effect(Put(s))

  extension [A](a: A)
    def state[S]: A ! State % S = Eff.pure(a)

  @tailrec def runState[S, A](s: S)(a: A ! State % S): (S, A) = a match
    case !.Pure(a) => (s, a)
    case !.Effect(x, k) => x match
      case State.Get() => runState(s)(k(s))
      case State.Put(s) => runState(s)(k(s))

  def index[A](seq: Seq[A], begin: Long = 0): (Long, Seq[(Long, A)]) = runState(begin):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- getState; _ <- putState(n + 1) yield (n, a) +: xs

}

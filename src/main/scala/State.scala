package okay

import scala.annotation.tailrec

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

  @tailrec def handleState[S, A, F[+_]](s: S)(a: A ! State % S + F): (S, A) ! F = a match
    case !.Pure(a) => pure((s, a))
    case !.Effect(x, k) => Eff.<|>[State[S, *], F](x) match
      case Left(Get()) => handleState(s)(k(s))
      case Left(Put(s)) => handleState(s)(k(s))
      case Right(e) => effect(e, x => _handleState(s)(k(x)))
  private def _handleState[S, A, F[+_]] = handleState[S, A, F]

  inline def runState[S, A](s: S)(a: A ! State % S): (S, A) =
    Eff.run(handleState(s)(a))

  def index[A](seq: Seq[A], begin: Long = 0): (Long, Seq[(Long, A)]) = runState(begin):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- getState; _ <- putState(n + 1) yield (n, a) +: xs

}

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
    def loop(s: S)(x: A ! State % S + F): (S, A) !! F = x match
      case !.Pure(a) => done(pure((s, a)))
      case !.Effect(x, k) => Eff.<|>[State[S, *], F](x) match
        case Left(Get()) => tailcall(k(s).flatMap(loop(s)))
        case Left(Put(s)) => tailcall(k(s).flatMap(loop(s)))
        case Right(e) => done(effect(e, x => tailcall(k(x).flatMap(loop(s)))))

    loop(s)(a).result
  }

  inline def runState[S, A](s: S)(a: A ! State % S): (S, A) =
    Eff.run(handleState(s)(a))

  def index[A](seq: Seq[A], begin: Long = 0): (Long, Seq[(Long, A)]) = runState(begin):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- getState; _ <- putState(n + 1) yield (n, a) +: xs

}

package okay

import scala.annotation.tailrec
import scala.util.control.TailCalls.*
import okay.Eff.*

infix type %[F[_, _], S] = F[S, *]

enum State[S, +A] {
  case Get() extends State[S, S]
  case Put(s: S) extends State[S, S]
}

extension [A](a: A)
  def state[S]: A ! State % S = Eff.pure(a)

object State {
  inline def get[S]: S ! State % S = effect(Get())
  inline def set[S](s: S): S ! State % S = effect(Put(s))

  def handle[S, A, F[+_]](s: S)(a: A ! State % S + F): (S, A) ! F = {
    def _loop(s: S)(x: A ! State % S + F): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! State % S + F): (S, A) ! F = x match
      case Continue(Continue(a, h), k) => loop(s)(a.flatMap(h(_).flatMap(k)))
      case Continue(Pure(a), k) => loop(s)(k(a))
      case Pure(a) => Pure((s, a))
      case Effect(e) => <|>[State[S, *], F](e) match
        case Left(Get()) => Pure((s, s))
        case Left(Put(s)) => Pure((s, s))
        case Right(e) => Effect(e).map((s, _))
      case Continue(Effect(e), k) => <|>[State[S, *], F](e) match
        case Left(Get()) => loop(s)(k(s))
        case Left(Put(s)) => loop(s)(k(s))
        case Right(e) => Effect(e).flatMap(x => _loop(s)(k(x)))

    loop(s)(a)
  }

  inline def run[S, A](s: S)(a: A ! State % S): (S, A) =
    Eff.run(handle(s)(a))

  def index[A](seq: Seq[A], from: Long = 0): (Long, Seq[(Long, A)]) = run(from):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- get; _ <- set(n + 1) yield (n, a) +: xs

}

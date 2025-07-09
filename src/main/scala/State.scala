package okay

import scala.annotation.tailrec

infix type %[F[_, _], S] = F[S, *]

enum State[S, +A] {
  case Get(f: S => A)
  case Set(a: A, s: S)
}

object State {
  def get[S]: S ! State % S = raise(Get(identity))
  def set[S](s: S): S ! State % S = raise(Set(s, s))

  extension [A](a: A)
    def state[S]: A ! State % S = Free.pure(a)

  given [S]: Functor[State % S] with
    override def fmap[A, B](a: State[S, A], f: A => B): State[S, B] = a match
      case Get(g) => Get(s => f(g(s)))
      case Set(a, s) => Set(f(a), s)

  @tailrec def runState[S, A](s: S)(a: A ! State % S): (A, S) =
    a.fold((_, s)):
      case Get(f) => runState(s)(f(s))
      case Set(a, s) => runState(s)(a)

  def index[A](seq: Seq[A], begin: Long = 0): Seq[(Long, A)] = runState(begin):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- get; _ <- set(n + 1) yield (n, a) +: xs
  ._1.reverse

}

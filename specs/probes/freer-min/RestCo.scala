package okay.min2

/** a handler's REST on the covariant kernel: does `G` infer exactly from `Diag[Ask] + G` against a 3-member row? */
object RestCo:
  import Probe.*
  enum Cnt[S, R, +A]:
    case Tick[T]() extends Cnt[T, T, Unit]
  type Three = Diag[Ask] + Diag[Say] + Cnt

  val three: Freer[Three, Unit, Unit, Int] =
    for
      n <- effect(Ask.Number)
      _ <- effect(Say.Line("x"))
      _ <- perform(Cnt.Tick[Unit]())
    yield n

  def runAsk[G[_, _, +_], A](p: Freer[Diag[Ask] + G, Unit, Unit, A], in: Int): Freer[G, Unit, Unit, A] = ???
  def runAll[G[_, _, +_], A](p: Freer[Diag[Ask] + Diag[Say] + G, Unit, Unit, A]): Freer[G, Unit, Unit, A] = ???

  val rest2: Freer[Diag[Say] + Cnt, Unit, Unit, Int] = runAsk(three, 1)   // two members left
  val rest1: Freer[Cnt, Unit, Unit, Int] = runAll(three)                   // one member left
  val r0 = 0
  val show0 = 0

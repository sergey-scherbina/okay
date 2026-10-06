package okay.min3

/** positional: the handled member LEFT, the rest as one application on the RIGHT — master's shape */
object RestPos:
  import okay.min2.Probe.{Ask, Say}
  import RestInv.Cnt
  type Rest = Diag[Say] + Cnt
  val three: Freer[Diag[Ask] + Rest, Unit, Unit, Int] =
    for
      n <- effect(Ask.Number)
      _ <- effect(Say.Line("x"))
      _ <- perform(Cnt.Tick[Unit]())
    yield n
  def runAsk[G[_, _, +_], A](p: Freer[Diag[Ask] + G, Unit, Unit, A], in: Int): Freer[G, Unit, Unit, A] = ???
  val r = runAsk(three, 1)
  val ok: Freer[Rest, Unit, Unit, Int] = r
  val show: String = r

object RestPosCo:
  import okay.min2.{Freer, Diag, +, effect, perform}
  import okay.min2.Probe.{Ask, Say}
  import okay.min2.RestCo.Cnt
  type Rest = Diag[Say] + Cnt
  val three: Freer[Diag[Ask] + Rest, Unit, Unit, Int] =
    for
      n <- effect(Ask.Number)
      _ <- effect(Say.Line("x"))
      _ <- perform(Cnt.Tick[Unit]())
    yield n
  def runAsk[G[_, _, +_], A](p: Freer[Diag[Ask] + G, Unit, Unit, A], in: Int): Freer[G, Unit, Unit, A] = ???
  val r = runAsk(three, 1)
  val ok: Freer[Rest, Unit, Unit, Int] = r
  val show: String = r

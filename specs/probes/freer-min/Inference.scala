//> using scala 3.9.0
package okay.freer

object Probe:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]

  /** the prompt names the row */
  final class KPrompt[F[_, _, +_], S, Y](val label: String)

  /** the body's row G is inferred BOTTOM-UP (no expected row reaches the for); membership is an evidence, a
   * `<:<` with no type variable in it, which is also the widening */
  def resetE[F[_, _, +_], S, Y, G[_, _, +_]](p: KPrompt[F, S, Y])(body: Freer[G, S, S, Y])
                                            (using ev: Freer[G, S, S, Y] <:< Freer[Row[F], S, S, Y]): Freer[Row[F], S, S, Y] = ???
  final class AtE[X]:
    def apply[F[_, _, +_], S, Y, G[_, _, +_]](p: KPrompt[F, S, Y])(f: (X => Freer[Row[F], S, S, Y]) => Freer[G, S, S, Y])
                                            (using ev: Freer[G, S, S, Y] <:< Freer[Row[F], S, S, Y]): Freer[Row[F], S, S, X] = ???
  def shiftE[X]: AtE[X] = AtE[X]()

  val kp = KPrompt[Diag[Ask] + Diag[Say], Unit, Int]("kp")
  val kq = KPrompt[Diag[Ask] + Diag[Say], Unit, Int]("kq")

  // a for inline under reset, k used twice inside the shift body, two effects in the row
  val r2 = resetE(kp)(
    for
      n <- inject(Ask.Number)
      x <- shiftE[Int](kp)(k => k(n).flatMap(k))
      _ <- inject(Say.Line(x.toString))
    yield x + 1)
  val r2Show: String = r2

  // nested delimiters, a shift to the outer one from inside the inner
  val r3 = resetE(kp)(resetE(kq)(shiftE[Int](kp)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))
  val r3Show: String = r3

  // REFUSED: an effect not in the prompt's row
  enum Other[+A]:
    case Op extends Other[Int]
  val r4 = resetE(kp)(inject(Other.Op).map(_ + 1))

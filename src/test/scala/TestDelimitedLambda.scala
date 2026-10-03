package okay

import okay.Freer.{Return, Inject}

/**
 * A SECOND EFFECT on the same machine (specs/cont-atm.md): delimited control with named prompts, the shape of
 * `Shift % P` — programs at `Unit` as `Free`'s are, a prompt a VALUE boundary: a mark on the segment, which a value
 * passes and a capture stops at. `push` hangs it, `shift0` cuts the segment there (`split`), a resumption puts the
 * frames back over it (`join`). The machine is the one Cont runs on, unchanged.
 */
class TestDelimitedLambda extends munit.FunSuite:

  /** a prompt: a mark, with the value its place answers */
  final class Prompt[Y](val name: String) extends Delimited.Mark

  /** a captured continuation: the frames above the prompt, and the prompt, put back on resumption (deep) */
  final case class Cap[A, Y](above: Frames[L, A, Y, Unit, Unit], p: Prompt[Y])

  sealed trait L[S, R, +A]
  /** `body` under `p`: its value is the value here */
  final case class Push[Y](p: Prompt[Y], body: Freer[L, Unit, Unit, Y]) extends L[Unit, Unit, Y]
  /** capture to `p`, through any prompts between; the body answers in `p`'s place */
  final case class Shift0[A, Y](p: Prompt[Y], body: Cap[A, Y] => Freer[L, Unit, Unit, Y]) extends L[Unit, Unit, A]
  /** a captured continuation resumed: its frames and its prompt back on top, `a` into them */
  final case class Resume[A, Y](k: Cap[A, Y], a: A) extends L[Unit, Unit, Y]

  object Steps extends Step[L, L]:
    def step[A, B, S, T, R, Z](op: L[T, R, A], k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z],
                               machine: Delimited[L]): Delimited.Next[L, Z] = op match
      case Push(p, body) => machine.next(body, machine.mark(p, k), m)
      case Shift0(p, body) => machine.split(k, _ eq p) match
        case null => throw IllegalStateException(s"no delimiter for prompt ${p.name}")
        case s: Delimited.Split[L, A, B, S, Unit] =>
          val (y, i) = installed(s, p)
          val above = i.substituteCo[[x] =>> Frames[L, A, s.Y, x, Unit]](s.above)
          val below = i.substituteCo[[x] =>> Frames[L, s.Y, B, S, x]](s.below)
          machine.next(body(Cap(y.substituteCo[[v] =>> Frames[L, A, v, Unit, Unit]](above), p)),
            y.substituteCo[[v] =>> Frames[L, v, B, S, Unit]](below), m)
      case Resume(cap, a) => machine.next(Return(a), machine.join(cap.above, machine.mark(cap.p, k)), m)

  /**
   * THE ONE CLAIM of this effect, the generative-prompt axiom (Dybvig, Peyton Jones & Sabry's `eqPrompt`): a mark
   * that is prompt `p` was hung by `Push` at `p`, so the value at its place is `p`'s, and the index there `Unit`,
   * as every program of this effect is. It is the effect's, not the machine's: the machine never relates two marks.
   */
  private def installed[A, B, S, Y](s: Delimited.Split[L, A, B, S, Unit], @annotation.unused p: Prompt[Y]): (s.Y =:= Y, s.S1 =:= Unit) =
    (summon[s.Y =:= s.Y].asInstanceOf[s.Y =:= Y], summon[s.S1 =:= s.S1].asInstanceOf[s.S1 =:= Unit])

  private def run[A](c: Freer[L, Unit, Unit, A]): A = Delimited(Steps).value(c)
  private def pure[A](a: A): Freer[L, Unit, Unit, A] = Return(a)
  private def reset[Y](p: Prompt[Y])(body: Freer[L, Unit, Unit, Y]): Freer[L, Unit, Unit, Y] = Inject(Push(p, body))
  private def shift0[A, Y](p: Prompt[Y])(body: Cap[A, Y] => Freer[L, Unit, Unit, Y]): Freer[L, Unit, Unit, A] =
    Inject(Shift0(p, body))
  private def resume[A, Y](k: Cap[A, Y], a: A): Freer[L, Unit, Unit, Y] = Inject(Resume(k, a))

  test("a capture by name crosses another prompt's place, and its k puts that place back") {
    val p = Prompt[Int]("p")
    val q = Prompt[String]("q")
    val c = reset(p)(reset(q)(shift0[Int, Int](p)(k => resume(k, 10).flatMap(a => resume(k, 1000).map(b => a + b)))
      .map(_.toString)).map(_.length))
    // k(10) = "10".length = 2; k(1000) = 4
    assertEquals(run(c), 6)
  }

  test("multi-shot through a prompt") {
    val p = Prompt[List[Int]]("p")
    val q = Prompt[List[Int]]("q")
    val c = reset(p)(reset(q)(shift0[Int, List[Int]](p)(k => resume(k, 1).flatMap(a => resume(k, 2).map(b => a ++ b)))
      .map(x => List(x, -x))).map(_.map(_ * 10)))
    assertEquals(run(c), List(10, -10, 20, -20))
  }

  test("a prompt not pushed fails by name") {
    val p = Prompt[Int]("absent")
    val e = intercept[IllegalStateException](run(shift0[Int, Int](p)(_ => pure(0))))
    assert(e.getMessage.nn.contains("absent"), e.getMessage)
  }

  test("stack safety on 256 KB: a capture through 100 000 prompts") {
    val p = Prompt[Int]("p")
    val levels = 100000
    val inner = shift0[Int, Int](p)(k => resume(k, 1))
    val deep = (1 to levels).foldLeft(inner)((c, i) => reset(Prompt[Int](s"q$i"))(c).map(_ + 1))
    var out = 0
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = run(reset(p)(deep)) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    assertEquals(out, 1 + levels)
  }

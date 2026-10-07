package okay.freer

import okay.*

import okay.freer.Freer.{Return, Inject}

/**
 * A SECOND EFFECT on the same machine (specs/cont-atm.md): delimited control with named prompts, the shape of
 * `Shift % P` — programs at `Unit` as `Free`'s are, a prompt a VALUE boundary on the stack: a value passes it, a
 * capture to it stops there. `push` installs it, `shift0` cuts the stack there (`cut`), a resumption puts the piece
 * back (`reinstall`). The machine is the one Cont runs on, unchanged.
 */
class TestDelimitedLambda extends munit.FunSuite:

  /** a prompt: a boundary's mark, with the value its place answers */
  final class Prompt[Y](val name: String) extends Delimited.Mark

  sealed trait L[S, R, +A]
  /** `body` under `p`: its value is the value here */
  final case class Push[Y](p: Prompt[Y], body: Freer[L, Unit, Unit, Y]) extends L[Unit, Unit, Y]
  /** capture to `p`, through any prompts between; the body answers in `p`'s place */
  final case class Shift0[A, Y](p: Prompt[Y], body: Delimited.Piece[L, A, Unit, Y, Unit] => Freer[L, Unit, Unit, Y])
    extends L[Unit, Unit, A]
  /** a captured piece resumed: back on top, `a` into it */
  final case class Resume[A, Y](k: Delimited.Piece[L, A, Unit, Y, Unit], a: A) extends L[Unit, Unit, Y]

  object Steps extends Delimited.Step[L, L]:
    def step[A, B, S, T, R, Z](op: L[T, R, A], k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z],
                               machine: Delimited[L]): Delimited.Next[L, Z] = op match
      case Push(p, body) => machine.next(body, machine.end, machine.delim(p, k, m))
      case sh: Shift0[a, ?] => machine.cut[a, B, S, T, R, Z](k, m, _ eq sh.p, _ => false) match
        case null => throw IllegalStateException(s"no delimiter for prompt ${sh.p.name}")
        case f =>
          val (is, at) = installed(f, sh.p)
          val piece = is.substituteCo[[v] =>> Delimited.Piece[L, a, Unit, v, Unit]](at.substituteCo[[x] =>> Delimited.Piece[L, a, Unit, f.Y, x]](f.piece))
          val out = is.substituteCo[[v] =>> Frames[L, v, f.B2, f.S2, Unit]](at.substituteCo[[x] =>> Frames[L, f.Y, f.B2, f.S2, x]](f.out))
          machine.next(sh.body(piece), out, f.rest)
      case Resume(piece, a) => machine.reinstall(piece, Return(a), k, m)

  /**
   * THE ONE CLAIM of this effect, the generative-prompt axiom (Dybvig, Peyton Jones & Sabry's `eqPrompt`): a
   * boundary marked by prompt `p` was installed by `Push` at `p`, so the value at its place is `p`'s, and the index
   * there `Unit`, as every program of this effect is. It is the effect's, not the machine's.
   */
  private def installed[A, R, Z, Y](f: Delimited.Found[L, A, Unit, R, Z], @annotation.unused p: Prompt[Y]): (f.Y =:= Y, f.I =:= Unit) =
    (summon[f.Y =:= f.Y].asInstanceOf[f.Y =:= Y], summon[f.I =:= f.I].asInstanceOf[f.I =:= Unit])

  private def run[A](c: Freer[L, Unit, Unit, A]): A = Delimited(Steps).value(c)
  private def pure[A](a: A): Freer[L, Unit, Unit, A] = Return(a)
  private def reset[Y](p: Prompt[Y])(body: Freer[L, Unit, Unit, Y]): Freer[L, Unit, Unit, Y] = Inject(Push(p, body))
  private def shift0[A, Y](p: Prompt[Y])(body: Delimited.Piece[L, A, Unit, Y, Unit] => Freer[L, Unit, Unit, Y]): Freer[L, Unit, Unit, A] =
    Inject(Shift0(p, body))
  private def resume[A, Y](k: Delimited.Piece[L, A, Unit, Y, Unit], a: A): Freer[L, Unit, Unit, Y] = Inject(Resume(k, a))

  test("a capture by name crosses another prompt's place, and its k puts that place back") {
    val p = new Prompt[Int]("p")
    val q = new Prompt[String]("q")
    val c = reset(p)(reset(q)(shift0[Int, Int](p)(k => resume(k, 10).flatMap(a => resume(k, 1000).map(b => a + b)))
      .map(_.toString)).map(_.length))
    // k(10) = "10".length = 2; k(1000) = 4
    assertEquals(run(c), 6)
  }

  test("multi-shot through a prompt") {
    val p = new Prompt[List[Int]]("p")
    val q = new Prompt[List[Int]]("q")
    val c = reset(p)(reset(q)(shift0[Int, List[Int]](p)(k => resume(k, 1).flatMap(a => resume(k, 2).map(b => a ++ b)))
      .map(x => List(x, -x))).map(_.map(_ * 10)))
    assertEquals(run(c), List(10, -10, 20, -20))
  }

  test("a prompt not pushed fails by name") {
    val p = new Prompt[Int]("absent")
    val e = intercept[IllegalStateException](run(shift0[Int, Int](p)(_ => pure(0))))
    assert(e.getMessage.nn.contains("absent"), e.getMessage)
  }

  test("stack safety on 256 KB: a capture through 100 000 prompts") {
    val p = new Prompt[Int]("p")
    val levels = 100000
    val inner = shift0[Int, Int](p)(k => resume(k, 1))
    val deep = (1 to levels).foldLeft(inner)((c, i) => reset(new Prompt[Int](s"q$i"))(c).map(_ + 1))
    var out = 0
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = run(reset(p)(deep)) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    assertEquals(out, 1 + levels)
  }

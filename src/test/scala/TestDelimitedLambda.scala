package okay

import okay.Freer.{Return, Inject}
import Delimited.{Next, Cap}

/**
 * A SECOND EFFECT on the same stack (specs/cont-atm.md): λ$ with named prompts — `ret $ body` at a prompt, and
 * `shift0` to a prompt through any boundaries between. Written with nothing but the stack's own manipulations
 * (`Stack.Bound`, `cut`, `reinstall`); the machine is the one Cont runs on, unchanged.
 */
class TestDelimitedLambda extends munit.FunSuite:

  /** a prompt: a boundary's mark, with the answer every installation of it has */
  final class Prompt[Y](val name: String) extends Delimited.Tag[Y]

  /** a shift0 body: in the marked level's place, at whatever answers outside it */
  type Away[A, Y] = [W] => Cap[L, A, Y] => Freer[L, W, W, Y]

  sealed trait L[S, R, +A]
  /** `ret $ body` at `p`: inside, a level of its own answering `R`; outside, that answer as a value */
  final case class Dollar[A, B, T, R, X](p: Prompt[R], ret: A => Freer[L, B, T, B], body: Freer[L, T, R, A]) extends L[X, X, R]
  /** capture to `p`, through any boundaries between */
  final case class Shift0[A, Y, X](p: Prompt[Y], body: Away[A, Y]) extends L[X, X, A]
  /** a captured continuation resumed: its boundaries put back, its answer the value */
  final case class Resume[A, Z, X](k: Cap[L, A, Z], a: A) extends L[X, X, Z]

  object Steps extends Delimited.Effect[L]:
    def step[A, S, T, R](op: L[S, T, A], k: Frames[L, A, S], m: Stack[L, T, R], machine: Delimited[L]): Next[L, R] = op match
      case Dollar(p, ret, body) => Next(body, Frames.Frame(ret, Frames.End()), Stack.Bound(p, k, m))
      case s: Shift0[a, ?, ?] => Delimited.cut[L, a, T, R](k, m, _ eq s.p) match
        case null => throw IllegalStateException(s"no delimiter for prompt ${s.p.name}")
        case f: Delimited.Found[L, a, R] =>
          val is = same(f.cap.tag, s.p)
          Next(s.body[f.U](is.substituteCo[[t] =>> Cap[L, a, t]](f.cap)), is.substituteCo[[t] =>> Frames[L, t, f.U]](f.out), f.m)
      case Resume(cap, a) => Delimited.reinstall(cap, a, k, m)

  /**
   * THE ONE CLAIM of this effect, the generative-prompt axiom (Dybvig, Peyton Jones & Sabry's `eqPrompt`): a
   * boundary marked by a prompt was put there at that prompt's answer — `Dollar` is the only one that marks it.
   * It is the effect's, not the stack's: the stack never relates two marks.
   */
  private def same[T, Y](@annotation.unused t: Delimited.Tag[T], @annotation.unused p: Prompt[Y]): T =:= Y =
    summon[T =:= T].asInstanceOf[T =:= Y]

  private def run[A](c: Freer[L, A, A, A]): A = Delimited(Steps).run(c, identity)
  private def pure[A, R](a: A): Freer[L, R, R, A] = Return(a)
  private def reset[A, R, X](p: Prompt[R])(body: Freer[L, A, R, A]): Freer[L, X, X, R] =
    Inject(Dollar[A, A, A, R, X](p, (a: A) => Return[L, A, A](a), body))
  private def shift0[A, Y, X](p: Prompt[Y])(body: Away[A, Y]): Freer[L, X, X, A] = Inject(Shift0[A, Y, X](p, body))
  private def resume[A, Z, X](k: Cap[L, A, Z], a: A): Freer[L, X, X, Z] = Inject(Resume[A, Z, X](k, a))

  test("a capture by name crosses another prompt's boundary, and its k puts that boundary back") {
    val p = new Prompt[Int]("p")
    val q = new Prompt[String]("q")
    val c: Freer[L, Int, Int, Int] =
      reset(p)(reset[String, String, Int](q)(shift0[Int, Int, String](p)([W] => k =>
        resume[Int, Int, W](k, 10).flatMap(a => resume[Int, Int, W](k, 1000).map(b => a + b))).map(_.toString))
        .map(_.length))
    // k(10) = "10".length = 2; k(1000) = 4
    assertEquals(run(c), 6)
  }

  test("multi-shot through a boundary") {
    val p = new Prompt[List[Int]]("p")
    val q = new Prompt[List[Int]]("q")
    val c: Freer[L, List[Int], List[Int], List[Int]] =
      reset(p)(reset[List[Int], List[Int], List[Int]](q)(shift0[Int, List[Int], List[Int]](p)([W] => k =>
        resume[Int, List[Int], W](k, 1).flatMap(a => resume[Int, List[Int], W](k, 2).map(b => a ++ b)))
        .map(x => List(x, -x))).map(_.map(_ * 10)))
    assertEquals(run(c), List(10, -10, 20, -20))
  }

  test("a prompt not installed fails by name") {
    val p = new Prompt[Int]("absent")
    val e = intercept[IllegalStateException](run(shift0[Int, Int, Int](p)([W] => _ => pure[Int, W](0))))
    assert(e.getMessage.nn.contains("absent"), e.getMessage)
  }

  test("stack safety on 256 KB: a capture through 100 000 boundaries") {
    val p = new Prompt[Int]("p")
    val levels = 100000
    val inner: Freer[L, Int, Int, Int] = shift0[Int, Int, Int](p)([W] => k => resume[Int, Int, W](k, 1))
    val deep = (1 to levels).foldLeft(inner)((c, i) => reset[Int, Int, Int](new Prompt[Int](s"q$i"))(c).map(_ + 1))
    var out = 0
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = run(reset[Int, Int, Int](p)(deep)) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    assertEquals(out, 1 + levels)
  }

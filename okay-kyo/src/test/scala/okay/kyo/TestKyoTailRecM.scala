package okay.kyo

import _root_.kyo.{<, Env}

/** kyo's loop (specs/eager-carrier-depth.md): `Loop`, not the `flatMap`
 * recursion — a pure kyo value maps at once — so a million iterations
 * run on a 128 KB thread, pure and under an effect */
class TestKyoTailRecM extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private def onSmallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 128L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  val n = 1000000

  test("pure kyo: a million iterations on a 128 KB thread") {
    val R = summon[okay.TailRecM[Pending[Any]]]
    assertEquals(onSmallStack(R.tailRecM(0)(i => if i < n then Left(i + 1) else Right(i)).eval), n)
  }

  test("under Env: a million iterations on a 128 KB thread") {
    val R = summon[okay.TailRecM[Pending[Env[Int]]]]
    val k = R.tailRecM(0)(i => Env.use[Int](step => if i < n then Left(i + step) else Right(i)))
    assertEquals(onSmallStack(Env.run(1)(k).eval), n)
  }
}

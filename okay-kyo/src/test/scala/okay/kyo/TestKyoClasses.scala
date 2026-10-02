package okay.kyo

import _root_.kyo.{<, Abort, AllowUnsafe, Duration, Emit, Env, IO, KyoApp}
import java.util.concurrent.{CountDownLatch, TimeUnit}

/**
 * okay's class ladder over kyo's `A < S` (specs/interop-classes.md):
 * the monad for every effect set, and the parallel applicative chosen
 * at the call site.
 */
class TestKyoClasses extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private def runAsync[A: _root_.kyo.Flat](k: A < (Abort[Nothing] & _root_.kyo.Async)): A =
    import AllowUnsafe.embrace.danger
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(k).getOrThrow

  test("okay.traverse over A < Env reads the environment for each leaf") {
    val k = okay.traverse[Pending[Env[Int]], Int, Int](Seq(1, 2, 3))(i => Env.use[Int](_ + i))
    assertEquals(Env.run(10)(k).eval, Seq(11, 12, 13))
  }

  test("okay.traverse over A < Emit keeps the order of what was told") {
    val k = okay.traverse[Pending[Emit[String]], String, Int](Seq("a", "b"))(s => Emit.valueWith(s)(s.length))
    val (told, out) = Emit.run[String](k).eval
    assertEquals(told.toList, List("a", "b"))
    assertEquals(out, Seq(1, 1))
  }

  test("whenS over kyo: the body runs only when the condition holds") {
    val S = summon[okay.Selective[Pending[Emit[String]]]]
    val k = S.ifS(Emit.valueWith("cond")(true))(Emit.value("then"))(Emit.value("else"))
    val (told, _) = Emit.run[String](k).eval
    assertEquals(told.toList, List("cond", "then"))
  }

  private def leaf(latch: CountDownLatch, millis: Long): Boolean < (Abort[Nothing] & _root_.kyo.Async) =
    IO { latch.countDown(); latch.await(millis, TimeUnit.MILLISECONDS) }

  test("parApplicative: two leaves that must meet, meet — a rendezvous, not a clock") {
    val latch = CountDownLatch(2)
    val k = okay.traverse[Pending[Abort[Nothing] & _root_.kyo.Async], Int, Boolean](Seq(1, 2))(_ => leaf(latch, 10000))(using KyoClasses.parApplicative[Nothing])
    assertEquals(runAsync(k), Seq(true, true))
  }

  test("the monad's traverse over the same leaves cannot meet — the control") {
    val latch = CountDownLatch(2)
    val out = runAsync(okay.traverse[Pending[Abort[Nothing] & _root_.kyo.Async], Int, Boolean](Seq(1, 2))(_ => leaf(latch, 200)))
    note(s"sequential leaves answered $out")
    assertEquals(out.head, false)
  }
}

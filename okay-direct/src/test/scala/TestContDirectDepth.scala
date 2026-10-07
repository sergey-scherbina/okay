package okay


import okay.freer.*
import okay.freer.given
import okay.Direct.*
import scala.language.implicitConversions

/** cont-stack-layer1-c (4): a `Cps.shift` body that is a `direct` block over programs. The block is expanded
 * into binds before `shift`'s macro reads the body, so `k` is called inside a bind's lambda — later, from the
 * interpreter's loop — and never nests: a million levels on a 128 KB thread, and the answers of the plain form */
class TestContDirectDepth extends munit.FunSuite:
  type P = Pure
  val n = 1_000_000

  /** the body on a 128 KB thread (the core's SmallStack is in its own test tree, not visible here) */
  def onSmallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 128L * 1024)
    t.start()
    t.join()
    out.fold(e => throw e, identity)

  def row(levels: Int)(step: Int => Cps[Int, Int ! P, Int ! P]): Cps[Int, Int ! P, Int ! P] =
    (1 to levels).foldLeft(Cps.Pure[Int, Int ! P](0))((m, _) => m.flatMap(step))

  test("a million direct-block bodies over a program answer on 128 KB, forced by the Free fold") {
    val c = row(n)(x => Cps.shift[Int, Int ! P, Int ! P](k => direct { !k(x + 1) + 0 }))
    assertEquals(onSmallStack(!.run(Cps.run(c)((v: Int) => pure(v)))), n)
  }

  test("a direct-block body answers what the plain one does, multi-shot included") {
    val viaDirect = Cps.shift[Int, Int ! P, Int ! P](k => direct { !k(1) + !k(10) })
    val plain = Cps.shift[Int, Int ! P, Int ! P](k => k(1).flatMap(a => k(10).map(b => a + b)))
    assertEquals(!.run(Cps.run(viaDirect.map(_ * 2))((v: Int) => pure(v))), 22)
    assertEquals(!.run(Cps.run(plain.map(_ * 2))((v: Int) => pure(v))), 22)
  }

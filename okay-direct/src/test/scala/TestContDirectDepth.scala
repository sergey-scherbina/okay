package okay

import okay.Direct.*
import scala.language.implicitConversions

/** cont-stack-layer1-c (4): a `Cont.shift` body that is a `direct` block over programs. The block is expanded
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

  def row(levels: Int)(step: Int => Cont[Int, Int ! P, Int ! P]): Cont[Int, Int ! P, Int ! P] =
    (1 to levels).foldLeft(Cont.Pure[Int, Int ! P](0))((m, _) => m.flatMap(step))

  test("a million direct-block bodies over a program answer on 128 KB, forced by the Free fold") {
    val c = row(n)(x => Cont.shift[Int, Int ! P, Int ! P](k => direct { !k(x + 1) + 0 }))
    assertEquals(onSmallStack(!.run(Cont.run(c)((v: Int) => pure(v)))), n)
  }

  test("a direct-block body answers what the plain one does, multi-shot included") {
    val viaDirect = Cont.shift[Int, Int ! P, Int ! P](k => direct { !k(1) + !k(10) })
    val plain = Cont.shift[Int, Int ! P, Int ! P](k => k(1).flatMap(a => k(10).map(b => a + b)))
    assertEquals(!.run(Cont.run(viaDirect.map(_ * 2))((v: Int) => pure(v))), 22)
    assertEquals(!.run(Cont.run(plain.map(_ * 2))((v: Int) => pure(v))), 22)
  }

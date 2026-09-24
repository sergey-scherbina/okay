package okay2.cats

import java.util.concurrent.atomic.AtomicBoolean

import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import okay2._
import okay2.async.Async
import okay2.platform._
import CatsInterop.{fromIO, toIOBlocking}

/** the Async bridge (okay2-interop-async) */
class TestCatsAsync extends munit.FunSuite {

  test("toIOBlocking runs an Async program on the blocking pool; fromIO is an IO as an Async operation") {
    val p: Int ! Async = fromIO(IO.pure(20)).flatMap(a => Async(a + 22))
    assertEquals(toIOBlocking(p).unsafeRunSync(), 42)
    assertEquals(!.run(Async.run[Int, Pure](p)), 42)
  }

  test("fromIO is cancelled when the waiting side gives up") {
    val cancelled = new AtomicBoolean(false)
    val p = Async.timeout(50)(fromIO(IO.never[Unit].onCancel(IO(cancelled.set(true)))))
    assertEquals(!.run(Async.run[Option[Unit], Pure](p)), None)
    var i = 0
    while (!cancelled.get && i < 200) { Thread.sleep(10); i += 1 }
    assert(cancelled.get, "the IO kept running after the Async side timed out")
  }

  test("a failing IO fails the Async program with its error") {
    val p: Int ! Async = fromIO(IO.raiseError[Int](new IllegalStateException("io")))
    val e = intercept[IllegalStateException](!.run(Async.run[Int, Pure](p)))
    assertEquals(e.getMessage, "io")
  }

  test("scheduler: okay2's par on the cats-effect runtime") {
    implicit val S: okay2.async.Scheduler = CatsInterop.scheduler
    val p = Async.par(Async(1), Async(2))
    assertEquals(!.run(Async.run[(Int, Int), Pure](p)), (1, 2))
  }
}

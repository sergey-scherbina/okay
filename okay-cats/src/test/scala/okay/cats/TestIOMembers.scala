package okay.cats

import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import okay.{Async, asOkay, async, via}
import okay.freer.{%, +}
import okay.freer.perform
import okay.freer.{!, Handler}
import okay.std.{State}
import okay.freer.Row.bind
import okay.given
import okay.freer.given
import okay.std.given
import java.util.concurrent.atomic.AtomicInteger

/**
 * cats-effect's IO as an effect of the tree (specs/foreign-effects-in-tree.md, stage 1): the row member is `IO`
 * itself, and the runtime is chosen when the program is handled.
 */
class TestIOMembers extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  test("an IO kept in the tree: building runs nothing, toIO runs it") {
    val ran = AtomicInteger()
    val p: Int ! IO = IO { ran.incrementAndGet(); 20 }.perform.flatMap(a => IO(a + 1).perform)
    note(s"built, ran=${ran.get}")
    assertEquals(ran.get, 0)
    assertEquals(p.toIO.unsafeRunSync(), 21)
    assertEquals(ran.get, 1)
  }

  test("toIO: IO and okay's Async in one IO") {
    val p: Int ! (IO + Async) = IO(1).perform.bind(a => async(a + 1))
    assertEquals(p.toIO.unsafeRunSync(), 2)
  }

  test("via[IO]: each IO lowered to okay's Async, the rest of the row kept") {
    val p: Int ! (IO + State % Int) = IO(2).perform.bind(a => State.get[Int].map(_ + a))
    val q: Int ! (Async + State % Int) = p.via[IO]
    assertEquals(State.handle(10)(q).runWith, (10, 12))
  }

  test("a handler of our own answers the IO operations") {
    val p: Int ! IO = IO(1).perform.flatMap(a => IO(a + 1).perform)
    val seen = AtomicInteger()
    val sync = Handler.answer[IO].poly([X] => (io: IO[X]) => { seen.incrementAndGet(); io.unsafeRunSync() })
    assertEquals(p.handle(sync).run, 2)
    assertEquals(seen.get, 2)
  }

  test("asOkay is perform then via[IO]") {
    assertEquals(IO(5).asOkay.runWith, IO(5).perform.via[IO].runWith)
  }

  test("a thousand IO steps in one program") {
    val p: Int ! IO = (1 to 1000).foldLeft(IO(0).perform)((m, _) => m.flatMap(x => IO(x + 1).perform))
    assertEquals(p.toIO.unsafeRunSync(), 1000)
    assertEquals(p.via[IO].runWith, 1000)
  }
}

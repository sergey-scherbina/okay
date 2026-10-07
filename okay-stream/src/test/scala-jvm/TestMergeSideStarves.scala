package okay


import okay.freer.*
import okay.freer.given
/**
 * ready-merge-side-starves (okay-stream/BUGS.md): a ready merge of two
 * ENDLESS sides must keep delivering both. It did not, on every scheduler
 * with owned workers: the right side's feeder `offer`ed its first element,
 * the offer woke the parked merge, and the merge's drive resumed INLINE on
 * the feeder's thread (`DriveTask.resumeLate` from a managed worker). The
 * merge never parked again — its left side is always ready — so it held
 * the feeder's thread for good, the feeder underneath never ran, and a
 * fold waiting for the third `Right` waited for ever (rounds 16-27 of
 * 300, five runs in five, before the fix). Fixed in `DriveTask.resumeLate`:
 * a late answer on a thread running another fiber goes home, not inline.
 */
class TestMergeSideStarves extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

  private val thirdRight = FoldUntil[Either[Int, Int], Int, Int](0)((n, e) => if e.isRight then n + 1 else n)(_ >= 3)(identity)

  for (name, sch) <- List("own" -> Schedulers.own.build, "default" -> summon[Scheduler], "loom" -> Schedulers.loom) do
    test(s"$name: either of two endless sides delivers the other side, round after round") {
      given Scheduler = sch
      for r <- 1 to 300 do
        val m = Source.of(LazyList.from(0)).either(Source.of(LazyList.from(1000000)), capacity = 4)
        val f = sch.fork(() => m.runFoldUntil(using thirdRight))
        @volatile var out: Either[Throwable, Int] | Null = null
        val waiter = Thread.ofPlatform().daemon().start(() => out = f.joinEither())
        waiter.join(10_000)
        note(s"round $r")
        assert(out != null, s"$name round $r: the right side stopped arriving")
        assertEquals(out, Right(3), s"$name round $r")
    }
}

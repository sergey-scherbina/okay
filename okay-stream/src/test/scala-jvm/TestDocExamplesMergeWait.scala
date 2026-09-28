package okay

/** docs/merge-and-wait.md's examples, line for line (TestDocSnippets
 * pins each line of the page to a line here or in a library source) */
class TestDocExamplesMergeWait extends munit.FunSuite {

  test("merge-and-wait: the default mechanism and wait") {
    val merged = Source.of(List(1, 2, 3)) merge Source.of(List(10, 20))   // Merge.Ready, Wait.Ladder(100, 50, 4)
    val all = merged.runCollect.runWith
    assertEquals(all.sorted, Vector(1, 2, 3, 10, 20))
  }

  test("merge-and-wait: Merge.Shared, the chunked join") {
    given Merge = Merge.Shared
    val joined = Source.of(List(1, 2, 3)).merge(Source.of(List(10, 20)), chunked = true)
    assertEquals(joined.runCollect.runWith.sorted, Vector(1, 2, 3, 10, 20))
  }

  test("merge-and-wait: every strategy in the companion, and one of your own") {
    def run(using Wait): Vector[Int] =
      (Source.of(List(1, 2, 3)) merge Source.of(List(10, 20))).runCollect.runWith.sorted
    locally {
      given Wait = Wait.Register
      assertEquals(run, Vector(1, 2, 3, 10, 20))
    }
    locally {
      given Wait = Wait.Spin(1000)
      assertEquals(run, Vector(1, 2, 3, 10, 20))
    }
    locally {
      given Wait = Wait.Cycle(100, 50, 4)
      assertEquals(run, Vector(1, 2, 3, 10, 20))
    }
    locally {
      given Wait = new Wait:
        def until(ready: () => Boolean)(using p: Pause): Boolean =
          var i = 0
          var got = false
          while !got && i < 10 do { got = ready(); if !got then p.yieldNow(); i += 1 }
          if !got then p.block()
          got
      assertEquals(run, Vector(1, 2, 3, 10, 20))
    }
  }

  final class CountingPause extends Pause:
    var spins, yields, nanos, blocks = 0
    def threads = true
    def spin(): Unit = spins += 1
    def yieldNow(): Unit = yields += 1
    def nano(): Unit = nanos += 1
    def block(): Unit = blocks += 1

  test("merge-and-wait: a counting Pause shows the rungs the default ladder climbs") {
    val pause = CountingPause()
    val came = Wait.Ladder(100, 50, 4).until(() => false)(using pause)
    assert(!came)
    assertEquals((pause.spins, pause.yields, pause.nanos, pause.blocks), (100, 50, 4, 1))
  }
}

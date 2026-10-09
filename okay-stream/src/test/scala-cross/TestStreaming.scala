package okay

/** ONE suite, written once over the front (`Streaming[S]`), run on both backends: the front is transparent when
 * the same program answers the same on each (stream-twin) */
abstract class StreamingSuite[S[_]](backend: String)(using St: Streaming[S]) extends munit.FunSuite {
  import St.*

  given scala.concurrent.ExecutionContext = munitExecutionContext

  test(s"$backend: map, filter, take over a range") {
    runAsync(range(0, 100).map(_ * 2).filter(_ % 3 == 0).take(4).toVector)
      .map(v => assertEquals(v, Vector(0L, 6L, 12L, 18L)))
  }

  test(s"$backend: take does not run the producer to its end") {
    // a pure `map` may compute ahead within a chunk (a chunked backend computes its chunk at once); what `take`
    // guarantees is that the producer is not run past what it needed
    var made = 0
    val s = range(0, 100000).map { i => made += 1; i }
    runAsync(s.take(3).toVector).map { v =>
      assertEquals(v, Vector(0L, 1L, 2L))
      assert(made <= 1000, s"made $made")
    }
  }

  test(s"$backend: ++ and flatMap, in order") {
    runAsync((fromList(List(1, 2)) ++ emit(3)).flatMap(i => fromList(List(i, i * 10))).toVector)
      .map(v => assertEquals(v, Vector(1, 10, 2, 20, 3, 30)))
  }

  test(s"$backend: evalMap and foldLeft") {
    runAsync(fromList(List(1, 2, 3)).evalMap(i => async(i + 1)).foldLeft(0)(_ + _)).map(v => assertEquals(v, 9))
  }

  test(s"$backend: a hundred thousand elements, most filtered out, in constant stack") {
    runAsync(range(0, 100000).filter(_ % 10000 == 0).toVector).map(v => assertEquals(v.size, 10))
  }

  test(s"$backend: buffer keeps every element, in order, past a channel smaller than the stream") {
    runAsync(range(0, 1000).map(_ * 2).buffer(16).toVector).map(v => assertEquals(v, (0L until 1000L).map(_ * 2).toVector))
  }

  test(s"$backend: zip pairs in lockstep, as long as the shorter; zipWith") {
    runAsync(range(0, 5).zipWith(fromList(List("a", "b", "c")))((i, s) => s"$s$i").toVector)
      .map(v => assertEquals(v, Vector("a0", "b1", "c2")))
  }

  test(s"$backend: the sorted joins, duplicate keys and unmatched rows") {
    val l = fromList(List(1 -> "a", 2 -> "b", 2 -> "c", 4 -> "d"))
    val r = fromList(List(2 -> 20, 2 -> 21, 3 -> 30, 4 -> 40))
    for
      inner <- runAsync(l.joinSorted(r).toVector)
      left <- runAsync(l.leftJoinSorted(r).toVector)
      full <- runAsync(l.fullJoinSorted(r).toVector)
    yield
      assertEquals(inner, Vector(2 -> ("b", 20), 2 -> ("b", 21), 2 -> ("c", 20), 2 -> ("c", 21), 4 -> ("d", 40)))
      assertEquals(left.count(_._2._2.isEmpty), 1)
      assertEquals(full.size, 7)
  }

  test(s"$backend: tumbling windows count their elements") {
    val panes = range(0, 25).windowed(Windows.tumbling(10, 0)((_: Long) => "k")(identity)(okay.freer.Aggregator.count[Long]))
    runAsync(panes.toVector).map(v => assertEquals(v.map(_.value), Vector(10L, 10L, 5L)))
  }

  test(s"$backend: merge keeps every element of both and each side's order") {
    runAsync(range(0, 50).merge(range(100, 150)).toVector).map { v =>
      assertEquals(v.sorted, (0L until 50L).toVector ++ (100L until 150L).toVector)
      assertEquals(v.filter(_ < 100), (0L until 50L).toVector)
    }
  }
}

class TestStreamingMachine extends StreamingSuite[streams.machine.Flow]("machine")
class TestStreamingClassic extends StreamingSuite[streams.classic.Flow]("classic")

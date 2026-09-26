package okay

/**
 * A TOLD VALUE IS DELIVERED even when the program's continuation after
 * it throws (source-merge-via-ready, 2026-09-26). The stream views of
 * a writer — `Writer.uncons` and the iterators `feed` and every linear
 * consumer walk — applied the continuation BEFORE handing the told
 * value over, so a source whose next step throws while being BUILT
 * (`Source.of` over a stream whose `uncons` throws) lost the value it
 * had just told: `Channel.buffer` and the old `Source.merge` delivered
 * 1, 2 of a source that told 1, 2, 3 and then failed. The views of a
 * writer WITH effects are pinned in okay-stream (TestSourceToldBeforeThrow),
 * where Async is.
 */
class TestWriterToldBeforeThrow extends munit.FunSuite {

  /** tells 1, 2, 3; the continuation after 3 throws when APPLIED */
  private def told: Unit ! Writer % Int =
    Writer.tell(1).flatMap(_ => Writer.tell(2)).flatMap(_ => Writer.tell(3))
      .flatMap(_ => throw RuntimeException("after 3"))

  /** everything a view hands over before it throws */
  private def drain(next: () => Option[Int]): (List[Int], String) =
    var out = List.empty[Int]
    val err =
      try
        var more = true
        while more do next() match
          case Some(x) => out = x :: out
          case None => more = false
        "none"
      catch case e: Throwable => e.getMessage
    (out.reverse, err)

  test("the pure iterator hands over 3 before the throw") {
    val it = summon[Stream[[W] =>> Unit ! Writer % W, Pure]].iterator(told)
    assertEquals(drain(() => if it.hasNext then Some(it.next()) else None), (List(1, 2, 3), "after 3"))
  }

  test("Writer.uncons (pure) hands over 3 before the throw") {
    var cur: Unit ! Writer % Int = told
    assertEquals(drain(() => Writer.uncons(cur) match
      case Right((w, rest)) => { cur = rest; Some(w) }
      case Left(_) => None), (List(1, 2, 3), "after 3"))
  }

}

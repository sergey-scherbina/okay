package okay2

/**
 * A TOLD VALUE IS DELIVERED even when the continuation after it throws
 * — the Scala 3 core's TestWriterToldBeforeThrow, ported with the fix
 * (okay2-writer-told-then, 2026-09-26). The stream views applied the
 * continuation as they handed the value over, and a throw from it took
 * the value along: a source that told 1, 2, 3 and then failed came out
 * as 1, 2.
 */
class TestWriterToldBeforeThrow extends munit.FunSuite {

  private val boom: Unit => (Unit ! Writer[Int]) = _ => throw new RuntimeException("after 3")

  private def told: Unit ! Writer[Int] =
    Writer.tell(1).flatMap(_ => Writer.tell(2)).flatMap(_ => Writer.tell(3)).flatMap(boom)

  private type Row = Writer[Int] + Produce
  private def toldIn: Unit ! Row = told.at[Row]

  /** everything a view hands over before it throws */
  private def drain(next: () => Option[Int]): (List[Int], String) = {
    var out = List.empty[Int]
    val err =
      try {
        var more = true
        while (more) next() match {
          case Some(x) => out = x :: out
          case None => more = false
        }
        "none"
      } catch { case e: Throwable => e.getMessage }
    (out.reverse, err)
  }

  test("Writer.uncons hands over 3 before the throw") {
    var cur: Unit ! Writer[Int] = told
    assertEquals(drain(() => Writer.uncons(cur) match {
      case Right((w, rest)) => cur = rest; Some(w)
      case Left(_) => None
    }), (List(1, 2, 3), "after 3"))
  }

  test("Writer.unconsIn hands over 3 before the throw") {
    var cur: Unit ! Row = toldIn
    assertEquals(drain(() => Writer.unconsIn[Int, Unit, Produce](cur).runWith match {
      case Right((w, rest)) => cur = rest; Some(w)
      case Left(_) => None
    }), (List(1, 2, 3), "after 3"))
  }

  test("the pure iterator hands over 3 before the throw") {
    val it = Stream.feedStream[Unit].iterator(told)
    assertEquals(drain(() => if (it.hasNext) Some(it.next()) else None), (List(1, 2, 3), "after 3"))
  }

  test("the iterator over a writer with effects hands over 3 before the throw") {
    val it = Stream.writerStreamIn[Unit, Produce].iterator(toldIn)
    assertEquals(drain(() => if (it.hasNext) Some(it.next()) else None), (List(1, 2, 3), "after 3"))
  }
}

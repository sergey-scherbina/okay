package okay

/** core's TestWriterToldBeforeThrow, for a writer WITH effects — the
 * view `Channel.buffer`'s feed walks — and end to end through buffer */
class TestSourceToldBeforeThrow extends munit.FunSuite {

  private def told: Source[Int] =
    !.widen[Unit, Writer % Int, Async](
      Writer.tell(1).flatMap(_ => Writer.tell(2)).flatMap(_ => Writer.tell(3))
        .flatMap(_ => throw RuntimeException("after 3")))

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

  test("the iterator over a writer with effects hands over 3 before the throw") {
    val it = summon[Stream[[W] =>> Unit ! Writer % W + Async, Async]].iterator(told)
    assertEquals(drain(() => if it.hasNext then Some(it.next()) else None), (List(1, 2, 3), "after 3"))
  }

  test("Writer.uncons (with effects) hands over 3 before the throw") {
    var cur: Source[Int] = told
    assertEquals(drain(() => Writer.uncons[Int, Unit, Async](cur).runWith match
      case Right((w, rest)) => { cur = rest; Some(w) }
      case Left(_) => None), (List(1, 2, 3), "after 3"))
  }

  test("Channel.buffer of such a source delivers 1, 2, 3 and then the failure") {
    var got = Vector.empty[Int]
    val err = try { Channel.buffer(8)(told).drained.runForeach(x => okay.async { got :+= x }).runWith; "none" }
              catch case e: Throwable => e.getMessage
    assertEquals((got, err), (Vector(1, 2, 3), "after 3"))
  }
}

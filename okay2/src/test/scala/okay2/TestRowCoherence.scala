package okay2

/**
 * row-coercion-coherence-law, the okay2 twin. Here a wider row is a
 * SUBTYPE: `Free` is contravariant in its row and `+` is `with`, so
 * `!.widen` returns the program itself and okay2 has no walk beside it.
 * With one road there is nothing for two roads to disagree on, and the
 * law is that the widened program IS the program and runs as it did.
 * okay2's real incoherence (an intersection's `#Op` is the LAST
 * parent's, memory okay2-intersection-row) was in reading an operation's
 * type, not in widening, and its guard is Split's typed views.
 */
class TestRowCoherence extends munit.FunSuite {

  val counter: Int ! State[Int] =
    for {
      s <- State.get[Int]
      _ <- State.set(s + 1)
      t <- State.get[Int]
    } yield t

  test("widen is the program itself, and the widened program runs as the program did") {
    val w = !.widen[Int, State[Int], Reader[Boolean]](counter)
    assert(w eq counter)
    val wide: (Int, Int) ! Reader[Boolean] = State.handle[Int, Int, Reader[Boolean]](1)(w)
    assertEquals(!.run(Reader.run(true)(wide)), State.run[Int, Int](1)(counter))
  }
}

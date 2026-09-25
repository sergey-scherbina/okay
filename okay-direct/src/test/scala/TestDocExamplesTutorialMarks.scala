package okay

import okay.Direct.*

/**
 * The docs/tutorial.md examples that mark with the glyph, verbatim
 * (mark-glyph-only: rewriting them from `.!?` made them new lines, and
 * a new example line is pinned — the snippet-debt ratchet).
 */
class TestDocExamplesTutorialMarks extends munit.FunSuite:

  def look(i: Int): Int ! (Writer % String) = Writer.tell(s"look $i").flatMap(_ => pure(i * 10))

  test("tutorial: the whole for, and the HOFs a mark lands in") {
    val prog: List[Int] ! (Writer % String) = direct {
      for
        x <- List(1, 2)
        y <- List(10, 20) if y > 10        // a guard between generators
      yield look(x + y).?                 // List(210, 220); log: look 21, look 22
    }
    assertEquals(!.run(Writer.run(prog)), (Seq("look 21", "look 22"), List(210, 220)))

    val m: Option[Map[String, Int]] =
      direct[Option] { for (k, n) <- Map("a" -> 1) yield (k, Some(n * 10).?) }   // Some(Map(a -> 10))
    assertEquals(m, Some(Map("a" -> 10)))

    val e: Boolean ! (Writer % String) =
      direct { List(1, 2, 3, 4).exists(x => look(x).? > 15) }   // true, and the log stops at "look 2"
    assertEquals(!.run(Writer.run(e)), (Seq("look 1", "look 2"), true))

    val f: Int ! (Writer % String) =
      direct { List(1, 2, 3).foldLeft(0)((acc, x) => acc + look(x).?) }          // 60
    assertEquals(!.run(Writer.run(f)), (Seq("look 1", "look 2", "look 3"), 60))
  }

  test("tutorial: a staged block, compound programs walked too") {
    val sw = Stager.StateWriter[Int, String, Int]()      // the row's staged interpreter

    def step(i: Int, acc: Int): Handled[sw.Row, sw.R, Int] =
      if i >= 100 then Handled.pure(acc)
      else Direct.staged(sw) {
        val a = State.get[Int].?
        val _ = State.modify[Int](_ + i).?  // compound programs are walked too
        Writer.tell("w").?
        step(i + 1, acc + a).?
      }

    val ((state, log), answer) = sw.run(0)(step(0, 0))
    // state is 0+1+…+99; each step reads the state BEFORE its own modify
    assertEquals(state, (0 until 100).sum)
    assertEquals(log, Vector.fill(100)("w"))
    assertEquals(answer, (0 until 100).map(i => (0 until i).sum).sum)
  }

  test("tutorial: fib as a generator block") {
    val fib: Gen[Long] = generator[Long] {        // or a block: while/if/recursion, emit, stop
      var (a, b) = (0L, 1L)
      while true do { Gen.emit(a).?; val t = a; a = b; b = t + b }
    }
    assertEquals(fib.iterator.drop(10).next(), 55L)
  }

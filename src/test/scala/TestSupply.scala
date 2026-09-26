package okay

import okay.Row.{at, plus}

/** specs/core-gaps.md, Supply */
class TestSupply extends munit.FunSuite {
  test("Fresh.run draws 0, 1, 2 in program order") {
    val three = for a <- Fresh.next; b <- Fresh.next; c <- Fresh.next yield List(a, b, c)
    assertEquals(!.run(Fresh.run(three)), List(0L, 1L, 2L))
  }

  test("run answers the final seed with the value") {
    val names = for a <- Supply.next[String]; b <- Supply.next[String] yield s"$a,$b"
    assertEquals(!.run(Supply.run("x")(_ + "'")(names)), ("x''", "x,x'"))
  }

  test("another effect keeps its place between draws") {
    type R = Fresh + Writer % String
    val p: Long ! R =
      for
        a <- Fresh.next.plus[Writer % String]
        _ <- Writer.tell(s"got $a").at[R]
        b <- Fresh.next.plus[Writer % String]
        _ <- Writer.tell(s"got $b").at[R]
      yield a + b
    assertEquals(!.run(Writer.run[String, Long, Pure](Fresh.run(p))), (List("got 0", "got 1"), 1L))
  }

  test("under Choose each branch continues from the seed it was captured with") {
    type R = Fresh + Choose
    val p: (String, Long, Long) ! R =
      for
        a <- Fresh.next.plus[Choose]
        s <- choose("l", "r").at[R]
        b <- Fresh.next.plus[Choose]
      yield (s, a, b)
    assertEquals(!.run(runChoice(Fresh.run(p))), Seq(("l", 0L, 1L), ("r", 0L, 1L)))
  }

  test("stack-safe over 100 000 draws") {
    val n = 100000
    val p = (1 to n).foldLeft(pure[Fresh, Long](0L))((acc, _) => acc.flatMap(s => Fresh.next.map(s + _)))
    assertEquals(!.run(Fresh.run(p)), n.toLong * (n - 1) / 2)
  }
}

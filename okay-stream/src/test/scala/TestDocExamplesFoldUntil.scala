package okay

/**
 * The examples docs/tutorial.md §2, docs/guide.md §2–3 and the
 * typepedia print for `FoldUntil` and `!.loop`, VERBATIM — a doc
 * snippet that is not compiled drifts (the-record-outlives-the-truth),
 * so every one here is what the page shows, with the answer the page
 * claims.
 */
class TestDocExamplesFoldUntil extends munit.FunSuite:

  def countdown(n: Int): Unit ! Writer % Int =
    if n == 0 then pure(())
    else Writer.tell(n).flatMap(_ => countdown(n - 1))

  test("tutorial §2: a fold that stops, over the million-step countdown") {
    val found = countdown(1000000).foldUntil(using FoldUntil.find[Int](_ % 7 == 0))
    assertEquals(found, Some(999999))
  }

  test("tutorial §2: !.loop — the digits of a number, least significant first") {
    val digits: Int ! Writer % Int = !.loop(2024) { n =>
      Writer.tell(n % 10).map(_ => if n < 10 then Right(1) else Left(n / 10))
    }
    assertEquals(!.run(Writer.run(digits)), (Seq(4, 2, 0, 2), 1))
  }

  test("guide §2: !.loop — Collatz steps, the state carrying the count") {
    val steps: Int ! Nothing = !.loop[(Int, Int), Int, Nothing]((27, 0)) { (n, k) =>
      pure(if n == 1 then Right(k) else Left((if n % 2 == 0 then n / 2 else 3 * n + 1, k + 1)))
    }
    assertEquals(!.run(steps), 111)
  }

  test("guide §3: the first three of a million, and a running sum that stops itself") {
    assertEquals(Source.range(0, 1000000).runFoldUntil(using FoldUntil.take[Long](3)).runWith, Vector(0L, 1L, 2L))
    val sum = Chunks.foldUntil(Chunks.nats[Int]())(using
      FoldUntil.until[Int, Int, Int](0)((s, a) => if s + a > 100 then Right(s) else Left(s + a))(identity))
    assertEquals(sum, 91)
  }

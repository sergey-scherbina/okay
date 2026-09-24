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
    val answer =
      !.run(steps)   // 111 — the Collatz steps from 27, the count carried in the state
    assertEquals(answer, 111)
  }

  test("guide §3: the first three of a million, and a running sum that stops itself") {
    val firstThree =
      Source.range(0, 1000000).runFoldUntil(using FoldUntil.take[Long](3))   // Vector(0, 1, 2) ! Async
    assertEquals(firstThree.runWith, Vector(0L, 1L, 2L))
    val sum =
      Chunks.foldUntil(Chunks.nats[Int]())(using
        FoldUntil.until[Int, Int, Int](0)((s, a) => if s + a > 100 then Right(s) else Left(s + a))(identity))
    assertEquals(sum, 91)
  }

  test("tutorial §2: the same fold as an iteratee, and on a plain List") {
    assertEquals(pipe(countdown(1000000))(Take.foldUntil(using FoldUntil.find[Int](_ % 7 == 0))), Some(999999))
    assertEquals(List(3, 1, 4, 1, 5).foldUntilTo(using FoldUntil.find[Int](_ > 3)), Some(4))
  }

  test("guide §5: a header parser that stops the upstream at the blank line") {
    val header: Stage[String, (String, String), Either[Int, Int]] =
      Stage.transduceUntil[String, (String, String), Int, Either[Int, Int]](0)((n, line) =>
        if line.isEmpty then pure(Right(Right(n)))          // the blank line: stop, n fields read
        else
          val Array(k, v) = line.split(": ", 2)
          Stage.tell[String, (String, String)]((k, v)).map(_ => Left(n + 1)),
        n => Left(n))                                         // the input ended first

    def lines(xs: String*): Unit ! Writer % String =
      xs.foldRight(pure[Writer % String, Unit](()))((l, rest) => Writer.tell(l).flatMap(_ => rest))

    assertEquals(!.run(
      Writer.run(through(lines("host: a", "port: 1", "", "body"))(header))   // (Seq((host,a), (port,1)), Right(2))
    ), (Seq(("host", "a"), ("port", "1")), Right(2)))
    assertEquals(!.run(
      Writer.run(through(lines("host: a"))(header))                          // (Seq((host,a)), Left(1))
    ), (Seq(("host", "a")), Left(1)))
  }

  test("guide §4: Staged — the whole pipeline one while-loop") {
    val total =
      Staged.fold(
        Staged.take(
          Staged.filter(Staged.map(Staged.range(0, 1000000), _ * 2), _ % 3 == 0),
          1000))(0L)(_ + _)
    assertEquals(total, 2997000L)   // 6 * (0 + 1 + … + 999)
  }

package okay2

/** Nondeterminism: the multi-shot handler, and the search on top. */
class TestChoice extends munit.FunSuite {

  test("Choice: multi-shot handler explores every branch (cartesian)") {
    val prog: Int ! Choose =
      choose(1, 2, 3).flatMap(x => choose(10, 20).map(x * _))
    assertEquals(!.run(runChoice(prog)), Seq(10, 20, 20, 40, 30, 60))
  }

  test("Choice: empty choice prunes the branch; guard is the same thing spelled as a step") {
    val prog: Int ! Choose =
      choose(1, 2).flatMap(x => if (x == 1) choose[Int]() else choose(x))
    assertEquals(!.run(runChoice(prog)), Seq(2))
    val guarded: Int ! Choose = for {
      x <- choose(1, 2, 3)
      _ <- Choose.guard(x % 2 == 1)
    } yield x * 10
    assertEquals(!.run(runChoice(guarded)), Seq(10, 30))
  }

  test("pythagorean triples: guard prunes, searched by Choose") {
    val triples: (Int, Int, Int) ! Choose =
      choose((1 to 20): _*)
        .flatMap(a => choose((a to 20): _*)
          .flatMap(b => choose((b to 20): _*)
            .flatMap(c => Choose.guard(a * a + b * b == c * c)
              .map(_ => (a, b, c)))))
    assertEquals(!.run(runChoice(triples)).take(3),
      Seq((3, 4, 5), (5, 12, 13), (6, 8, 10)))
  }

  test("effects forward through the search: a Writer on the crossed path, the row anywhere") {
    type F = Writer[String]
    val prog: Int ! (Choose + F) =
      choose(1, 2).at[Choose + F].flatMap(x =>
        Writer.tell(s"seen $x").at[Choose + F].map(_ => x * 10))
    val (told, found) = !.run(Writer.run(runChoice(prog)))
    assertEquals(found, Seq(10, 20))
    assertEquals(told, Seq("seen 1", "seen 2"))
    // Choose on the right: Remove finds it
    val flipped: Int ! (F + Choose) = prog.at[F + Choose]
    assertEquals(!.run(Writer.run(runChoice(flipped)))._2, Seq(10, 20))
  }
}

/** Backtracking: msplit and everything that derives from it. */
class TestLogic extends munit.FunSuite {
  import Logic._

  type Row = Choose + Pure
  type P[A] = A ! Row

  def amb[A](as: A*): P[A] = choose(as: _*).plus[Pure]
  def fail[A]: P[A] = Choose.fail[A].plus[Pure]
  def nats: P[Long] = Free.inject[Choose, Long](Choose.Op(LazyList.from(0).map(_.toLong))).plus[Pure]

  test("msplit: the first answer and the rest as a program") {
    val Some((a, rest)) = !.run(msplit[Int, Pure](amb(1, 2, 3))): @unchecked
    assertEquals(a, 1)
    assertEquals(!.run(runChoice(rest)), Seq(2, 3))
    assertEquals(!.run(msplit[Int, Pure](fail[Int])), None)
  }

  test("cut commits: one answer, the rest of the search discarded") {
    assertEquals(!.run(runChoice(cut[Int, Pure](amb(1, 2, 3)))), Seq(1))
    assertEquals(!.run(runChoice(cut[Int, Pure](fail[Int]))), Seq.empty)
  }

  test("ifte is the soft cut: else runs ONLY when the condition has no answer") {
    val hit = ifte[Int, Int, Pure](amb(1, 2))(x => pure(x * 10))(pure(-1))
    assertEquals(!.run(runChoice(hit)), Seq(10, 20))
    val miss = ifte[Int, Int, Pure](fail[Int])(x => pure(x))(pure(-1))
    assertEquals(!.run(runChoice(miss)), Seq(-1))
  }

  test("gnot: negation as failure") {
    assertEquals(!.run(runChoice(gnot[Int, Pure](fail[Int]))), Seq(()))
    assertEquals(!.run(runChoice(gnot[Int, Pure](amb(1)))), Seq.empty)
  }

  test("interleave is fair: an infinite branch cannot starve the other") {
    val evens = nats.map(_ * 2)
    val odds = nats.map(_ * 2 + 1)
    val six = !.run(observe[Long, Pure](6)(interleave(evens, odds)))
    assertEquals(six, Seq(0L, 1L, 2L, 3L, 4L, 5L))
  }

  test("fair bind finds a witness under an infinite generator") {
    val prog = fairBind(nats)(x => if (x * x == 16) pure[Row, Long](x) else fail[Long])
    assertEquals(!.run(observe[Long, Pure](1)(prog)), Seq(4L))
  }

  test("observe takes n answers from an infinite search, lazily") {
    assertEquals(!.run(observe[Long, Pure](5)(nats)), Seq(0L, 1L, 2L, 3L, 4L))
  }
}

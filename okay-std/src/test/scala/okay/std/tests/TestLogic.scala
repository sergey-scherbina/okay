package okay.std.tests

import okay.*
import okay.std.*

import okay.std.given


import Logic.*

/** Backtracking: msplit and everything that derives from it. */
class TestLogic extends munit.FunSuite {

  type Row = Choose
  type P[A] = A ! Row

  def amb[A](as: A*): P[A] = effect(Choose(as))

  def fail[A]: P[A] = effect(Choose(Seq.empty))

  def nats: P[Long] = effect(Choose(LazyList.from(0).map(_.toLong)))

  test("guard prunes: pythagorean triples by MonadPlus, searched by Choose") {
    val triples: (Int, Int, Int) ! Choose =
      choose((1 to 20)*)
        .flatMap(a => choose((a to 20)*)
          .flatMap(b => choose((b to 20)*)
            .flatMap(c => guard[[A] =>> A ! Choose](a * a + b * b == c * c)
              .map(_ => (a, b, c)))))
    assertEquals(runChoice(triples).run.take(3),
      Seq((3, 4, 5), (5, 12, 13), (6, 8, 10)))
  }

  test("msplit: the first answer and the rest as a program") {
    val Some((a, rest)) = msplit[Int, Choose](amb(1, 2, 3)).run: @unchecked
    assertEquals(a, 1)
    assertEquals(runChoice(rest).run, Seq(2, 3))
    assertEquals(msplit[Int, Choose](fail[Int]).run, None)
  }

  test("cut commits: one answer, the rest of the search discarded") {
    assertEquals(runChoice(Logic.cut[Int, Choose](amb(1, 2, 3))).run, Seq(1))
    assertEquals(runChoice(Logic.cut[Int, Choose](fail[Int])).run, Seq.empty)
  }

  test("ifte is the soft cut: else runs ONLY when the condition has no answer") {
    // condition succeeds -> then over ALL its answers, else never
    val hit = ifte[Int, Int, Choose](amb(1, 2))(x => pure(x * 10))(pure(-1))
    assertEquals(runChoice(hit).run, Seq(10, 20))
    // condition fails -> else
    val miss = ifte[Int, Int, Choose](fail[Int])(x => pure(x))(pure(-1))
    assertEquals(runChoice(miss).run, Seq(-1))
  }

  test("gnot: negation as failure") {
    assertEquals(runChoice(gnot[Int, Choose](fail[Int])).run, Seq(()))
    assertEquals(runChoice(gnot[Int, Choose](amb(1))).run, Seq.empty)
  }

  test("interleave is fair: an infinite branch cannot starve the other") {
    val evens = nats.map(_ * 2)
    val odds = nats.map(_ * 2 + 1)
    val six = observe[Long, Choose](6)(interleave(evens, odds)).run
    assertEquals(six, Seq(0L, 1L, 2L, 3L, 4L, 5L))   // strict turn-taking
  }

  test("fair bind finds a witness under an infinite generator") {
    // unfair flatMap would dive into the first candidate's branch
    // forever; >>- gives every candidate a turn
    val prog = fairBind(nats)(x => if x * x == 16 then pure(x) else fail[Long])
    assertEquals(observe[Long, Choose](1)(prog).run, Seq(4L))
  }

  test("observe takes n answers from an infinite search, lazily") {
    assertEquals(observe[Long, Choose](5)(nats).run, Seq(0L, 1L, 2L, 3L, 4L))
  }

  test("effects forward through the search: a Writer on the crossed path") {
    type F = Writer % String
    val prog: Int ! Choose + F =
      effect[Choose, Int](Choose(Seq(1, 2))).at[Choose + F].flatMap(x =>
        effect[F, Unit](Writer(s"seen $x")).at[Choose + F].map(_ => x * 10))
    val (told, found) = (Writer.run(
      runChoice(prog))).run
    assertEquals(found, Seq(10, 20))
    assertEquals(told, Seq("seen 1", "seen 2"))
  }

  test("a refutable pattern binds where the row can fail, and prunes") {
    // `case Some(n) <-` desugars to withFilter, which exists exactly
    // under MonadPlus: the branch that does not match dies
    val kept: Int ! Choose =
      for case Some(n) <- choose(Some(1), None, Some(3)) yield n
    assertEquals(runChoice(kept).run, Seq(1, 3))

    // an `if` guard goes through the same method — guard, spelled the
    // way a for-comprehension spells it
    val guarded: Int ! Choose =
      for
        case Some(n) <- choose(Some(1), None, Some(3))
        if n > 1
      yield n * 10
    assertEquals(runChoice(guarded).run, Seq(30))
  }
}

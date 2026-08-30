package okay

import !.*
import Logic.*

/** Backtracking: msplit and everything that derives from it. */
class TestLogic extends munit.FunSuite {

  type Row = Choose + okay.Pure
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
    assertEquals(!.run(runChoice[(Int, Int, Int), okay.Pure](triples)).take(3),
      Seq((3, 4, 5), (5, 12, 13), (6, 8, 10)))
  }

  test("msplit: the first answer and the rest as a program") {
    val Some((a, rest)) = !.run(msplit[Int, okay.Pure](amb(1, 2, 3))): @unchecked
    assertEquals(a, 1)
    assertEquals(!.run(runChoice[Int, okay.Pure](rest)), Seq(2, 3))
    assertEquals(!.run(msplit[Int, okay.Pure](fail[Int])), None)
  }

  test("once commits: one answer, the rest of the search discarded") {
    assertEquals(!.run(runChoice[Int, okay.Pure](once[Int, okay.Pure](amb(1, 2, 3)))), Seq(1))
    assertEquals(!.run(runChoice[Int, okay.Pure](once[Int, okay.Pure](fail[Int]))), Seq.empty)
  }

  test("ifte is the soft cut: else runs ONLY when the condition has no answer") {
    // condition succeeds -> then over ALL its answers, else never
    val hit = ifte[Int, Int, okay.Pure](amb(1, 2))(x => pure(x * 10))(pure(-1))
    assertEquals(!.run(runChoice[Int, okay.Pure](hit)), Seq(10, 20))
    // condition fails -> else
    val miss = ifte[Int, Int, okay.Pure](fail[Int])(x => pure(x))(pure(-1))
    assertEquals(!.run(runChoice[Int, okay.Pure](miss)), Seq(-1))
  }

  test("gnot: negation as failure") {
    assertEquals(!.run(runChoice[Unit, okay.Pure](gnot[Int, okay.Pure](fail[Int]))), Seq(()))
    assertEquals(!.run(runChoice[Unit, okay.Pure](gnot[Int, okay.Pure](amb(1)))), Seq.empty)
  }

  test("interleave is fair: an infinite branch cannot starve the other") {
    val evens = nats.map(_ * 2)
    val odds = nats.map(_ * 2 + 1)
    val six = !.run(observe[Long, okay.Pure](6)(interleave(evens, odds)))
    assertEquals(six, Seq(0L, 1L, 2L, 3L, 4L, 5L))   // strict turn-taking
  }

  test("fair bind finds a witness under an infinite generator") {
    // unfair flatMap would dive into the first candidate's branch
    // forever; >>- gives every candidate a turn
    val prog = fairBind(nats)(x => if x * x == 16 then pure(x) else fail[Long])
    assertEquals(!.run(observe[Long, okay.Pure](1)(prog)), Seq(4L))
  }

  test("observe takes n answers from an infinite search, lazily") {
    assertEquals(!.run(observe[Long, okay.Pure](5)(nats)), Seq(0L, 1L, 2L, 3L, 4L))
  }

  test("effects forward through the search: a Writer on the crossed path") {
    type F = Writer % String
    val prog: Int ! (Choose + F) =
      effect[Choose + F, Int](Choose(Seq(1, 2))).flatMap(x =>
        effect[Choose + F, String](Writer(s"seen $x")).map(_ => x * 10))
    val (told, found) = !.run(Writer.run[String, Seq[Int], okay.Pure](
      runChoice[Int, F](prog)))
    assertEquals(found, Seq(10, 20))
    assertEquals(told, Seq("seen 1", "seen 2"))
  }
}

package scala2probe

import okay.scala2._

/** nondeterminism from Scala 2.13 (specs/scala2-facade.md, stage 13) */
class TestChooseFromScala2 extends munit.FunSuite {

  // the naturals from n, as an infinite search
  def nats(n: Int): Eff[Choose, Int] = Choose.from(true, false).flatMap(stop => if (stop) Eff.pure(n) else nats(n + 1))

  test("every answer: the Pythagorean triples up to 13") {
    val triples = for {
      a <- Choose.from(1 to 13: _*)
      b <- Choose.from(a to 13: _*)
      c <- Choose.from(b to 13: _*)
      _ <- Choose.guard(a * a + b * b == c * c)
    } yield (a, b, c)
    assertEquals(Eff.run(Choose.all(triples)), Seq((3, 4, 5), (5, 12, 13), (6, 8, 10)))
  }

  test("first(n) stops the search, so it may be infinite; cut commits to the first answer") {
    assertEquals(Eff.run(Choose.first(3)(nats(0))), Seq(0, 1, 2))
    assertEquals(Eff.run(Choose.all(Choose.cut(nats(7)))), Seq(7))
  }

  test("ifte: every answer of the condition, or the else branch only when there is none") {
    val some = Choose.ifte(Choose.from(1, 2))(n => Eff.pure(n * 10))(Eff.pure(-1))
    val none = Choose.ifte(Choose.fail[Int])(n => Eff.pure(n * 10))(Eff.pure(-1))
    assertEquals(Eff.run(Choose.all(some)), Seq(10, 20))
    assertEquals(Eff.run(Choose.all(none)), Seq(-1))
  }

  test("fairness: an infinite branch does not starve the other one") {
    val fair = Choose.interleave(nats(0), Choose.from(100, 200))
    val got = Eff.run(Choose.first(6)(fair))
    assert(got.contains(100) && got.contains(200), got.toString)
    val bound = Choose.fairBind(Choose.from(0, 1000))(start => nats(start))
    assert(Eff.run(Choose.first(6)(bound)).contains(1000))
  }

  test("another effect in the rest of the row passes through the fair search, in order") {
    val counted: Eff[Choose with Writer[String], Int] =
      Choose.interleave(Writer.tell("a").flatMap(_ => Choose.from(1, 2)), Writer.tell("b").map(_ => 3))
    val (log, answers) = Eff.run(Writer.run(Choose.all(counted)))
    assertEquals(answers.toSet, Set(1, 2, 3))
    assertEquals(log.toList, List("a", "b"))
  }

  test("State inside the search: each branch its own state; outside: one shared") {
    val prog: Eff[Choose with State[Int], Int] = for {
      x <- Choose.from(1, 2)
      _ <- State.modify[Int](_ + x)
      s <- State.get[Int]
    } yield s
    assertEquals(Eff.run(Choose.all(State.run(0)(prog))), Seq((1, 1), (2, 2)))
    assertEquals(Eff.run(State.run(0)(Choose.all(prog))), (3, Seq(1, 3)))
  }

  test("Search.bestOf stops at the first good sample; all and majority") {
    var calls = 0
    val gen: Eff[Async, Int] = Async.delay { calls += 1; calls }
    assertEquals(Eff.runAsync(Search.bestOf(5)(gen)(_ == 2)), Some(2))
    assertEquals(calls, 2)
    assertEquals(Eff.runAsync(Search.bestOf(3)(Async.delay(0))(_ > 0)), None)
    assertEquals(Eff.runAsync(Search.all(4)(Async.delay("x"))(_ => true)), Seq("x", "x", "x", "x"))
    assertEquals(Search.majority(Seq("a", "b", "a")), Some("a"))
  }
}

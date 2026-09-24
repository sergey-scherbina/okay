package scala2probe

import okay.scala2._

/** nondeterminism from Scala 2.13 (specs/scala2-facade.md, stage 13) */
class TestChooseFromScala2 extends munit.FunSuite {

  // the naturals from n, as an infinite search
  def nats(n: Int): Int ! Choose = Choose.choose(true, false).flatMap(stop => if (stop) pure(n) else nats(n + 1))

  test("every answer: the Pythagorean triples up to 13") {
    val triples = for {
      a <- Choose.choose(1 to 13: _*)
      b <- Choose.choose(a to 13: _*)
      c <- Choose.choose(b to 13: _*)
      _ <- Choose.guard(a * a + b * b == c * c)
    } yield (a, b, c)
    assertEquals(!.run(Choose.runChoice(triples)), Seq((3, 4, 5), (5, 12, 13), (6, 8, 10)))
  }

  test("first(n) stops the search, so it may be infinite; cut commits to the first answer") {
    assertEquals(!.run(Logic.observe(3)(nats(0))), Seq(0, 1, 2))
    assertEquals(!.run(Choose.runChoice(Logic.cut(nats(7)))), Seq(7))
  }

  test("ifte: every answer of the condition, or the else branch only when there is none") {
    val some = Logic.ifte(Choose.choose(1, 2))(n => pure(n * 10))(pure(-1))
    val none = Logic.ifte(Choose.fail[Int])(n => pure(n * 10))(pure(-1))
    assertEquals(!.run(Choose.runChoice(some)), Seq(10, 20))
    assertEquals(!.run(Choose.runChoice(none)), Seq(-1))
  }

  test("fairness: an infinite branch does not starve the other one") {
    val fair = Logic.interleave(nats(0), Choose.choose(100, 200))
    val got = !.run(Logic.observe(6)(fair))
    assert(got.contains(100) && got.contains(200), got.toString)
    val bound = Logic.fairBind(Choose.choose(0, 1000))(start => nats(start))
    assert(!.run(Logic.observe(6)(bound)).contains(1000))
  }

  test("another effect in the rest of the row passes through the fair search, in order") {
    val counted: Int ! (Choose + Writer[String]) =
      Logic.interleave(Writer.tell("a").flatMap(_ => Choose.choose(1, 2)), Writer.tell("b").map(_ => 3))
    val (log, answers) = !.run(Writer.run(Choose.runChoice(counted)))
    assertEquals(answers.toSet, Set(1, 2, 3))
    assertEquals(log.toList, List("a", "b"))
  }

  test("State inside the search: each branch its own state; outside: one shared") {
    val prog: Int ! (Choose + State[Int]) = for {
      x <- Choose.choose(1, 2)
      _ <- State.modify[Int](_ + x)
      s <- State.get[Int]
    } yield s
    assertEquals(!.run(Choose.runChoice(State.handle(0)(prog))), Seq((1, 1), (2, 2)))
    assertEquals(!.run(State.handle(0)(Choose.runChoice(prog))), (3, Seq(1, 3)))
  }

  test("Search.bestOf stops at the first good sample; all and majority") {
    var calls = 0
    val gen: Int ! Async = Async { calls += 1; calls }
    assertEquals(Search.bestOf(5)(gen)(_ == 2).runWith, Some(2))
    assertEquals(calls, 2)
    assertEquals(Search.bestOf(3)(Async(0))(_ > 0).runWith, None)
    assertEquals(Search.all(4)(Async("x"))(_ => true).runWith, Seq("x", "x", "x", "x"))
    assertEquals(Search.majority(Seq("a", "b", "a")), Some("a"))
  }
}

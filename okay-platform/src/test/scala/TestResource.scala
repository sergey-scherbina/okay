package okay

import scala.language.implicitConversions




import okay.std.*

/** The resource region: releases at the end of the scope, no matter what. */
class TestResource extends munit.FunSuite {

  test("releases in reverse acquisition order at the end of the scope") {
    var log = List.empty[String]
    def res(n: String) = Resource.acquire { log ::= s"open $n"; n } (r => log ::= s"close $r")
    val prog = res("a").flatMap(a => res("b").map(b => a + b))
    assertEquals((Resource.run(prog)).run, "ab")
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }

  test("an abort handled inside the scope still releases") {
    var released = false
    type F = Throws % String + Resource
    val prog: Int ! F =
      effect(Resource.Acquire(() => (), _ => released = true)).flatMap: _ =>
        effect(Throws("boom"))
    val either = Resource.run(runEither(prog)).run
    assertEquals(either, Left("boom"))
    assertEquals(released, true)
  }

  test("a JVM exception during a step still releases") {
    var released = false
    val prog: Int ! Resource = Resource.acquire(())(_ => released = true)
      .flatMap(_ => pure[Int, Resource](0).map(_ => throw RuntimeException("boom")))
    intercept[RuntimeException]((Resource.run(prog)).run): Unit
    assertEquals(released, true)
  }

  test("forwarded effects: finalizers travel with the residual") {
    var released = false
    type F = Resource + Produce
    val prog: Int ! F =
      effect(Resource.Acquire(() => (), _ => released = true)).flatMap: _ =>
        produce(41).map(_ + 1)
    val residual: Int ! Produce = Resource.run(prog)
    assertEquals(released, false)
    assertEquals(residual.runWith, 42)
    assertEquals(released, true)
  }

  test("a throw in the continuation AFTER a forwarded effect still releases") {
    // the leak sql-seam found: the residual applies k(y) at the OUTER
    // handler's call site, outside the region's own try — user code
    // composed after a forwarded effect (a .map that throws) must not
    // skip the finalizers
    var released = false
    type F = Resource + Produce
    val prog: Int ! F =
      effect(Resource.Acquire(() => (), _ => released = true)).flatMap: _ =>
        produce(41).map(_ => throw RuntimeException("boom"))
    val residual: Int ! Produce = Resource.run(prog)
    assertEquals(released, false)
    intercept[RuntimeException](residual.runWith): Unit
    assertEquals(released, true)
  }

  test("bracketNow over any Answers-able row, not only Async") {
    var released = 0
    assertEquals(bracketNow(41)(_ => released += 1)(r => async(r + 1)).runWith, 42)
    assertEquals(bracketNow(1)(_ => released += 1)(r => produce(r + 1)).runWith, 2)
    val _ = intercept[RuntimeException]:
      bracketNow(0)(_ => released += 1)(_ => async[Int](throw RuntimeException("boom"))).runWith
    assertEquals(released, 3)
  }

  test("bracket FORWARDS the use-program's effects, and releases after them") {
    var log = List.empty[String]
    val p: Int ! Writer % String =
      bracket { log ::= "open"; 41 }(_ => log ::= "close")(r => Writer.tell(s"use $r").map(_ => r + 1))
    assertEquals(log, Nil, "nothing runs until the program does")
    assertEquals((Writer.run(p)).run, (List("use 41"), 42))
    assertEquals(log.reverse, List("open", "close"))
    // a program is a value: run twice, it acquires and releases twice
    assertEquals((Writer.run(p)).run, (List("use 41"), 42))
    assertEquals(log.reverse, List("open", "close", "open", "close"))
  }

  test("a throwing finalizer does not skip the others, and its error is reported") {
    var log = List.empty[String]
    val prog: Int ! Resource =
      for
        _ <- Resource.acquire("a")(_ => log ::= "close a")
        _ <- Resource.acquire("b")(_ => throw RuntimeException("close b failed"))
        _ <- Resource.acquire("c")(_ => log ::= "close c")
      yield 1
    val e = intercept[RuntimeException](Resource.scoped(prog))
    assertEquals(e.getMessage, "close b failed")
    assertEquals(log.reverse, List("close c", "close a"), "every finalizer ran, in reverse order")
  }

  test("a failing use keeps its error; a failing release is attached, not swapped in") {
    var log = List.empty[String]
    val prog: Int ! Resource =
      for
        _ <- Resource.acquire("a")(_ => log ::= "close a")
        _ <- Resource.acquire("b")(_ => throw RuntimeException("close b failed"))
        n <- pure[Int, Resource](0)
      yield 1 / n
    val e = intercept[ArithmeticException](Resource.scoped(prog))
    assertEquals(e.getSuppressed.toList.map(_.getMessage), List("close b failed"))
    assertEquals(log, List("close a"))
  }

  test("a failing Async step keeps its error over a failing release") {
    val e = intercept[IllegalStateException]:
      bracket(0)(_ => throw RuntimeException("release failed"))(_ => async[Int](throw IllegalStateException("step failed"))).runWith
    assertEquals(e.getSuppressed.toList.map(_.getMessage), List("release failed"))
  }

  test("bracketNow: a failing use keeps its error over a failing release") {
    val e = intercept[IllegalStateException]:
      bracketNow(0)(_ => throw RuntimeException("release failed"))(_ => pure[Int, Produce](0).map[Int](_ => throw IllegalStateException("use failed"))).runWith
    assertEquals(e.getMessage, "use failed")
    assertEquals(e.getSuppressed.toList.map(_.getMessage), List("release failed"))
  }

  test("Resource.open's closer runs every finalizer even when one throws") {
    var log = List.empty[String]
    val (_, close) = Resource.open(
      for
        _ <- Resource.acquire("a")(_ => log ::= "close a")
        _ <- Resource.acquire("b")(_ => throw RuntimeException("close b failed"))
      yield ())
    val e = intercept[RuntimeException](close())
    assertEquals(e.getMessage, "close b failed")
    assertEquals(log, List("close a"))
  }

  test("bracket releases at a raise, whichever handler catches it outside") {
    var released = 0
    val p: Int ! Throws % String = bracket(1)(_ => released += 1)(_ => raise[String, Int]("boom"))
    assertEquals((runEither(p)).run, Left("boom"))
    assertEquals(released, 1)
    assertEquals((runEither(p.recover(_ => pure(0)))).run, Right(0))
    assertEquals(released, 2, "recovered outside: released exactly once more")
  }

  test("bracket releases at a None, a halt and a pruned branch") {
    var released = 0
    val m: Int ! Maybe = bracket(1)(_ => released += 1)(_ => None.maybe)
    assertEquals((Maybe.run(m)).run, None)
    assertEquals(released, 1)
    val h: Int ! Chronicle % String = bracket(1)(_ => released += 1)(_ => Chronicle.confess[String, Int]("bad"))
    assertEquals((Chronicle.run(h)).run, Chronicle.Verdict.Failed(Vector("bad")))
    assertEquals(released, 2)
    val c: Int ! Choose = bracket(1)(_ => released += 1)(_ => choose[Int]())
    assertEquals((runChoice(c)).run, Seq())
    assertEquals(released, 3)
  }

  test("a Some is not final: the release waits for the end of use") {
    var log = List.empty[String]
    val m: Int ! Maybe = bracket { log ::= "open"; 1 }(_ => log ::= "close")(r => Some(r).maybe.map { x => log ::= "use"; x })
    assertEquals((Maybe.run(m)).run, Some(1))
    assertEquals(log.reverse, List("open", "use", "close"))
  }

  test("bracket releases when a forwarded Async step throws") {
    var released = 0
    val _ = intercept[RuntimeException]:
      bracket(0)(_ => released += 1)(_ => async[Int](throw RuntimeException("boom"))).runWith
    assertEquals(released, 1)
    assertEquals(bracket(41)(_ => released += 1)(r => async(r + 1)).runWith, 42)
    assertEquals(released, 2)
  }

  test("a forwarded Async.Run that THROWS still releases (resource-async-failure)") {
    var log = List.empty[String]
    val prog: Int ! Resource + Async = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close")
      .flatMap(_ => okay.async[Int] { log ::= "run"; throw RuntimeException("boom") })
    val out = Async.runAsync(Resource.run(prog)).value
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "run", "close"))
  }

  test("a forwarded Async.Await whose callback answers Left still releases") {
    var log = List.empty[String]
    val prog: Int ! Resource + Async = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close")
      .flatMap(_ => Async.await[Int](k => { k(Left(RuntimeException("no"))); () => () }))
    val out = Async.runAsync(Resource.run(prog)).value
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "close"))
  }

  test("a ROW: Async + Throws — a throwing Run, raised where it was, still releases") {
    var log = List.empty[String]
    type G = Throws % String
    val prog: Int ! Resource + Async + G = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close")
      .flatMap(_ => okay.async[Int] { log ::= "run"; throw RuntimeException("boom") })
    val out = Async.runAsync(runEither(Resource.run(prog))).value
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "run", "close"))
  }
}

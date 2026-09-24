package scala2probe

import okay.scala2._

/** okay.scala2.Eff from Scala 2.13: several effects in one program,
 * the row an intersection (specs/scala2-facade.md, stage 2) */
class TestEffFromScala2 extends munit.FunSuite {

  val prog: Int ! (State[Int] + Writer[String]) = for {
    n <- State.get[Int]
    _ <- Writer.tell("saw " + n)
    _ <- State.set(n + 1)
    m <- State.get[Int]
    _ <- Writer.tell("now " + m)
  } yield m * 10

  test("State and Writer in one program, handled in either order") {
    assertEquals(!.run(Writer.run(State.handle(1)(prog))), (Vector("saw 1", "now 2"), (2, 20)))
    assertEquals(!.run(State.handle(1)(Writer.run(prog))), (2, (Vector("saw 1", "now 2"), 20)))
  }

  test("Reader, State and Throws: a raise stops the program, the state outside it still answers") {
    def step(limit: Int): Int ! (Reader[Int] + State[Int] + Throws[String]) = for {
      max <- Reader.ask[Int]
      _ <- State.modify[Int](_ + 1)
      n <- State.get[Int]
      r <- if (n > max) Throws.raise[String, Int]("over " + max) else pure(n)
    } yield r + limit
    val twice = step(0).flatMap(_ => step(100))

    assertEquals(!.run(State.handle(0)(Throws.runEither(Reader.run(5)(twice)))), (2, Right(102)))
    assertEquals(!.run(State.handle(0)(Throws.runEither(Reader.run(1)(twice)))), (2, Left("over 1")))
  }

  test("Async: nothing runs until runAsync, and attempt turns a throw into Throws") {
    var ran = 0
    val p = Async { ran += 1; ran }.map(_ * 2)
    assertEquals(ran, 0)
    assertEquals(p.runWith, 2)

    val boom = new IllegalStateException("boom")
    val failing = Async.catching[Int](throw boom)
    assertEquals(Throws.runEither(failing).runWith, Left(boom))
  }

  test("a program with an effect left unhandled does not compile") {
    val unhandled = compileErrors("!.run(State.handle(1)(prog))")
    assert(unhandled.contains("type mismatch"), unhandled)
    assertEquals(compileErrors("!.run(Writer.run(State.handle(1)(prog)))"), "")
  }

  test("Prog and Eff are the same program") {
    val viaEff = Eff.fromProg(Prog.delay(20)).flatMap(n => Async.catching(n + 1))
    assertEquals(Eff.toProg(viaEff).run(), 21)
  }
}

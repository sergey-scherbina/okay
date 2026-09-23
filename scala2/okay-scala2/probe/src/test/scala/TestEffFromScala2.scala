package scala2probe

import okay.scala2._

/** okay.scala2.Eff from Scala 2.13: several effects in one program,
 * the row an intersection (specs/scala2-facade.md, stage 2) */
class TestEffFromScala2 extends munit.FunSuite {

  val prog: Eff[State[Int] with Writer[String], Int] = for {
    n <- State.get[Int]
    _ <- Writer.tell("saw " + n)
    _ <- State.put(n + 1)
    m <- State.get[Int]
    _ <- Writer.tell("now " + m)
  } yield m * 10

  test("State and Writer in one program, handled in either order") {
    assertEquals(Eff.run(Writer.run(State.run(1)(prog))), (Vector("saw 1", "now 2"), (2, 20)))
    assertEquals(Eff.run(State.run(1)(Writer.run(prog))), (2, (Vector("saw 1", "now 2"), 20)))
  }

  test("Reader, State and Throws: a raise stops the program, the state outside it still answers") {
    def step(limit: Int): Eff[Reader[Int] with State[Int] with Throws[String], Int] = for {
      max <- Reader.ask[Int]
      _ <- State.modify[Int](_ + 1)
      n <- State.get[Int]
      r <- if (n > max) Throws.raise[String, Int]("over " + max) else Eff.pure(n)
    } yield r + limit
    val twice = step(0).flatMap(_ => step(100))

    assertEquals(Eff.run(State.run(0)(Throws.run(Reader.run(5)(twice)))), (2, Right(102)))
    assertEquals(Eff.run(State.run(0)(Throws.run(Reader.run(1)(twice)))), (2, Left("over 1")))
  }

  test("Async: nothing runs until runAsync, and attempt turns a throw into Throws") {
    var ran = 0
    val p = Async.delay { ran += 1; ran }.map(_ * 2)
    assertEquals(ran, 0)
    assertEquals(Eff.runAsync(p), 2)

    val boom = new IllegalStateException("boom")
    val failing = Async.attempt[Int](throw boom)
    assertEquals(Eff.runAsync(Throws.run(failing)), Left(boom))
  }

  test("a program with an effect left unhandled does not compile") {
    val unhandled = compileErrors("Eff.run(State.run(1)(prog))")
    assert(unhandled.contains("type mismatch"), unhandled)
    assertEquals(compileErrors("Eff.run(Writer.run(State.run(1)(prog)))"), "")
  }

  test("Prog and Eff are the same program") {
    val viaEff = Eff.fromProg(Prog.delay(20)).flatMap(n => Async.attempt(n + 1))
    assertEquals(Eff.toProg(viaEff).run(), 21)
  }
}

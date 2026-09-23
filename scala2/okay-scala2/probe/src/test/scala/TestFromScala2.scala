package scala2probe

import okay.scala2.Prog

/** Written in Scala 2.13 and compiled by scalac 2.13 with
 * -Ytasty-reader: if a signature in okay-scala2 stops being readable
 * from Scala 2, THIS file stops compiling (specs/scala2-facade.md). */
class TestFromScala2 extends munit.FunSuite {

  test("a for-comprehension over Prog compiles and runs") {
    val prog = for {
      a <- Prog.pure(20)
      b <- Prog.delay(a + 1)
    } yield a + b + 1
    assertEquals(prog.run(), 42)
  }

  test("delay suspends: nothing runs until run()") {
    var ran = 0
    val prog = Prog.delay { ran += 1; ran }
    assertEquals(ran, 0)
    assertEquals(prog.run(), 1)
    assertEquals(prog.run(), 2)
  }

  test("a throw inside delay and Prog.fail are the same failure") {
    val boom = new IllegalStateException("boom")
    val thrown: Prog[Int] = Prog.delay(throw boom)
    val failed: Prog[Int] = Prog.fail(boom)
    for (p <- List(thrown, failed)) {
      assertEquals(p.runEither(), Left(boom))
      assertEquals(p.attempt.run(), Left(boom))
      assertEquals(p.recover(e => Prog.pure(e.getMessage.length)).run(), 4)
      assert(intercept[IllegalStateException](p.run()) eq boom)
    }
  }

  test("fromEither: Right is the answer, Left the failure") {
    val boom = new RuntimeException("boom")
    assertEquals(Prog.fromEither(Right(7)).run(), 7)
    assertEquals(Prog.fromEither[Int](Left(boom)).runEither(), Left(boom))
  }

  test("a failure stops the rest of the program") {
    var after = false
    val p = Prog.fail[Int](new RuntimeException("x")).flatMap(_ => Prog.delay { after = true; 1 })
    assert(p.runEither().isLeft)
    assert(!after)
  }

  test("sequence over 10 000 programs is stack-safe") {
    val ps = List.tabulate(10000)(i => Prog.delay(i))
    val xs = Prog.sequence(ps).run()
    assertEquals(xs.length, 10000)
    assertEquals(xs.head, 0)
    assertEquals(xs.last, 9999)
  }

  test("pattern matching on the answer, Scala 2 style") {
    val p = Prog.pure(Option(3)).map {
      case Some(n) => n * 2
      case None => 0
    }
    assertEquals(p.run(), 6)
  }
}

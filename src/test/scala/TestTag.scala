package okay

import okay.RowLift.{at, plus}

/** Several instances of ONE signature in one row. */
class TestTag extends munit.FunSuite {

  type Small = Tag.Of["small", State % Int]
  type Big   = Tag.Of["big", State % Int]
  type Both  = Small + Big

  /** an ordinary function, written against a plain State and knowing
   * nothing about keys */
  def bump(by: Int): Int ! (State % Int) =
    for
      n <- State.get[Int]
      _ <- State.set(n + by)
    yield n

  test("one function, two states, one row — and it was not written for it") {
    val p: (Int, Int) ! Both =
      for
        a <- Tag.tag["small", State % Int, Int, okay.Pure](bump(1)).plus[Big]
        b <- Tag.tag["big", State % Int, Int, okay.Pure](bump(10)).at[Both]
      yield (a, b)

    // handling is the effect's OWN: untag one key, run State, repeat
    val afterSmall: (Int, (Int, Int)) ! Big =
      State.handle[Int, (Int, Int), Big](1)(Tag.untag["small", State % Int, (Int, Int), Big](p))
    val (big, (small, answer)) =
      !.run(State.handle[Int, (Int, (Int, Int)), okay.Pure](100)(
        Tag.untag["big", State % Int, (Int, (Int, Int)), okay.Pure](afterSmall)))
    assertEquals(answer, (1, 100))
    assertEquals(small, 2)     // 1 + 1
    assertEquals(big, 110)     // 100 + 10
  }

  test("a key tells apart two instances of a signature that carries nothing") {
    type A = Tag.Of["a", Reader % Int]
    type B = Tag.Of["b", Reader % Int]
    val p: (Int, Int) ! (A + B) =
      for
        x <- Tag.one["a", Reader % Int, Int](Reader.Ask()).plus[B]
        y <- Tag.one["b", Reader % Int, Int](Reader.Ask()).at[A + B]
      yield (x, y)
    val inner = Reader.run[Int, (Int, Int), A](7)(
      Tag.untag["b", Reader % Int, (Int, Int), A](p.at[B + A]))
    val out = !.run(Reader.run[Int, (Int, Int), okay.Pure](1)(
      Tag.untag["a", Reader % Int, (Int, Int), okay.Pure](inner)))
    assertEquals(out, (1, 7))
  }

  test("a tagged effect can also be handled by its own comonadic handler") {
    enum Beep[+A] derives okay.Effect:
      case Boop() extends Beep[Int]
    val h: Handler[Beep] = new:
      def handle[A](e: Beep[A]): A = e match { case Beep.Boop() => 42 }
    val p: Int ! Tag.Of["x", Beep] = Tag.one["x", Beep, Int](Beep.Boop())
    assertEquals(p.runWith(using Tag.handler["x", Beep](h)), 42)
  }
}

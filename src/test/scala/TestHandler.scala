package okay

import okay.Row.*

/** level 1: one `handle` for every ready effect, its handler a value */
class TestHandler extends munit.FunSuite:

  test("State as a value: p.handle(State(5))") {
    val p: Int ! State % Int = State.get[Int].flatMap(s => State.set(s + 1).map(_ => s * 2))
    assertEquals(p.handle(State(5)).run, (6, 10))
  }

  test("two effects, taken off one at a time, in either order") {
    val p: Int ! State % Int + Throws % String =
      State.get[Int].plus[Throws % String].flatMap(s => if s > 3 then raise[String, Int]("big").plus[State % Int] else pure(s))
    assertEquals(p.handle(State(5)).handle(Throws.either).run, Left("big"))
    assertEquals(p.handle(Throws.either).handle(State(1)).run, (1, Right(1)))
  }

  test("every ready effect as a value: Reader, Writer, Maybe, Choose") {
    val r: Int ! Reader % Int = Reader.ask[Int].map(_ * 2)
    assertEquals(r.handle(Reader(21)).run, 42)
    val w: Int ! Writer % String = Writer.tell("a").flatMap(_ => Writer.tell("b")).map(_ => 1)
    assertEquals(w.handle(Writer.log).run, (Seq("a", "b"), 1))
    val m: Int ! Maybe = Maybe.none[Int]
    assertEquals(m.handle(Maybe.option).run, None)
    val c: Int ! Choose = choose(1, 2).flatMap(x => choose(10, 20).map(_ + x))
    assertEquals(c.handle(Choose.all).run, Seq(11, 21, 12, 22))
  }

  test("reset is one of them: p.handle(Reset[Int])") {
    val p: Int ! Shift % Int + State % Int =
      for
        x <- shift[Int, Int, State % Int](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
      yield x * 2 + s
    assertEquals(p.handle(Reset[Int]).handle(State(5)).run, (5, 32))
  }

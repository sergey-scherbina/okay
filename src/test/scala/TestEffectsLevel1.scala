package okay

/** level 1 through the typeclass: one program, written once over `Effects[M]`, in Free and in Eager */
class TestEffectsLevel1 extends munit.FunSuite:

  type Row = Shift % Int + State % Int

  /** reset { shift(k => k(1) + k(10)) * 2 + get } — with no encoding named */
  def program[M[_[+_], _]](using M: Effects[M]): M[State % Int, Int] =
    M.reset[Int, State % Int](
      M.shift[Int, Int, State % Int](k => k(1).flatMap(a => k(10).map(b => a + b))).flatMap(x =>
        M.perform[Row, Int](State.Get[Int, Int]()).map(s => x * 2 + s)))

  test("Free: the typeclass's shift/reset are the top-level ones") {
    val E = summon[Effects[Free]]
    assertEquals(E.run(E.handle(program[Free], State(5))), (5, 32))
  }

  test("Eager: the same program, the same answer") {
    val E = summon[Effects[Eager]]
    assertEquals(E.run(E.handle(program[Eager], State(5))), (5, 32))
  }

  test("shift0 and a dropped continuation, in both encodings") {
    def early[M[_[+_], _]](using M: Effects[M]): M[okay.Pure, Int] =
      M.reset[Int, okay.Pure](M.shift0[Int, Int, okay.Pure](_ => M.pure(42)).map(_ + 1))
    assertEquals(summon[Effects[Free]].run(early[Free]), 42)
    assertEquals(summon[Effects[Eager]].run(early[Eager]), 42)
  }

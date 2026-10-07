package okay.freer


import okay.freer.Row.*
import okay.freer.Shift.{collect, emit, exit}

/** shift-patterns: exit and emit/collect — in a keyed `reset` and in a `collect`, one evidence for both
 * (specs/shift-merge.md) */
class TestShiftPatterns extends munit.FunSuite:

  test("exit leaves the reset block with its answer; what follows is dropped") {
    var after = false
    val p: Int ! Pure = reset {
      for
        x <- pure[Shift % Int, Int](20)
        _ <- if x > 10 then exit[Unit](x * 2) else pure[Shift % Int, Unit](())
        _ = after = true
      yield x
    }
    assertEquals(p.run, 40)
    assert(!after)
  }

  test("collect and emit: a generator, in order, with State beside it") {
    def count(n: Int)(using Shift.Emitting.Aux[Int, State % Int]): Unit ! Shift % ? + State % Int =
      if n == 0 then pure(())
      else
        for
          s <- State.get[Int].plus[Shift % ?]
          _ <- emit(s * 10)
          _ <- State.set(s + 1).plus[Shift % ?]
          _ <- !.tailcall(count(n - 1))
        yield ()
    val p: List[Int] ! State % Int = collect(count(3))
    assertEquals(p.handle(State(1)).run, (4, List(10, 20, 30)))
  }

  test("collect: 100 000 emits") {
    def all(n: Int)(using Shift.Emitting.Aux[Int, Pure]): Unit ! Shift % ? =
      if n == 0 then pure(()) else emit(n).flatMap(_ => !.tailcall(all(n - 1)))
    assertEquals(collect[Int, Pure](all(100000)).run.length, 100000)
  }

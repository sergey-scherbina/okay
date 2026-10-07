package okay.kyo

import okay.async
import okay.freer.{!}
import okay.given
import okay.freer.given
import okay.std.given
import KyoInterop.*
import _root_.kyo.{<, AllowUnsafe, Duration, KyoApp}

class TestKyoInterop extends munit.FunSuite {

  test("pure kyo evaluates into okay") {
    val k: Int < Any = (1: Int < Any).flatMap((x: Int) => x + 41)
    assertEquals(!.run(fromKyo(k)), 42)
  }

  test("kyo async runs inside one okay operation") {
    val k: Int < _root_.kyo.Async = _root_.kyo.Async.run(21).flatMap(f => f.get).flatMap((x: Int) => x * 2)
    assertEquals(fromKyoAsync(k).runWith, 42)
  }

  test("structural mapping: Reader <-> Env") {
    import okay.freer.{%}
    import okay.freer.{effect, pure}
    import okay.std.{Reader}
    import _root_.kyo.Env
    val ours: Int ! Reader % Int =
      effect[Reader % Int, Int](Reader.Ask()).flatMap(x => pure(x * 2))
    assertEquals(Env.run(21)(toKyoEnv(ours)).eval, 42)
    val theirs: Int < Env[Int] = Env.get[Int].flatMap((x: Int) => x + 1)
    assertEquals(!.run(okay.std.Reader.run[Int, Int, okay.freer.Pure](41)(
      okay.freer.!.widen(fromKyoEnv(theirs)))), 42)
  }

  test("structural mapping: Writer <-> Emit, tell for tell") {
    import okay.freer.{%}
    import okay.freer.{effect}
    import okay.std.{Writer}
    import _root_.kyo.Emit
    val ours: Int ! Writer % String =
      effect[Writer % String, Unit](Writer("a")).flatMap(_ =>
        effect[Writer % String, Unit](Writer("b")).map(_ => 7))
    val (told, a) = Emit.run[String](toKyoEmit(ours)).eval
    assertEquals(told.toList, List("a", "b"))
    assertEquals(a, 7)
    val theirs: Int < Emit[String] =
      Emit.valueWith("x")(Emit.valueWith("y")(5: Int < Emit[String]))
    val back = fromKyoEmit(theirs)
    assertEquals(okay.std.Writer.uncons(back).toOption.map(_._1), Some("x"))
    val (ws, r) = !.run(okay.std.Writer.run[String, Int, okay.freer.Pure](okay.freer.!.widen(back)))
    assertEquals((ws, r), (Seq("x", "y"), 5))
  }

  test("structural mapping: Throws <-> Abort") {
    import okay.freer.{%}
    import okay.freer.{effect}
    import okay.std.{Throws}
    import _root_.kyo.Abort
    val ours: Int ! Throws % String =
      effect[Throws % String, Int](Throws("boom"))
    assertEquals(Abort.run[String](toKyoAbort(ours)).eval.foldFailureOrThrow(e => e)(_.toString), "boom")
    val theirs: Int < Abort[String] = Abort.fail("bad")
    assertEquals(!.run(okay.std.runEither[Int, Nothing, String](okay.freer.!.widen(fromKyoAbort(theirs)))),
      Left("bad"))
    assertEquals(!.run(okay.std.runEither[Int, Nothing, String](
      okay.freer.!.widen(fromKyoAbort(7: Int < Abort[String])))), Right(7))
  }

  test("structural mapping: Choose <-> Choice — the same arrow") {
    import _root_.kyo.Choice
    val ours: Int ! okay.std.Choose =
      okay.std.choose(1, 2, 3).flatMap(x => okay.std.choose(10, 20).map(x * _))
    assertEquals(Choice.run(toKyoChoice(ours)).eval.toList.sorted,
      List(10, 20, 20, 30, 40, 60))
    val theirs: Int < Choice =
      Choice.get(Seq(1, 2)).flatMap((x: Int) => Choice.get(Seq(10, 20)).flatMap((y: Int) => x * y))
    assertEquals(!.run(okay.std.runChoice[Int, okay.freer.Pure](okay.freer.!.widen(fromKyoChoice(theirs)))).sorted,
      Seq(10, 20, 20, 40))
  }

  test("okay async becomes a kyo suspension") {
    import AllowUnsafe.embrace.danger
    val k = toKyo(async(40).map(_ + 2))
    assertEquals(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(k).getOrThrow, 42)
  }
}

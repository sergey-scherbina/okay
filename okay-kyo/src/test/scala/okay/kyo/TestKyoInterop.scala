package okay.kyo

import okay.{!, async}
import okay.given
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
    import okay.{%, Reader, effect, pure}
    import _root_.kyo.Env
    val ours: Int ! Reader % Int =
      effect[Reader % Int, Int](Reader.Ask()).flatMap(x => pure(x * 2))
    assertEquals(Env.run(21)(toKyoEnv(ours)).eval, 42)
    val theirs: Int < Env[Int] = Env.get[Int].flatMap((x: Int) => x + 1)
    assertEquals(!.run(okay.Reader.run[Int, Int, Nothing](41)(
      okay.!.widen(fromKyoEnv(theirs)))), 42)
  }

  test("structural mapping: Writer <-> Emit, tell for tell") {
    import okay.{%, Writer, effect, pure}
    import _root_.kyo.Emit
    val ours: Int ! Writer % String =
      effect[Writer % String, String](Writer("a")).flatMap(_ =>
        effect[Writer % String, String](Writer("b")).map(_ => 7))
    val (told, a) = Emit.run[String](toKyoEmit(ours)).eval
    assertEquals(told.toList, List("a", "b"))
    assertEquals(a, 7)
    val theirs: Int < Emit[String] =
      Emit.valueWith("x")(Emit.valueWith("y")(5: Int < Emit[String]))
    val back = fromKyoEmit(theirs)
    assertEquals(okay.Writer.uncons(back).toOption.map(_._1), Some("x"))
    val (ws, r) = !.run(okay.Writer.run[String, Int, Nothing](okay.!.widen(back)))
    assertEquals((ws, r), (Seq("x", "y"), 5))
  }

  test("structural mapping: Throws <-> Abort") {
    import okay.{%, Throws, effect, pure}
    import _root_.kyo.Abort
    val ours: Int ! Throws % String =
      effect[Throws % String, Int](Throws("boom"))
    assertEquals(Abort.run[String](toKyoAbort(ours)).eval.foldFailureOrThrow(e => e)(_.toString), "boom")
    val theirs: Int < Abort[String] = Abort.fail("bad")
    assertEquals(!.run(okay.runEither[Int, Nothing, String](okay.!.widen(fromKyoAbort(theirs)))),
      Left("bad"))
    assertEquals(!.run(okay.runEither[Int, Nothing, String](
      okay.!.widen(fromKyoAbort(7: Int < Abort[String])))), Right(7))
  }

  test("structural mapping: Choose <-> Choice — the same arrow") {
    import _root_.kyo.Choice
    val ours: Int ! okay.Choose =
      okay.choose(1, 2, 3).flatMap(x => okay.choose(10, 20).map(x * _))
    assertEquals(Choice.run(toKyoChoice(ours)).eval.toList.sorted,
      List(10, 20, 20, 30, 40, 60))
    val theirs: Int < Choice =
      Choice.get(Seq(1, 2)).flatMap((x: Int) => Choice.get(Seq(10, 20)).flatMap((y: Int) => x * y))
    assertEquals(!.run(okay.runChoice[Int, Nothing](okay.!.widen(fromKyoChoice(theirs)))).sorted,
      Seq(10, 20, 20, 40))
  }

  test("okay async becomes a kyo suspension") {
    import AllowUnsafe.embrace.danger
    val k = toKyo(async(40).map(_ + 2))
    assertEquals(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(k).getOrThrow, 42)
  }
}

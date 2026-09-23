package okay

/**
 * widen-split (specs/widen-split.md): `!.widen` is a coercion and
 * `!.normalize` the walk; they agree on every tree shape, and the
 * coercion forces nothing — the rerun law the eager head broke.
 */
class TestWidenSplit extends munit.FunSuite:

  type W = Writer % Int
  type R = Writer % Int + State % Int

  /** every shape the walk matches: Pure, Inject, Bind, Delay, Bind(Delay, f) */
  def shapes: List[(String, Unit ! W)] = List(
    "pure" -> pure(()),
    "inject" -> Writer.tell(1),
    "bind" -> Writer.tell(1).flatMap(_ => Writer.tell(2)),
    "delay" -> Free.delay(() => Writer.tell(3)),
    "bind-delay" -> Free.delay(() => Writer.tell(4)).flatMap(_ => Writer.tell(5)),
    "deep" -> (1 to 200).foldLeft(pure[W, Unit](()))((p, i) => p.flatMap(_ => Writer.tell(i))))

  def told(p: Unit ! R): Seq[Int] = !.run(Writer.run(State.handle(0)(p).map(_ => ())))._1

  test("widen and normalize agree on every shape: the same told elements") {
    for (name, p) <- shapes do
      assertEquals(told(!.widen[Unit, W, State % Int](p)), told(!.normalize[Unit, W, State % Int](p)), name)
  }

  test("widen forces nothing: a deferred head is entered once per run, not at widen time") {
    var entered = 0
    val p: Unit ! W = Free.delay(() => { entered += 1; Writer.tell(9) })
    val w = !.widen[Unit, W, State % Int](p)
    assertEquals(entered, 0, "widen itself entered the thunk")
    assertEquals(told(w), Seq(9)); assertEquals(entered, 1)
    assertEquals(told(w), Seq(9)); assertEquals(entered, 2, "the widened value is re-runnable")
    // the walk keeps the same law now (the deferred shapes rebuilt as deferred)
    entered = 0
    val n = !.normalize[Unit, W, State % Int](p)
    assertEquals(entered, 0); assertEquals(told(n), Seq(9)); assertEquals(told(n), Seq(9)); assertEquals(entered, 2)
  }

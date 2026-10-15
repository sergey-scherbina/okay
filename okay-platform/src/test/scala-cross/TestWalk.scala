package okay

import okay.std.{Reader, Writer}

/** okay-freer's `resume` on the kernel: a program walked to its next operation, the rest a program again */
class TestWalk extends munit.FunSuite:
  type W = Writer % Int
  type R = Reader % Int

  def counting(n: Int): Int ! W = if n == 0 then pure(0) else Writer.tell(n).flatMap(_ => defer(counting(n - 1)))

  /** a walker of Writer alone: every told value, in order — the shape of a freer `resume` loop */
  @scala.annotation.tailrec final def drain[A](p: A ! W, acc: List[Int]): (List[Int], A) = Walk.view(p) match
    case Walk.Done(a) => (acc.reverse, a)
    case o: Walk.Op[W, x, A, W] => o.op match
      case Writer.Say(w) => drain(o.resume(()), w :: acc)

  test("view: each operation in order, the rest resumed"):
    assertEquals(drain(counting(5), Nil), (List(5, 4, 3, 2, 1), 0))

  test("view: a million steps in constant stack"):
    assertEquals(drain(counting(1000000), Nil)._1.length, 1000000)

  test("the rest is a value: resumed twice, each a run of its own"):
    Walk.view(counting(2)) match
      case o: Walk.Op[W, x, Int, W] =>
        val rest = o.resume(().asInstanceOf[x])
        assertEquals(drain(rest, Nil), (List(1), 0))
        assertEquals(drain(rest, Nil), (List(1), 0))
      case other => fail(s"not an operation: $other")

  test("next: Writer walked among Reader, the asks answered where the walk is"):
    val p: Int ! W + R = Reader.ask[Int].flatMap(i => Writer.tell(i).flatMap(_ => Reader.ask[Int].map(_ * 2)))
    def told(q: Int ! W + R): List[Int] ! R = Walk.next[W, Int, W + R](q).flatMap:
      case Walk.Done(_) => pure(Nil)
      case o: Walk.Op[W, x, Int, W + R] => o.op match
        case Writer.Say(w) => told(o.resume(())).map(w :: _)
    assertEquals(Reader.run(7)(told(p)).run, List(7))

  test("the rest run where it was not walked: its operations re-placed"):
    val p: Int ! W + R = Writer.tell(1).flatMap(_ => Reader.ask[Int].flatMap(i => Writer.tell(i).map(_ => i)))
    val first = Reader.run(5)(Walk.next[W, Int, W + R](p)).run
    first match
      case o: Walk.Op[W, x, Int, W + R] =>
        val (ws, a) = Reader.run(9)(Writer.run(o.resume(().asInstanceOf[x]))).run
        assertEquals((ws, a), (Seq(9), 9))
      case other => fail(s"not an operation: $other")

  test("fail: the rest given a failure raised where the operation was"):
    val p: Int ! W = Writer.tell(1).map(_ => 0)
    Walk.view(p) match
      case o: Walk.Op[W, x, Int, W] =>
        intercept[IllegalStateException](drain(o.fail(IllegalStateException("boom")), Nil))
      case other => fail(s"not an operation: $other")

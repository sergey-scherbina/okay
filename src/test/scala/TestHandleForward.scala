package okay

import !.*
import scala.annotation.nowarn

/** the HANDLED side: an operation carrying its own answer */
case class Claim[+A](a: A) derives Effect

/**
 * What a handler may observe, pinned before `handle`'s forwarding arm
 * is touched (BACKLOG handle-forward-fast).
 *
 * `Effects.handle` today folds the whole program through `Cont` and
 * answers a FORWARDED operation with `shift(k => perform(e).flatMap(k))`
 * — a continuation capture spent on an operation no handler claims,
 * measured at +112.7 bytes per forwarded operation (handle-decompose).
 * Moving that arm onto the tree, the way `relay` already does, is only
 * sound if NOTHING an aborting or a multi-shot handler observes
 * changes. These tests are that claim, written as assertions rather
 * than as an argument.
 *
 * The G side is `Produce`, an IDENTITY signature, so a tracing handler
 * records the forwarded operations themselves and the trace IS the
 * observable: which operations reached the target program, in what
 * order, and how many times.
 */
class TestHandleForward extends munit.FunSuite {

  val E = summon[Effects[Free]]

  /** run a forwarded program, recording every operation it performs */
  def trace(p: Int ! Produce): (Int, List[Any]) =
    val log = List.newBuilder[Any]
    val h = summon[Handler[Produce]].tracing(log += _)
    val a = p.runWith(using h)
    (a, log.result())

  /** produce(1) ; claim(10) ; produce(3) — one of each, interleaved */
  def mixed: Int ! Claim + Produce =
    effect[Claim + Produce, Int](1).flatMap: x =>
      effect[Claim + Produce, Int](Claim(10)).flatMap: y =>
        effect[Claim + Produce, Int](3).map(z => x + y + z)

  @nowarn("msg=cannot be checked at runtime")
  def handled(h: Claim !> (Int ! Produce)): Int ! Produce =
    E.handle[Claim, Produce](mixed)(pure(_))(h)

  test("forwarding: a resuming handler leaves both forwarded ops, in order") {
    val (a, ops) = trace(handled([X] => (c: Claim[X]) => Cont.Pure(c.a)))
    assertEquals(a, 14)
    assertEquals(ops, List(1, 3))
  }

  test("forwarding: an ABORTING handler keeps what was forwarded BEFORE it") {
    // the handler drops the continuation, so produce(3) never happens —
    // but produce(1) was already committed to the target program and
    // an abort cannot un-perform it. That asymmetry is the whole
    // argument for moving the forwarding arm, so it is asserted.
    val (a, ops) = trace(handled([X] => (_: Claim[X]) => shift(_ => pure(-1))))
    assertEquals(a, -1)
    assertEquals(ops, List(1))
  }

  test("forwarding: a MULTI-SHOT handler forwards what follows it TWICE") {
    val h: Claim !> (Int ! Produce) =
      [X] => (c: Claim[X]) =>
        shift[X, Int ! Produce, Int ! Produce]: k =>
          k(c.a).flatMap(x => k(c.a).map(y => x + y))
    val (a, ops) = trace(handled(h))
    // each run of the continuation performs produce(3) and answers
    // 1 + 10 + 3; the handler adds the two answers
    assertEquals(a, 28)
    assertEquals(ops, List(1, 3, 3))
  }

  test("forwarding: the handled operation's own answer reaches the rest") {
    val (a, ops) = trace(handled([X] => (c: Claim[X]) => Cont.Pure(c.a)))
    assertEquals(a, 14)
    assertEquals(ops, List(1, 3))
  }

  test("stack safety: 100k FORWARDED operations through handle") {
    val n = 100000
    val prog = (1 to n).foldLeft(pure[Claim + Produce, Int](0)): (m, _) =>
      m.flatMap(x => effect[Claim + Produce, Int](x + 1))
    @nowarn("msg=cannot be checked at runtime")
    val g = E.handle[Claim, Produce](prog)(pure(_))([X] => (c: Claim[X]) => Cont.Pure(c.a))
    assertEquals(g.runWith, n)
  }

  test("stack safety: 100k HANDLED operations through handle") {
    val n = 100000
    val prog = (1 to n).foldLeft(pure[Claim + Produce, Int](0)): (m, _) =>
      m.flatMap(x => effect[Claim + Produce, Int](Claim(x + 1)))
    @nowarn("msg=cannot be checked at runtime")
    val g = E.handle[Claim, Produce](prog)(pure(_))([X] => (c: Claim[X]) => Cont.Pure(c.a))
    assertEquals(g.runWith, n)
  }

  test("stack safety: 100k operations ALTERNATING handled and forwarded") {
    val n = 100000
    val prog = (1 to n).foldLeft(pure[Claim + Produce, Int](0)): (m, i) =>
      m.flatMap: x =>
        if i % 2 == 0 then effect[Claim + Produce, Int](Claim(x + 1))
        else effect[Claim + Produce, Int](x + 1)
    @nowarn("msg=cannot be checked at runtime")
    val g = E.handle[Claim, Produce](prog)(pure(_))([X] => (c: Claim[X]) => Cont.Pure(c.a))
    assertEquals(g.runWith, n)
  }
}

package okay

import !.*
import okay.Row.at

case class Fa[+A](a: A) derives Effect
case class Fb[+A](a: A) derives Effect
case class Fc[+A](a: A) derives Effect
case class Fd[+A](a: A) derives Effect

/**
 * `Handler.flat` agrees with `Handler.union` — specs/handler-fusion.md,
 * the `Handler.flat` box. The handlers are identities that RECORD, so
 * "agrees" is checked on which handler saw which operation, in what
 * order, not only on the answer (four identity handlers would answer
 * the same value even if every operation went to the wrong one).
 */
class TestFlat extends munit.FunSuite:

  type Row = Fa + (Fb + (Fc + Fd))

  class Logs:
    val a, b, c, d = List.newBuilder[Any]
    given ha: Handler[Fa] = new Handler[Fa]:
      def handle[A](e: Fa[A]): A = { a += e.a; e.a }
    given hb: Handler[Fb] = new Handler[Fb]:
      def handle[A](e: Fb[A]): A = { b += e.a; e.a }
    given hc: Handler[Fc] = new Handler[Fc]:
      def handle[A](e: Fc[A]): A = { c += e.a; e.a }
    given hd: Handler[Fd] = new Handler[Fd]:
      def handle[A](e: Fd[A]): A = { d += e.a; e.a }
    def seen: (List[Any], List[Any], List[Any], List[Any]) = (a.result(), b.result(), c.result(), d.result())

    val union: Handler[Row] =
      given h34: Handler[Fc + Fd] = Handler.union[Fc, Fd]
      given h234: Handler[Fb + (Fc + Fd)] = Handler.union[Fb, Fc + Fd]
      Handler.union[Fa, Fb + (Fc + Fd)]
    val flat: Handler[Row] = Handler.flat[Row]

  /** one operation at every position, twice, interleaved */
  def mixed: Int ! Row =
    for
      w <- effect[Row, Int](Fa(1))
      x <- effect[Row, Int](Fb(w + 1))
      y <- effect[Row, Int](Fc(x + 1))
      z <- effect[Row, Int](Fd(y + 1))
      w2 <- effect[Row, Int](Fa(z + 1))
      x2 <- effect[Row, Int](Fb(w2 + 1))
      y2 <- effect[Row, Int](Fc(x2 + 1))
      z2 <- effect[Row, Int](Fd(y2 + 1))
    yield z2

  test("flat agrees with union on a mixed program: answer and every handler's trace") {
    val u = Logs(); val f = Logs()
    assertEquals(mixed.runWith(using f.flat), mixed.runWith(using u.union))
    assertEquals(f.seen, u.seen)
    assertEquals(f.seen, (List(1, 5), List(2, 6), List(3, 7), List(4, 8)))
  }

  test("flat agrees with union at each of the four positions alone") {
    def only(mk: Int => Row[Int]): Int ! Row =
      (1 to 100).foldLeft(pure[Row, Int](0))((m, i) => m.flatMap(acc => effect[Row, Int](mk(acc + i))))
    val progs = List(only(Fa(_)), only(Fb(_)), only(Fc(_)), only(Fd(_)))
    for p <- progs do
      val u = Logs(); val f = Logs()
      assertEquals(p.runWith(using f.flat), p.runWith(using u.union))
      assertEquals(f.seen, u.seen)
    // and each program reached exactly its own handler
    val f = Logs()
    val _ = progs(3).runWith(using f.flat)
    assertEquals(f.seen._1, Nil)
    assertEquals(f.seen._4.size, 100)
  }

  test("the row's spelling does not matter: left-nested reads the same members") {
    type L = (Fa + Fb) + (Fc + Fd)
    val f = Logs()
    import f.given
    val h: Handler[L] = Handler.flat[L]
    val p: Int ! L = effect[L, Int](Fd(4)).flatMap(x => effect[L, Int](Fa(x + 1)))
    assertEquals(p.runWith(using h), 5)
    assertEquals(f.seen, (List(5), Nil, Nil, List(4)))
  }

  test("a `%`-shaped member (a lambda over the last argument) is read back to its constructor") {
    // Tag.Of[K, F][Any] is Tag[K, F, Any]: three arguments, the last
    // one the row's — the constructor is rebuilt as a lambda over it
    type T = Fa + Tag.Of["t", Fb]
    val f = Logs()
    import f.given
    given Handler[Tag.Of["t", Fb]] = Tag.handler["t", Fb](f.hb)
    val h: Handler[T] = Handler.flat[T]
    val p: Int ! T =
      effect[T, Int](Fa(1)).flatMap(x => Tag.one["t", Fb](Fb(x + 1)).at[T])
    assertEquals(p.runWith(using h), 2)
    assertEquals(f.seen, (List(1), List(2), Nil, Nil))
  }

  test("a member without a Handler in scope is refused at compile time, naming the member") {
    val e = compileErrors("""
      given Handler[Fa] = new Handler[Fa] { def handle[A](e: Fa[A]): A = e.a }
      Handler.flat[Fa + Fb]
    """)
    assert(e.contains("no Handler"), e)
    assert(e.contains("Fb"), e)
  }

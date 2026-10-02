package okay

/**
 * What to do with `foldCont`'s result (docs/contract.md, "foldCont").
 *
 * `p.foldCont(h)` is `foldMap` into `Cont`: an `A /> S`, a computation
 * still waiting for its LAST continuation. Closing it with `/ k` runs it.
 * The choice of `S`, the answer type, is the choice of what the
 * interpretation produces: one value, every value, or a function of a
 * state. The same program is interpreted three ways below; the handler is
 * the only thing that changes.
 */
class TestFoldCont extends munit.FunSuite:

  enum Op[+A]:
    case Pick(xs: List[Int]) extends Op[Int]

  val pair: (Int, Int) ! Op = for
    a <- effect(Op.Pick(List(1, 2)))
    b <- effect(Op.Pick(List(10, 20)))
  yield (a, b)

  test("S = the answer: resume once, close with / identity") {
    val first: Op !> (Int, Int) = [X] => (e: Op[X]) => e match
      case Op.Pick(xs) => Cont.shift[X, (Int, Int), (Int, Int)](k => k(xs.head))
    assertEquals(pair.foldCont(first) / identity, (1, 10))
  }

  test("S = every answer: resume once per choice, close by wrapping the answer") {
    val all: Op !> List[(Int, Int)] = [X] => (e: Op[X]) => e match
      case Op.Pick(xs) => Cont.shift[X, List[(Int, Int)], List[(Int, Int)]](k => xs.flatMap(k))
    assertEquals(pair.foldCont(all) / (p => List(p)), List((1, 10), (1, 20), (2, 10), (2, 20)))
  }

  test("S = a function of the state: pass the cell along, apply to the start") {
    type St = Int => (Int, Int)
    val counter: Int ! State % Int = for
      n <- State.get[Int]
      _ <- State.set(n + 1)
      m <- State.get[Int]
    yield n * 100 + m
    val cell: (State % Int) !> St = [X] => (e: State[Int, X]) => e match
      case State.Get() => Cont.shift[X, St, St](k => s => k(s)(s))
      case State.Set(s1) => Cont.shift[X, St, St](k => _ => k(s1)(s1))
      case State.Modify(f) => Cont.shift[X, St, St](k => s => k(f(s))(f(s)))
      case State.Update(f) => Cont.shift[X, St, St](k => s => k(f(s)._1)(f(s)._2))
    assertEquals((counter.foldCont(cell) / (a => s => (s, a)))(5), (6, 506))
  }

  test("runWith IS this, with S = A and the last continuation identity") {
    given Answers[Op] with
      def handle[A](a: Op[A]): A = a match
        case Op.Pick(xs) => xs.head
    assertEquals(pair.runWith, pair.foldCont(handler[Op, (Int, Int)]) / identity)
  }

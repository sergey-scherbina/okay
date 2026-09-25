package okay

/**
 * The free selective (specs/applicative-static.md, stage 2): what a
 * program will do, known before it does it.
 */
class TestStatic extends munit.FunSuite {

  /** a signature with a fetch and a flag — the smallest thing that
   * has both leaves and a conditional */
  enum Fetch[+A]:
    case Get(k: String) extends Fetch[Int]
    case Flag(name: String) extends Fetch[Boolean]

  import Fetch.*

  /** the store, and how many times it was asked */
  final class Store(val answers: Map[String, Int]):
    var calls = 0
    def one(k: String): Int = { calls += 1; answers.getOrElse(k, 0) }
    def bulk(ks: Seq[String]): Map[String, Int] =
      calls += 1
      ks.map(k => k -> answers.getOrElse(k, 0)).toMap

  private def handler(st: Store, flag: Boolean, log: collection.mutable.Buffer[String]): Handler[Fetch] =
    new:
      def handle[A](e: Fetch[A]): A = e match
        case Get(k) => log += s"get:$k"; st.one(k)
        case Flag(n) => log += s"flag:$n"; flag

  private val S = summon[Selective[[A] =>> Static[Fetch, A]]]
  import S.pure

  private def get(k: String): Static[Fetch, Int] = Static.op(Get(k))
  private def flag(n: String): Static[Fetch, Boolean] = Static.op(Flag(n))

  test("leaves names both branches before running; toFree runs at most one") {
    // if(flag) then get(a) else get(b) — three operations declared
    val prog = S.ifS(flag("f"))(get("a"))(get("b"))
    assertEquals(prog.leaves.length, 3)
    assertEquals(prog.leaves.collect { case Get(k) => k }.toSet, Set("a", "b"))
    assertEquals(prog.leaves.collect { case Flag(n) => n }, Vector("f"))

    val st = Store(Map("a" -> 1, "b" -> 2))
    val log = collection.mutable.Buffer.empty[String]
    given Handler[Fetch] = handler(st, true, log)
    assertEquals(prog.toFree.runWith, 1)
    assertEquals(log.toList, List("flag:f", "get:a"))  // two, not three
  }

  test("toFree agrees with the hand-written monadic program, answer and order") {
    val spine = S.fmap(get("a"), (x: Int) => (y: Int) => x * 10 + y).app(get("b"))
    val byHand: Int ! Fetch =
      effect[Fetch, Int](Get("a")).flatMap(x => effect[Fetch, Int](Get("b")).map(y => x * 10 + y))

    val (l1, l2) = (collection.mutable.Buffer.empty[String], collection.mutable.Buffer.empty[String])
    assertEquals(spine.toFree.runWith(using handler(Store(Map("a" -> 1, "b" -> 2)), true, l1)), 12)
    assertEquals(byHand.runWith(using handler(Store(Map("a" -> 1, "b" -> 2)), true, l2)), 12)
    assertEquals(l1.toList, l2.toList)
    assertEquals(l1.toList, List("get:a", "get:b"))
  }

  // specs/stack-safety.md: a program nested along each of the three
  // axes a fold used to recurse on, 3 000 deep, folded on a small stack
  private val deep = 3000
  private final case class Count[X](n: Int, x: X)
  private given Selective[Count] with
    def pure[X](x: X): Count[X] = Count(0, x)
    extension [X, Y](f: Count[X => Y])
      def app(a: Count[X]): Count[Y] = Count(f.n + a.n, f.x(a.x))
    extension [X, Y](e: Count[Either[X, Y]])
      def select(f: => Count[X => Y]): Count[Y] = Count(e.n + f.n, e.x.fold(f.x, identity))
  private val count: Fetch ==> Count = [X] => (op: Fetch[X]) => op match
    case Get(_) => Count(1, 1)
    case Flag(_) => Count(1, true)

  test("a deep nest in a select's CONDITION folds without the stack") {
    var s: Static[Fetch, Int] = get("a")
    for _ <- 1 to deep do s = S.fmap(s, (i: Int) => Left(i): Either[Int, Int]).select(pure((i: Int) => i + 1))
    val st = Store(Map("a" -> 1))
    assertEquals(SmallStack.run()(s.leaves.length), 1)
    assertEquals(SmallStack.run()(s.toFree.runWith(using handler(st, true, collection.mutable.Buffer.empty))), deep + 1)
    assertEquals(SmallStack.run()(s.foldMap(count)), Count(1, deep + 1))
  }

  test("a deep nest in an application's ARGUMENT folds without the stack") {
    var s: Static[Fetch, Int] = get("a")
    for _ <- 1 to deep do s = S.fmap(get("a"), (_: Int) => (x: Int) => x + 1).app(s)
    assertEquals(SmallStack.run()(s.foldMap(count)), Count(deep + 1, deep + 1))
  }

  test("a deep nest in a select's FUNCTION side folds without the stack") {
    var s: Static[Fetch, Int] = get("a")
    for _ <- 1 to deep do
      val prev = s
      s = pure(Left(0): Either[Int, Int]).select(S.fmap(prev, (v: Int) => (_: Int) => v + 1))
    assertEquals(SmallStack.run()(s.foldMap(count)), Count(1, deep + 1))
  }

  test("the Selective laws hold, on leaves and on answers") {
    val st = Store(Map("a" -> 1, "b" -> 2))
    def run[A](p: Static[Fetch, A], f: Boolean = true): A =
      p.toFree.runWith(using handler(st, f, collection.mutable.Buffer.empty))

    // identity: x.select(pure(identity)) == x.map(_.merge)  (Mokhov et al. 2019, 2.2)
    val x: Static[Fetch, Either[Int, Int]] = S.fmap(get("a"), (i: Int) => Left(i))
    assertEquals(run(x.select(pure(identity[Int]))), run(S.fmap(x, (e: Either[Int, Int]) => e.merge)))

    // distributivity: pure(v).select(y *> z) == (pure(v).select(y)) *> (pure(v).select(z))
    val v: Either[Int, Int] = Left(5)
    val y = S.fmap(get("a"), (i: Int) => (j: Int) => i + j)
    val z = S.fmap(get("b"), (i: Int) => (j: Int) => i * j)
    assertEquals(run(pure(v).select(y *> z)), run(pure(v).select(y) *> pure(v).select(z)))

    // and the same pair declares the same operations
    assertEquals(pure(v).select(y *> z).leaves.length,
                 (pure(v).select(y) *> pure(v).select(z)).leaves.length)
  }

  test("batching: N leaves, ONE round trip — the point of the type") {
    // a carrier whose app accumulates requests; answering is one call
    final case class Batch[A](keys: Vector[String], run: Map[String, Int] => A)
    given Selective[Batch] with
      def pure[A](a: A): Batch[A] = Batch(Vector.empty, _ => a)
      extension [A, B](f: Batch[A => B])
        def app(a: Batch[A]): Batch[B] =
          Batch(f.keys ++ a.keys, m => f.run(m)(a.run(m)))
      extension [A, B](e: Batch[Either[A, B]])
        // selectA: an accumulating carrier must ask for what the
        // program MIGHT need, which is exactly what `leaves` reports
        def select(f: => Batch[A => B]): Batch[B] =
          Batch(e.keys ++ f.keys, m => e.run(m).fold(f.run(m), identity))

    val toBatch: Fetch ==> Batch = [X] => (op: Fetch[X]) => op match
      case Get(k) => Batch(Vector(k), m => m.getOrElse(k, 0))
      case Flag(_) => Batch(Vector.empty, _ => true)

    val keys = (1 to 50).map(i => s"k$i")
    val spine = traverse(keys)(get)
    val plan = spine.foldMap(toBatch)
    assertEquals(plan.keys.length, 50)

    val st = Store(keys.zipWithIndex.map((k, i) => k -> i).toMap)
    val answers = plan.run(st.bulk(plan.keys))
    assertEquals(st.calls, 1)
    assertEquals(answers, keys.indices.toSeq)

    // the same program run the ordinary way asks fifty times
    val st2 = Store(st.answers)
    assertEquals(spine.toFree.runWith(using handler(st2, true, collection.mutable.Buffer.empty)),
                 keys.indices.toSeq)
    assertEquals(st2.calls, 50)
  }

  test("a RIGHT-nested spine converts too — the side toFree does not defer eagerly") {
    // toFree earns its Delay: the right component of an Ap is
    // converted in place when it is a leaf. This spine nests on the
    // RIGHT instead, so the fallback is the one under test.
    val n = 10000
    val spine = (1 to n).foldRight(pure(List.empty[Int])) { (i, acc) =>
      S.fmap(get(s"k$i"), (x: Int) => (xs: List[Int]) => x :: xs).app(acc)
    }
    assertEquals(spine.leaves.length, n)
    val st = Store(Map.empty)
    assertEquals(spine.toFree.runWith(using handler(st, true, collection.mutable.Buffer.empty)).length, n)
  }

  test("foldMap is stack-safe too — 50 000 leaves, the depth that used to overflow") {
    // static-foldmap-stack-safe: foldMap recursed on the host stack
    // and overflowed between 5 000 and 10 000 leaves. The walk is two
    // loops over a type-aligned Args now, so the spine costs no stack
    // at all. 50 000 is the depth at which the RECURSIVE version of
    // `leaves` died, measured when this limit was first written down.
    val n = 50000
    val spine = traverse(1 to n)(i => get(s"k$i"))
    final case class Count[X](n: Int, x: X)
    given Selective[Count] with
      def pure[X](x: X): Count[X] = Count(0, x)
      extension [X, Y](f: Count[X => Y])
        def app(a: Count[X]): Count[Y] = Count(f.n + a.n, f.x(a.x))
      extension [X, Y](e: Count[Either[X, Y]])
        def select(f: => Count[X => Y]): Count[Y] =
          Count(e.n + f.n, e.x.fold(f.x, identity))
    val nt: Fetch ==> Count = [X] => (op: Fetch[X]) => op match
      case Get(k) => Count(1, 0)
      case Flag(_) => Count(1, true)
    assertEquals(spine.foldMap(nt).n, n)
  }

  test("leaves and toFree are stack-safe on a spine traverse built") {
    val n = 10000
    val spine = traverse(1 to n)(i => get(s"k$i"))
    assertEquals(spine.leaves.length, n)
    val st = Store(Map.empty)
    assertEquals(spine.toFree.runWith(using handler(st, true, collection.mutable.Buffer.empty)).length, n)
  }
}

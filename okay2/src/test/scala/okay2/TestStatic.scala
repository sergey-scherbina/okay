package okay2

/** the fixtures for TestStatic, at top level (a class nested in a suite
 * trips -Xlint's outer check) */
object StaticFixtures {
  /** a signature with a fetch and a flag — the smallest thing that has
   * both leaves and a conditional */
  sealed trait Fetch extends Row { type Op[+A] = Fetch.Op[A] }
  object Fetch {
    sealed trait Op[+A]
    final case class Get(k: String) extends Op[Int]
    final case class Flag(name: String) extends Op[Boolean]
    implicit val effect: Effect[Fetch] = Effect.of[Fetch]
  }

  /** the store, and how many times it was asked */
  final class Store(val answers: Map[String, Int]) {
    var calls = 0
    def one(k: String): Int = { calls += 1; answers.getOrElse(k, 0) }
    def bulk(ks: Seq[String]): Map[String, Int] = {
      calls += 1
      ks.map(k => k -> answers.getOrElse(k, 0)).toMap
    }
  }

  /** THE TEST'S ONE CLAIM: scalac 2 does not refine a method's type
   * parameter by a constructor pattern (`case Get(k)` does not make
   * `A = Int`), so a handler's answer is asserted — as the Scala 3
   * core's own tests do where Scala 3 cannot see it either */
  def answer[A](x: Any): A = x.asInstanceOf[A]

  final case class Batch[A](keys: Vector[String], run: Map[String, Int] => A)
  implicit val batchSelective: Selective[Batch] = new Selective[Batch] {
    def pure[A](a: A): Batch[A] = Batch(Vector.empty, _ => a)
    def app[A, B](f: Batch[A => B], a: Batch[A]): Batch[B] = Batch(f.keys ++ a.keys, m => f.run(m)(a.run(m)))
    def select[A, B](e: Batch[Either[A, B]], f: => Batch[A => B]): Batch[B] = {
      val g = f
      Batch(e.keys ++ g.keys, m => e.run(m).fold(g.run(m), identity))
    }
  }

  final case class Count[X](n: Int, x: X)
  implicit val countSelective: Selective[Count] = new Selective[Count] {
    def pure[X](x: X): Count[X] = Count(0, x)
    def app[X, Y](f: Count[X => Y], a: Count[X]): Count[Y] = Count(f.n + a.n, f.x(a.x))
    def select[X, Y](e: Count[Either[X, Y]], f: => Count[X => Y]): Count[Y] = {
      val g = f
      Count(e.n + g.n, e.x.fold(g.x, identity))
    }
  }
}

/** The free selective: what a program will do, known before it does it.
 * The Scala 3 core's TestStatic. */
class TestStatic extends munit.FunSuite {
  import StaticFixtures._
  import Fetch.{Get, Flag}

  private def handler(st: Store, flag: Boolean, log: collection.mutable.Buffer[String]): Handler[Fetch] =
    new Handler[Fetch] {
      def handle[A](e: Fetch.Op[A]): A = e match {
        case Get(k) => log += s"get:$k"; answer[A](st.one(k))
        case Flag(n) => log += s"flag:$n"; answer[A](flag)
      }
    }

  private val S = implicitly[Selective[({ type L[A] = Static[Fetch, A] })#L]]
  import S.pure

  private def get(k: String): Static[Fetch, Int] = Static.op[Fetch, Int](Get(k))
  private def flag(n: String): Static[Fetch, Boolean] = Static.op[Fetch, Boolean](Flag(n))

  test("leaves names both branches before running; toFree runs at most one") {
    val prog = S.ifS(flag("f"))(get("a"))(get("b"))
    assertEquals(prog.leaves.length, 3)
    assertEquals(prog.leaves.collect { case Get(k) => k }.toSet, Set("a", "b"))
    assertEquals(prog.leaves.collect { case Flag(n) => n }, Vector("f"))

    val st = new Store(Map("a" -> 1, "b" -> 2))
    val log = collection.mutable.Buffer.empty[String]
    assertEquals(prog.toFree.runWith(handler(st, true, log)), 1)
    assertEquals(log.toList, List("flag:f", "get:a"))  // two, not three
  }

  test("toFree agrees with the hand-written monadic program, answer and order") {
    val spine = S.app(S.fmap(get("a"), (x: Int) => (y: Int) => x * 10 + y), get("b"))
    val byHand: Int ! Fetch =
      Free.inject[Fetch, Int](Get("a")).flatMap(x => Free.inject[Fetch, Int](Get("b")).map(y => x * 10 + y))

    val (l1, l2) = (collection.mutable.Buffer.empty[String], collection.mutable.Buffer.empty[String])
    assertEquals(spine.toFree.runWith(handler(new Store(Map("a" -> 1, "b" -> 2)), true, l1)), 12)
    assertEquals(byHand.runWith(handler(new Store(Map("a" -> 1, "b" -> 2)), true, l2)), 12)
    assertEquals(l1.toList, l2.toList)
    assertEquals(l1.toList, List("get:a", "get:b"))
  }

  test("the Selective laws hold, on leaves and on answers") {
    val st = new Store(Map("a" -> 1, "b" -> 2))
    def run[A](p: Static[Fetch, A], f: Boolean = true): A =
      p.toFree.runWith(handler(st, f, collection.mutable.Buffer.empty))

    val x: Static[Fetch, Either[Int, Int]] = S.fmap(get("a"), (i: Int) => Left(i): Either[Int, Int])
    assertEquals(run(S.select(x, pure(identity[Int] _))), run(S.fmap(x, (e: Either[Int, Int]) => e.merge)))

    val v: Either[Int, Int] = Left(5)
    val y = S.fmap(get("a"), (i: Int) => (j: Int) => i + j)
    val z = S.fmap(get("b"), (i: Int) => (j: Int) => i * j)
    def both[A, B](a: Static[Fetch, A], b: Static[Fetch, B]): Static[Fetch, B] = S.app(S.fmap(a, (_: A) => (bb: B) => bb), b)
    assertEquals(run(S.select(pure(v), both(y, z))), run(both(S.select(pure(v), y), S.select(pure(v), z))))
    assertEquals(S.select(pure(v), both(y, z)).leaves.length,
                 both(S.select(pure(v), y), S.select(pure(v), z)).leaves.length)
  }

  test("batching: N leaves, ONE round trip — the point of the type") {
    val toBatch = new Static.To[Fetch, Batch] {
      def apply[X](op: Fetch.Op[X]): Batch[X] = op match {
        case Get(k) => Batch(Vector(k), m => answer[X](m.getOrElse(k, 0)))
        case Flag(_) => Batch(Vector.empty, _ => answer[X](true))
      }
    }
    val keys = (1 to 50).map(i => s"k$i")
    val spine = traverse(keys)(get)
    val plan = spine.foldMap(toBatch)
    assertEquals(plan.keys.length, 50)

    val st = new Store(keys.zipWithIndex.map { case (k, i) => k -> i }.toMap)
    val answers = plan.run(st.bulk(plan.keys))
    assertEquals(st.calls, 1)
    assertEquals(answers, keys.indices.toSeq)

    val st2 = new Store(st.answers)
    assertEquals(spine.toFree.runWith(handler(st2, true, collection.mutable.Buffer.empty)), keys.indices.toSeq)
    assertEquals(st2.calls, 50)
  }

  test("a RIGHT-nested spine converts too — the side toFree does not defer eagerly") {
    val n = 10000
    val spine = (1 to n).foldRight(pure(List.empty[Int])) { (i, acc) =>
      S.app(S.fmap(get(s"k$i"), (x: Int) => (xs: List[Int]) => x :: xs), acc)
    }
    assertEquals(spine.leaves.length, n)
    val st = new Store(Map.empty)
    assertEquals(spine.toFree.runWith(handler(st, true, collection.mutable.Buffer.empty)).length, n)
  }

  test("foldMap is stack-safe too — 50 000 leaves, the depth that used to overflow") {
    val n = 50000
    val spine = traverse(1 to n)(i => get(s"k$i"))
    val nt = new Static.To[Fetch, Count] {
      def apply[X](op: Fetch.Op[X]): Count[X] = op match {
        case Get(_) => Count(1, answer[X](0))
        case Flag(_) => Count(1, answer[X](true))
      }
    }
    assertEquals(spine.foldMap(nt).n, n)
  }

  test("leaves and toFree are stack-safe on a spine traverse built") {
    val n = 10000
    val spine = traverse(1 to n)(i => get(s"k$i"))
    assertEquals(spine.leaves.length, n)
    val st = new Store(Map.empty)
    assertEquals(spine.toFree.runWith(handler(st, true, collection.mutable.Buffer.empty)).length, n)
  }
}

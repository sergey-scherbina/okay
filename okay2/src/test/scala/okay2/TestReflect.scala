package okay2

/** `Effects` in okay's shape (the Scala 3 core's TestReflect and the trait's level 1): `reify`/`reflect` are a
 * round trip; every default the trait defines through the tree agrees with `Free`'s own, read in `Eager` */
class TestReflect extends munit.FunSuite {
  import Eager._

  type S = State[Int]

  def counting[M[_, _]](n: Int)(implicit E: Effects[M]): M[S, Int] =
    (1 to n).foldLeft(E.pure[S, Int](0)) { (m, _) =>
      E.flatMap(m)((acc: Int) => E.map(E.perform[S, Int](State.Update[Int, Int](s => (s, s + 1))))(s => acc + s))
    }

  test("reify and reflect are a round trip: Free -> Eager -> Free, the same answers") {
    val tree: Int ! S = counting[Free](10)
    val eager: Eager[S, Int] = Effects.reflect[Eager.Rep, S, Int](tree)
    val back: Int ! S = Effects.reify[Eager.Rep, S, Int](eager)
    assertEquals(back.handle(State(0)).run, tree.handle(State(0)).run)
    assertEquals(Effects.convert[Eager.Rep, Free, S, Int](counting[Eager.Rep](10)).handle(State(0)).run, tree.handle(State(0)).run)
  }

  test("depth: 100 000 operations reflected and reified, on the default stack") {
    val tree: Int ! S = counting[Free](100000)
    val back: Int ! S = Effects.reify[Eager.Rep, S, Int](Effects.reflect[Eager.Rep, S, Int](tree))
    assertEquals(back.handle(State(0)).run._2, tree.handle(State(0)).run._2)
  }

  test("handle(m)(ret)(h): the definition through the tree agrees with Free's loop") {
    // a row's `#Op` is its last parent's in Scala 2, so a two-effect program is built as a tree and reflected
    val tree: Int ! (Throws[String] + S) =
      State.update[Int, Int](s => (s, s + 5)).plus[Throws[String]].flatMap(s =>
        if (s > 3) Throws.raise[String, Int]("big").plus[S] else pure[Throws[String] + S, Int](s))
    def raising[M[_, _]](implicit E: Effects[M]): M[Throws[String] + S, Int] = Effects.reflect[M, Throws[String] + S, Int](tree)
    def h[M[_, _]](implicit E: Effects[M]): Throws[String] !> M[S, Either[String, Int]] = new Interpr[Throws[String], M[S, Either[String, Int]]] {
      def apply[X](e: Throws.Op[String, X]): Cont[X, M[S, Either[String, Int]], M[S, Either[String, Int]]] = e match {
        case Throws.Raise(err) => Cont.shift[X, M[S, Either[String, Int]], M[S, Either[String, Int]]](_ => E.pure[S, Either[String, Int]](Left(err)))
      }
    }
    val viaFree = Effects.free.handle[Throws[String], S, Int, Either[String, Int]](raising[Free])(a => pure[S, Either[String, Int]](Right(a)))(h[Free])
    val E = Effects[Eager.Rep]
    val viaEager = E.handle[Throws[String], S, Int, Either[String, Int]](raising[Eager.Rep])(a => E.pure[S, Either[String, Int]](Right(a)))(h[Eager.Rep])
    for (s0 <- List(0, 7))
      assertEquals(Effects.reify[Eager.Rep, S, Either[String, Int]](viaEager).handle(State(s0)).run, viaFree.handle(State(s0)).run)
  }

  test("level 1 in any encoding: shift, reset, handle(m, h), run — Eager agrees with Free") {
    def dF[M[_, _]](implicit E: Effects[M]): M[Pure, Int] =
      E.reset[Int, Pure](E.map(E.shift[Int, Int, Pure](k => E.flatMap(k(1))(a => E.map(k(10))(b => a + b))))(_ * 2))
    assertEquals(Effects.free.run(dF[Free]), 22)
    assertEquals(Effects[Eager.Rep].run(dF[Eager.Rep]), 22)
    def exit0[M[_, _]](implicit E: Effects[M]): M[Pure, Int] =
      E.reset[Int, Pure](E.map(E.shift0[Int, Int, Pure](_ => E.pure[Pure, Int](42)))(_ + 1))
    assertEquals(Effects[Eager.Rep].run(exit0[Eager.Rep]), 42)
    val E = Effects[Eager.Rep]
    val handled = E.handle[Int, S, Any, Handler.Pair[Int]#L, Handler.Nothing, Pure](counting[Eager.Rep](3), State(0))
    assertEquals(E.run(handled), Effects.free.run(counting[Free](3).handle(State(0))))
  }
}

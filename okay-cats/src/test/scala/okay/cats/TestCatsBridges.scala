package okay.cats

import _root_.cats.data.{Chain, NonEmptyList}

/**
 * FromCats: any cats instance as okay's class (specs/interop-classes.md).
 * Its own file because the bridge is its own import — and must not
 * meet ToCats in one scope.
 */
class TestFromCats extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  import FromCats.given

  test("okay.traverse over cats' NonEmptyList and Chain, through the bridge") {
    // NonEmptyList's monad is cartesian: each choice of each element
    val out = okay.traverse(Seq(1, 2))(i => NonEmptyList.of(i, i * 10))
    assertEquals(out.toList, List(Seq(1, 2), Seq(1, 20), Seq(10, 2), Seq(10, 20)))
    assertEquals(okay.sequence(Seq(Chain(1), Chain(2))).toList, List(Seq(1, 2)))
  }

  test("precedence: cats Monad + Alternative answers okay's MonadPlus") {
    val M = summon[okay.MonadPlus[List]]
    assertEquals(M.append(List(1))(List(2)), List(1, 2))
    assertEquals(okay.guard[List](false)(using M), Nil)
  }

  test("a cats Applicative that is not a Monad stays one: cats' Validated accumulates through the bridge") {
    type V[A] = _root_.cats.data.ValidatedNel[String, A]
    val A = FromCats.applicative[V]
    val out = okay.traverse(Seq(1, 2, 3))(i =>
      if i == 2 then _root_.cats.data.Validated.validNel(i) else _root_.cats.data.Validated.invalidNel(s"$i"))(using A)
    assertEquals(out, _root_.cats.data.Validated.Invalid(NonEmptyList.of("1", "3")))
  }
}

/** ToCats: any okay instance as cats' class */
class TestToCats extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  import ToCats.given
  import _root_.cats.syntax.all.*

  /** a carrier only okay has an instance for: an applicative that
   * COUNTS its leaves, the static analysis a monad could not offer */
  final case class Count[A](n: Int, a: A)
  given okay.Applicative[Count] with
    def pure[A](a: A): Count[A] = Count(0, a)
    extension [A, B](f: Count[A => B])
      def app(a: Count[A]): Count[B] = Count(f.n + a.n, f.a(a.a))

  test("cats' traverse over a carrier only okay knows") {
    val out = List(1, 2, 3).traverse(i => Count(1, i * 2))
    assertEquals(out, Count(3, List(2, 4, 6)))
  }

  test("cats' Alternative from okay's: LazyList's MonadPlus") {
    import okay.given
    val A = summon[_root_.cats.Alternative[LazyList]]
    assertEquals(A.combineK(LazyList(1), LazyList(2)).toList, List(1, 2))
    assertEquals(A.empty[Int].toList, Nil)
  }
}

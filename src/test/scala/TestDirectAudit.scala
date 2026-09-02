package okay

import Direct.*

/** the 2026-09-02 audit's macro candidates: a local def after a
 * marked val, and a var bound from a mark then reassigned */
class TestDirectAudit extends munit.FunSuite {
  given Monad[Option] with
    override def pure[A](a: A): Option[A] = Some(a)
    extension [A](m: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] = m.flatMap(f)

  test("a local def sees a marked val bound before it") {
    val r: Option[Int] = direct[Option] {
      val n = Some(20).reflect
      def twice = n * 2
      twice + 2
    }
    assertEquals(r, Some(42))
  }

  test("a var bound from a mark can be reassigned afterwards") {
    val r: Option[Int] = direct[Option] {
      var n = Some(1).reflect
      n = n + 41
      n
    }
    assertEquals(r, Some(42))
  }

  test("argument order: a pure argument before a marked one runs first, and once under multi-shot") {
    given Monad[List] with
      override def pure[A](a: A): List[A] = List(a)
      extension [A](m: List[A])
        override def flatMap[B](f: A => List[B]): List[B] = m.flatMap(f)
    var log = ""
    def g(a: Int, b: Int): Int = a + b
    val r: List[Int] = direct[List] {
      g({ log += "a"; 1 }, List(1, 2, 3).reflect)
    }
    assertEquals(r, List(2, 3, 4))
    assertEquals(log, "a")
    // and a pure argument AFTER the last mark still runs per continuation
    var after = ""
    val r2: List[Int] = direct[List] {
      g(List(1, 2).reflect, { after += "b"; 10 })
    }
    assertEquals(r2, List(11, 12))
    assertEquals(after, "bb")
  }

  test("a loop receiver is forced lazily: an infinite LazyList under a short-circuiting monad") {
    var seen = 0
    val r: Option[Unit] = direct[Option] {
      for i <- LazyList.from(1) do
        seen = i
        (if i < 5 then Some(()) else None).reflect
    }
    assertEquals(r, None)
    assertEquals(seen, 5)
  }

  test("a var bound from a mark, reassigned from another mark") {
    val r: Option[Int] = direct[Option] {
      var n = Some(1).reflect
      n = Some(n + 41).reflect
      n
    }
    assertEquals(r, Some(42))
  }
}

class TestConditionAudit extends munit.FunSuite {
  import Condition.*
  test("100k consecutive resumes do not grow the stack") {
    def loop(i: Int, acc: Int): Int ! Op =
      if i == 0 then pure(acc)
      else signal[Int](i).flatMap(v => loop(i - 1, acc + v))
    val out = !.run(Condition.run[Int, Pure]((_, _) => Decision.Resume(1))(loop(100000, 0)))
    assertEquals(out, 100000)
  }
}

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

  test("a var bound from a mark, reassigned from another mark") {
    val r: Option[Int] = direct[Option] {
      var n = Some(1).reflect
      n = Some(n + 41).reflect
      n
    }
    assertEquals(r, Some(42))
  }
}

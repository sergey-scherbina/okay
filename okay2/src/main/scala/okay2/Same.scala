package okay2

/**
 * Sameness of TYPED tokens, with the type equality it implies: for a
 * key constructor K, `same(a: K[A], b: K[B])` answers whether a and b
 * are one and the same key — and when they are, hands over the witness
 * `A =:= B`, because one key holds one type. The structure that asks
 * (the `Delim` machine cutting its prompt stack, `Facts` keyed by
 * `Fact`) applies the witness and never casts.
 */
trait Same[K[_]] {
  def same[A, B](a: K[A], b: K[B]): Option[A =:= B]
}

object Same {
  def apply[K[_]](implicit s: Same[K]): Same[K] = s

  /** the axiom for reference tokens: identity. The one place a `=:=`
   * is CLAIMED rather than derived — a typed token that IS another
   * typed token has that token's type — stated once, as a witness,
   * for every key type that opts in */
  def byIdentity[K[X] <: AnyRef]: Same[K] = new Same[K] {
    def same[A, B](a: K[A], b: K[B]): Option[A =:= B] =
      if (a eq b) Some(implicitly[A =:= A].asInstanceOf[A =:= B]) else None
  }

  /** the two polymorphic functions `byValue` takes — traits, since
   * Scala 2 has no polymorphic function values */
  trait Equal[K[_]] { def apply[A, B](a: K[A], b: K[B]): Boolean }
  trait TagOf[K[_]] { def apply[A](a: K[A]): scala.reflect.ClassTag[A] }

  /**
   * The axiom for VALUE keys — a typed id over a primitive, say
   * `Id[A](n: Long)`. Equal values alone cannot witness A =:= B:
   * `Id[User](5)` and `Id[Order](5)` are equal numbers and different
   * keys. So a value key carries a runtime TAG of its type, and "the
   * same key" is "equal value AND equal tag". The tag is a ClassTag:
   * exact for a non-generic A, erased for a generic one.
   */
  def byValue[K[_]](equal: Equal[K], tag: TagOf[K]): Same[K] = new Same[K] {
    def same[A, B](a: K[A], b: K[B]): Option[A =:= B] =
      if (equal(a, b) && tag(a) == tag(b)) Some(implicitly[A =:= A].asInstanceOf[A =:= B]) else None
  }
}

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
}

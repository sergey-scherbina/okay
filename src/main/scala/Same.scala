package okay

/**
 * Sameness of TYPED tokens, with the type equality it implies: for a
 * key constructor K, `same(a: K[A], b: K[B])` answers whether a and b
 * are one and the same key — and when they are, hands over the
 * witness `A =:= B`, because one key holds one type. This is the
 * typeclass behind every heterogeneous structure keyed by tokens
 * (TMap first): the structure applies the witness and never casts.
 *
 * Scala 3's own equality is `CanEqual[L, R]` — under
 * `strictEquality`, `==` between two types compiles only with that
 * evidence, but the evidence proves nothing about the types. `Same`
 * is the proof-bearing sibling: from a `Same[K]` a `CanEqual[K[A],
 * K[B]]` follows (two keys of one constructor may always be asked
 * "the same?"), so token keys compare with `==` in strict mode too.
 */
trait Same[K[_]]:
  def same[A, B](a: K[A], b: K[B]): Option[A =:= B]

object Same:
  def apply[K[_]](using s: Same[K]): Same[K] = s

  /** the axiom for reference tokens: identity. The one place a
   * `=:=` is CLAIMED rather than derived — a typed token that IS
   * another typed token has that token's type — stated once, as a
   * witness, for every key type that opts in */
  def byIdentity[K[X] <: AnyRef]: Same[K] = new Same[K]:
    def same[A, B](a: K[A], b: K[B]): Option[A =:= B] =
      if a eq b then Some(summon[A =:= A].asInstanceOf[A =:= B]) else None


  /** the axiom for VALUE keys — a typed id over a primitive, say
   * `Id[A](n: Long)`. Equal values alone cannot witness A =:= B:
   * `Id[User](5)` and `Id[Order](5)` are equal numbers and different
   * keys. So a value key must carry a runtime TAG of its type, and
   * "the same key" is "equal value AND equal tag". The tag is a
   * ClassTag: exact for non-generic A, erased for a generic one
   * (`Id[List[Int]]` and `Id[List[String]]` share a tag) — so value
   * keys are for concrete types, stated here and in the test */
  def byValue[K[_]](equal: [A, B] => (K[A], K[B]) => Boolean,
                    tag: [A] => K[A] => scala.reflect.ClassTag[A]): Same[K] = new Same[K]:
    def same[A, B](a: K[A], b: K[B]): Option[A =:= B] =
      if equal(a, b) && tag(a) == tag(b) then Some(summon[A =:= A].asInstanceOf[A =:= B]) else None

/** the witness, if b is this key. `===` is the operator the stack
 * needs for typed tokens: not a Boolean but the PROOF — in the
 * `Some(ev)` branch the compiler knows A is B and `ev(x: A): B`
 * converts; `=!=` is the Boolean "not the same key". A plain
 * `==` stays what it is (`equals`, permitted under strictEquality by
 * the CanEqual below) for the places that want only a yes or no */
extension [K[_], A](a: K[A])(using s: Same[K])
  def sameAs[B](b: K[B]): Option[A =:= B] = s.same(a, b)
  infix def ===[B](b: K[B]): Option[A =:= B] = s.same(a, b)
  infix def =!=[B](b: K[B]): Boolean = s.same(a, b).isEmpty

/** strict equality for token keys: two keys of one constructor may
 * always be compared — `Same` decides, `==` may ask. Top-level in the
 * package (like the stack's other givens: `import okay.given`) so it
 * is in scope where keys are compared, not only where Same is named */
given sameCanEqual[K[_], A, B](using Same[K]): CanEqual[K[A], K[B]] = CanEqual.derived

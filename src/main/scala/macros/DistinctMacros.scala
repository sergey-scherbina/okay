package okay
package macros

import scala.annotation.tailrec
import scala.quoted.*

/** the macro behind `Distinct.derive` (okay-macros-package) */
@scala.annotation.publicInBinary private[okay] object DistinctMacros:

  /** public because an inline given's splice reaches it from outside
   * (E192, "unstable inline accessor"), exactly as `TypeableK.derived`
   * reaches `derivedImpl` */
  def impl[R[+_] : Type](using Quotes): Expr[Distinct[R]] =
    check[R]
    '{ Distinct.unchecked[R]() }

  private def check[R[+_] : Type](using q: Quotes): Unit =
    import q.reflect.*

    val any = TypeRepr.of[Any]
    val tagSym = Symbol.requiredClass("okay.Tag")
    val instSym = Symbol.requiredClass("okay.Instances")
    // TypeableK and its marker are higher-kinded, so they are reached
    // by symbol: `TypeRepr.of[TypeableK]` wants the argument applied
    val testRef = Symbol.requiredClass("okay.TypeableK").typeRef
    val byValueRef = Symbol.requiredClass("okay.TypeableK.ByValue").typeRef

    /** a member's RUNTIME identity: what its test actually looks at */
    enum Id:
      /** the test is the erasure — two of these collide */
      case Cls(sym: Symbol)
      /** `Tag.Of[K, F]`: the key first, then F's own identity */
      case Keyed(key: String, inner: Id)
      /** `Instances.Of[F]`: a handle at run time, F's identity under it */
      case Inst(inner: Id)
      /** a test that reads the value, or a member nothing can be said
       * about — either way it collides with nothing */
      case Alone

    import Id.*

    /**
     * THE ROW IS READ APPLIED, and that is the whole trick.
     *
     * A row is `[A] =>> F[A] | G[A]` — `+` is an alias for a type
     * LAMBDA, so how `TypeRepr.of[R]` arrives depends on how the call
     * site spelled it: sometimes the applied alias, sometimes the
     * lambda it reduces to. Matching the unapplied shape therefore
     * works at some call sites and silently fails at others, which is
     * exactly what the first cut of this file did — it found no
     * members, no members collide, and every row passed.
     *
     * Applying to `Any` normalises all of it: only a union's body is
     * an `OrType`, whatever the spelling, and `%` (two arguments, like
     * `+`) is not one — `(Writer % Int)[Any]` is a `Writer[Int,
     * Any]`.
     */
    def body(r: TypeRepr): TypeRepr = r.dealias match
      case tl: TypeLambda => tl.resType.dealias
      case other          => other.appliedTo(any).dealias

    def leaves(t: TypeRepr): List[TypeRepr] = t.dealias match
      case OrType(l, r) => leaves(l) ++ leaves(r)
      case leaf         => List(leaf)

    /**
     * The constructor back out of an applied leaf, which is what
     * `TypeableK` is indexed by. The dump says the two shapes that
     * arrive: `(Reader % Int)[A]`, whose single argument is the
     * lambda's own parameter and whose CONSTRUCTOR is the whole
     * tycon; and `Reader[Int, A]`, where the answer type has to be
     * abstracted again.
     */
    def ctorOf(leaf: TypeRepr): Option[TypeRepr] = leaf match
      case AppliedType(tc, List(_))            => Some(tc)
      case AppliedType(tc, args) if args.sizeIs > 1 =>
        Some(TypeLambda(List("A"), _ => List(TypeBounds.empty),
                        tl => AppliedType(tc, args.init :+ tl.param(0))))
      case _ => None

    /** does the member's own `TypeableK` read the operation's value? */
    def byValue(leaf: TypeRepr): Boolean = ctorOf(leaf).exists: c =>
      Implicits.search(testRef.appliedTo(c)) match
        case s: ImplicitSearchSuccess => s.tree.tpe <:< byValueRef.appliedTo(c)
        case _                        => false

    /**
     * `baseType` rather than a structural match on the tycon: a
     * member arrives as `Tag.Of["a", F][A]` — an applied ALIAS, not
     * an applied `Tag` — and asking for its base type at the class
     * does the reduction the pattern cannot.
     */
    def identityOf(leaf0: TypeRepr): Id =
      val leaf = leaf0.dealias
      if leaf =:= TypeRepr.of[Nothing] then Alone
      else
        leaf.baseType(tagSym) match
          case AppliedType(_, List(k, f, _)) =>
            k.dealias match
              case ConstantType(c) => Keyed(c.value.toString, under(f))
              case _               => Alone // a key that is not a literal
          case _ =>
            leaf.baseType(instSym) match
              case AppliedType(_, List(f, _)) => Inst(under(f))
              case _ =>
                leaf.classSymbol match
                  // Any and Object are what an ABSTRACT member's
                  // bound answers; neither is an identity
                  case Some(s) if s != defn.AnyClass && s != defn.ObjectClass =>
                    if byValue(leaf) then Alone else Cls(s)
                  case _ => Alone

    /** the identity of a signature carried as a CONSTRUCTOR — what
     * `Tag` and `Instances` hold in their first parameter */
    def under(f: TypeRepr): Id = identityOf(body(f))

    @tailrec def collide(a: Id, b: Id): Boolean = (a, b) match
      case (Cls(x), Cls(y))           => x == y
      case (Keyed(k, x), Keyed(l, y)) => k == l && collide(x, y)
      case (Inst(x), Inst(y))         => collide(x, y)
      case _                          => false

    val ms = leaves(body(TypeRepr.of[R]))
    val ids = ms.map(identityOf)
    if sys.env.contains("OKAY_DISTINCT_DEBUG") then
      report.info(
        s"row  = ${TypeRepr.of[R].show}\n" +
        s"shape= ${TypeRepr.of[R].getClass.getSimpleName}\n" +
        s"mem  = ${ms.map(_.show).mkString(" | ")}\n" +
        s"ids  = ${ids.mkString(" | ")}")

    for
      i <- ms.indices
      j <- (i + 1) until ms.length
      if collide(ids(i), ids(j))
    do
      report.errorAndAbort(
        s"${ms(i).show} and ${ms(j).show} cannot be told apart in one row.\n" +
        "Both are tested by the ERASURE of the signature, so the first handler in the\n" +
        "row answers the second one's operations too, and the second continuation gets\n" +
        "a ClassCastException at its first wrong answer (TestRowIdentity demonstrates\n" +
        "it at run time; this is the same thing, earlier).\n" +
        "\n" +
        "Give the instances an identity the split can see — a key with Tag, a handle\n" +
        "with Instances, a cell with Refs — or take them OFF the row: with Lexical\n" +
        "each instance is an installation the body names, so nothing is split by class\n" +
        "(Lexical.State(s0) { s => ... }). docs/many-instances.md chooses between them.\n" +
        "\n" +
        "If this signature's own TypeableK reads the operation's VALUE and CAN tell\n" +
        "them apart — as Writer's does — say so in its declared type:\n" +
        "  given yourK[S](using ...): TypeableK.ByValue[Your % S] = ...")

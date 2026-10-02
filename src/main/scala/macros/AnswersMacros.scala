package okay
package macros

import scala.quoted.*

/** the macros behind `derives Effect` / `TypeableK.derived` and `Answers.flat` (okay-macros-package) */
@scala.annotation.publicInBinary private[okay] object AnswersMacros:

  /**
   * The check is the reason this is a macro and not one line.
   *
   * A `ClassTag` of a UNION is its LUB, and a LUB is useless as a
   * test: measured, `ClassTag[(Choose + Writer % String)[Any]]` is
   * `interface java.io.Serializable` and `ClassTag[(Db + Writer %
   * String)[Any]]` is `interface scala.reflect.Enum` — classes every
   * operation in the program matches. A row derived this way would
   * send every operation left and say nothing, which is the failure
   * mode this library refuses on principle.
   *
   * A blacklist of such classes is whack-a-mole (the two above are
   * already different). The type says it exactly: refuse a union,
   * accept a signature. And a row does not need this anyway — the
   * generic instance below handles a composite row correctly, by
   * testing the parts.
   */
  def derivedImpl[F[_] : Type](using Quotes): Expr[Effect[F]] =
    import quotes.reflect.*
    val body = TypeRepr.of[F].dealias match
      case tl: TypeLambda => tl.resType.dealias
      case other => other.appliedTo(TypeRepr.of[Any]).dealias
    body match
      case OrType(_, _) =>
        report.errorAndAbort(
          "TypeableK.derived is for ONE signature, and this is a row.\n" +
          "The erasure of a union is its LUB, a class every operation matches, so the\n" +
          "split would send all of them left and say nothing.\n" +
          "A row needs no instance of its own: let each signature derive one, and the\n" +
          "row split will find them.")
      case _ =>
        // the signature's class with every argument a wildcard — what
        // `x.isInstanceOf[Users[?]]` tests. Emitted as a class of its
        // own per `derives` site (one per signature) so that the test
        // is a CONSTANT-class `instanceof` in the bytecode, where
        // `ByClass` reads its class from a field and calls
        // `Class.isInstance` (typeablek-instanceof: the residual of
        // handler-fusion-flat, 5.6% on a lane that is nothing but
        // dispatch). `ByClass` stays for `typeableK(cls)`, whose class
        // is a run-time value.
        val erased = body match
          case AppliedType(tycon, args) => AppliedType(tycon, args.map(_ => TypeBounds.empty))
          case other => other
        if !erased.typeSymbol.isClassDef then
          report.errorAndAbort(s"TypeableK.derived needs a class to test for, and ${erased.show} is not one")
        erased.asType match
          case '[t] => '{ new Effect[F] { def test(x: Any): Boolean = x.isInstanceOf[t] } }

  /** public because an inline def's splice reaches it from outside
   * (E192, "unstable inline accessor"), as `Distinct.impl` */
  def flatImpl[R[+_] : Type](using q: Quotes): Expr[Answers[R]] =
    import q.reflect.*

    def members(t: TypeRepr): List[TypeRepr] = t.dealias match
      case OrType(l, r) => members(l) ++ members(r)
      case m => List(m)

    /** `F[Any]` back to `F`: the constructor itself when `Any` is its
     * only argument, a lambda over the last argument otherwise —
     * `(Writer % W)[Any]` is `Writer[W, Any]`, `Tag.Of[K, F][Any]` is
     * `Tag[K, F, Any]` */
    def constructor(m: TypeRepr): TypeRepr = m.dealias match
      case AppliedType(tc, args) if args.nonEmpty && args.last =:= TypeRepr.of[Any] =>
        if args.size == 1 then tc
        else TypeLambda(List("A"), _ => List(TypeBounds.empty),
          tl => AppliedType(tc, args.init :+ tl.param(0)))
      case other =>
        report.errorAndAbort(s"Answers.flat: ${other.show} is not an effect signature applied to Any")

    val parts = members(TypeRepr.of[R[Any]]).map(constructor)
    if parts.sizeIs < 2 then
      report.errorAndAbort(s"Answers.flat: ${TypeRepr.of[R].show} is not a row (one member — use its Answers directly)")

    /** a member, its handler and (all but the last) its test, BOUND to
     * vals outside the handler object so that each is evaluated once
     * and captured as a field — the first cut spliced the givens
     * straight into `handle`, and a `given x: T = …` in a class body
     * is a lazy val, so every operation paid its accessor: measured no
     * faster than the nested chain it replaced (inline4 106.5 µs
     * against union4 109.1, and SLOWER at position 1) */
    type Bound = (TypeRepr, Term, Option[Term])

    /**
     * THE ONE CAST, emitted once per member: `split`'s claim, made at
     * the same kind of site. The test that guards the branch proves
     * the operation is this member's; for the last member, every
     * other test having failed proves it. No other cast is emitted.
     */
    def chain[A: Type](a: Expr[R[A]], bs: List[Bound]): Expr[A] = bs match
      case (m, hT, tO) :: rest => m.asType match
        case '[type f[x]; f] =>
          val h = hT.asExprOf[Answers[f]]
          tO match
            case None => '{ $h.handle($a.asInstanceOf[f[A]]) }
            case Some(tT) =>
              val t = tT.asExprOf[TypeableK[f]]
              '{ if $t.test($a) then $h.handle($a.asInstanceOf[f[A]]) else ${ chain[A](a, rest) } }
      case Nil => report.errorAndAbort("Answers.flat: empty row")

    def build(ms: List[TypeRepr], bound: List[Bound]): Expr[Answers[R]] = ms match
      case m :: rest => m.asType match
        case '[type f[x]; f] =>
          val h = Expr.summon[Answers[f]].getOrElse(
            report.errorAndAbort(s"Answers.flat: no Answers[${m.show}] in scope"))
          if rest.isEmpty then
            '{ val hv: Answers[f] = $h; ${ build(Nil, bound :+ (m, 'hv.asTerm, None)) } }
          else
            val t = Expr.summon[TypeableK[f]].getOrElse(
              report.errorAndAbort(s"Answers.flat: no TypeableK[${m.show}] in scope"))
            '{ val hv: Answers[f] = $h; val tv: TypeableK[f] = $t
               ${ build(rest, bound :+ (m, 'hv.asTerm, Some('tv.asTerm))) } }
      case Nil =>
        '{ new Answers[R] { def handle[A](a: R[A]): A = ${ chain[A]('a, bound) } } }

    build(parts, Nil)

package okay.freer
package macros

import okay.{Answers, TypeableK}
import scala.quoted.*

/** the macro behind `Row.flat`: a union row's handler as ONE dispatch (handler-fusion-flat) */
@scala.annotation.publicInBinary private[okay] object FlatMacros:

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
        report.errorAndAbort(s"Row.flat: ${other.show} is not an effect signature applied to Any")

    val parts = members(TypeRepr.of[R[Any]]).map(constructor)
    if parts.sizeIs < 2 then
      report.errorAndAbort(s"Row.flat: ${TypeRepr.of[R].show} is not a row (one member — use its Answers directly)")

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
      case Nil => report.errorAndAbort("Row.flat: empty row")

    def build(ms: List[TypeRepr], bound: List[Bound]): Expr[Answers[R]] = ms match
      case m :: rest => m.asType match
        case '[type f[x]; f] =>
          val h = Expr.summon[Answers[f]].getOrElse(
            report.errorAndAbort(s"Row.flat: no Answers[${m.show}] in scope"))
          if rest.isEmpty then
            '{ val hv: Answers[f] = $h; ${ build(Nil, bound :+ (m, 'hv.asTerm, None)) } }
          else
            val t = Expr.summon[TypeableK[f]].getOrElse(
              report.errorAndAbort(s"Row.flat: no TypeableK[${m.show}] in scope"))
            '{ val hv: Answers[f] = $h; val tv: TypeableK[f] = $t
               ${ build(rest, bound :+ (m, 'hv.asTerm, Some('tv.asTerm))) } }
      case Nil =>
        '{ new Answers[R] { def handle[A](a: R[A]): A = ${ chain[A]('a, bound) } } }

    build(parts, Nil)

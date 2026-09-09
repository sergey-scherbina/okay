package okay

import scala.quoted.*
import scala.deriving.Mirror

/**
 * `Lens[S](_.field)`: the field selector as CODE, not a string
 * (specs/optics.md). A macro that reads ONLY the selector's tree —
 * the policy of specs/codecs.md restated: a macro may read what the
 * compiler already wrote, never write. The lambda itself is the
 * getter, so there is no cast anywhere and the focus type is the type
 * checker's, not a match type's; the setter is built from the Mirror.
 * Anything that is not `_.f` is refused at compile time with a
 * message; a field that does not exist is refused by the type checker
 * before this macro ever runs, with its own "did you mean".
 */
final class Focus[S <: Product]:
  inline def apply[A](inline get: S => A)(using m: Mirror.ProductOf[S]): Lens[S, S, A, A] =
    ${ Focus.impl[S, A]('get, 'm) }

object Focus:
  def impl[S <: Product: Type, A: Type](get: Expr[S => A], m: Expr[Mirror.ProductOf[S]])(using Quotes): Expr[Lens[S, S, A, A]] =
    import quotes.reflect.*
    def selector(t: Term): Option[String] = t match
      case Inlined(_, _, inner) => selector(inner)
      case Block(Nil, inner) => selector(inner)
      case Typed(inner, _) => selector(inner)
      case Lambda(List(param), Select(Ident(p), f)) if p == param.name => Some(f)
      case _ => None
    val name = selector(get.asTerm).getOrElse(
      report.errorAndAbort(s"Lens[${Type.show[S]}] wants a field selector, `_.field`; got: ${get.asTerm.show}"))
    val fields = TypeRepr.of[S].typeSymbol.caseFields
    val idx = fields.indexWhere(_.name == name)
    if idx < 0 then
      report.errorAndAbort(s"$name is not a case field of ${Type.show[S]}; the fields are ${fields.map(_.name).mkString(", ")}")
    // the setter is `s.copy(f1, .., a, .., fn)` — every field positional,
    // the selected one replaced — which is what a hand-written lens costs;
    // the Mirror's fromProduct was measured at 12x that (specs/optics.md)
    def copyWith(s: Expr[S], a: Expr[A]): Expr[S] =
      val args = fields.zipWithIndex.map((f, j) => if j == idx then a.asTerm else Select(s.asTerm, f))
      val copy = Select.unique(s.asTerm, "copy")
      val applied = TypeRepr.of[S] match
        case AppliedType(_, targs) => TypeApply(copy, targs.map(t => Inferred(t)))
        case _ => copy
      Apply(applied, args).asExprOf[S]
    val _ = m
    '{ Lens[S, S, A, A]($get, (s, a) => ${ copyWith('s, 'a) }) }

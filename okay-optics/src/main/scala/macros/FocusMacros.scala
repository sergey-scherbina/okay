package okay
package macros

import scala.quoted.*
import scala.deriving.Mirror
import scala.annotation.tailrec

/** the macro behind `Focus` (`Lens[S](_.field)`), okay-macros-package */
@scala.annotation.publicInBinary private[okay] object FocusMacros:
  /**
   * The bound stays on the CLASS, where the caller writes it, and is
   * gone from here on purpose: `Fuse.plan` calls this function to
   * expand a selector-built lens itself, and the types it recovers
   * from a tree are abstract, so a `<: Product` here would have cost
   * a cast to satisfy. Nothing in the body needs it — `caseFields`
   * and `copy` are read off the symbol.
   */
  def impl[S: Type, A: Type](get: Expr[S => A], m: Expr[Mirror.ProductOf[S]])(using Quotes): Expr[Lens[S, S, A, A]] =
    import quotes.reflect.*
    @tailrec def selector(t: Term): Option[String] = t match
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

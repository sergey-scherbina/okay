package okay2

import scala.reflect.macros.{blackbox, whitebox}

/**
 * The two field constructors as Scala 2 macros — the core's `Focus`
 * (a Scala 3 macro reading the selector) and `Lens.field` (the Mirror
 * and a match type there). Both READ what the compiler already typed
 * and write a hand-written lens: the getter is the selector, the setter
 * `s.copy(f = a)`, which is what a hand-written lens costs.
 */
object OpticMacros {

  /** `Lens[S](_.f)` */
  def focus[S: c.WeakTypeTag, A: c.WeakTypeTag](c: blackbox.Context)(get: c.Expr[S => A]): c.Expr[Optic.Lens[S, S, A, A]] = {
    import c.universe._
    val S = weakTypeOf[S]
    val A = weakTypeOf[A]
    def selector(t: Tree): Option[TermName] = t match {
      case Function(List(param), Select(Ident(p), f)) if p == param.name => Some(f.toTermName)
      case Typed(inner, _) => selector(inner)
      case Block(Nil, inner) => selector(inner)
      case _ => None
    }
    val name = selector(get.tree).getOrElse(
      c.abort(c.enclosingPosition, s"Lens[$S] wants a field selector, `_.field`; got: ${show(get.tree)}"))
    val fields = S.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m.name.toTermName }.toList
    if (!fields.contains(name))
      c.abort(c.enclosingPosition, s"${name.decodedName} is not a case field of $S; the fields are ${fields.map(_.decodedName).mkString(", ")}")
    c.Expr[Optic.Lens[S, S, A, A]](
      q"_root_.okay2.Optic.Lens.apply[$S, $S, $A, $A]($get, (s: $S, a: $A) => s.copy($name = a))")
  }

  /** `Lens.field[S]("name")`: whitebox, so the answer carries the
   * field's own type */
  def field[S: c.WeakTypeTag](c: whitebox.Context)(name: c.Expr[String]): c.Tree = {
    import c.universe._
    val S = weakTypeOf[S]
    val n = name.tree match {
      case Literal(Constant(s: String)) => s
      case other => c.abort(other.pos, "Lens.field wants the field's name as a literal")
    }
    val accessor = S.decls.collectFirst { case m: MethodSymbol if m.isCaseAccessor && m.name.decodedName.toString == n => m }
      .getOrElse(c.abort(c.enclosingPosition, s"$n is not a case field of $S"))
    val A = accessor.typeSignatureIn(S).finalResultType
    val f = TermName(n).encodedName.toTermName
    q"_root_.okay2.Optic.Lens.apply[$S, $S, $A, $A]((s: $S) => s.$f, (s: $S, a: $A) => s.copy($f = a))"
  }

  /** `TypedZipper.field("name")`: the prefix is the `FieldOps` view,
   * whose type arguments say S, A and Z */
  def zipperField(c: whitebox.Context)(name: c.Expr[String]): c.Tree = {
    import c.universe._
    val (s, a, z) = c.prefix.actualType.typeArgs match {
      case List(s0, a0, z0) => (s0, a0, z0)
      case other => c.abort(c.enclosingPosition, s"field: expected the view's three type arguments, got $other")
    }
    val n = name.tree match {
      case Literal(Constant(v: String)) => v
      case other => c.abort(other.pos, "field wants the field's name as a literal")
    }
    val accessor = a.decls.collectFirst { case m: MethodSymbol if m.isCaseAccessor && m.name.decodedName.toString == n => m }
      .getOrElse(c.abort(c.enclosingPosition, s"$n is not a case field of $a"))
    val e = accessor.typeSignatureIn(a).finalResultType
    val f = TermName(n).encodedName.toTermName
    val zz = TermName(c.freshName("z"))
    q"""{
      val $zz = ${c.prefix.tree}.z
      _root_.okay2.TypedZipper.Below[$s, $a, $e, $z]($zz, (p: $a) => _root_.scala.util.Right[$a, $e](p.$f), (p: $a, v: $e) => p.copy($f = v), $zz.focus.$f, false, _root_.scala.Some($n))
    }"""
  }
}

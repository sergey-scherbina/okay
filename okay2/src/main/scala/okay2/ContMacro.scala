package okay2

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

/**
 * THE MACRO BEHIND `Cont.shift` (cont-stack-okay2-macro, Layer 1 A of specs/cont-stack.md in Scala 2; the Scala 3
 * core's `okay.macros.ContMacro`, its tail case). A shift whose body only calls its continuation in TAIL position,
 * with an argument free of it — through blocks, `if` and `match`, a `throw` in a branch allowed — IS that argument
 * evaluated when the runner reaches the shift: it is rewritten to `tailShift(() => v)` (`tailPure(v)` for a
 * literal), which the runner walks in its own loop with no frame and no room counted. Sound because the body has
 * nothing left to do after a tail call; `S <:< R`, summoned here, says its answer is the shift's. Every other
 * body, and a function passed as a value, is the leaf as before (`shiftLeaf`).
 */
object ContMacro {
  def shift[A: c.WeakTypeTag, S: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(f: c.Tree): c.Tree = {
    import c.universe._
    val (a, s, r) = (weakTypeOf[A], weakTypeOf[S], weakTypeOf[R])
    val prefix = c.prefix.tree
    def leaf: Tree = q"$prefix.shiftLeaf[$a, $s, $r]($f)"

    /** the literal lambda's parameter and body, under typing wrappers */
    @tailrec def lambda(t: Tree): Option[(Symbol, Tree)] = t match {
      case Function(List(p), body) => Some((p.symbol, body))
      case Typed(e, _) => lambda(e)
      case Block(Nil, e) => lambda(e)
      case _ => None
    }

    def mentions(k: Symbol, t: Tree): Boolean = t.exists(_.symbol == k)

    /** the value a tail-shaped body answers through `k`, or None; bounded by the body's own nesting, which the
     * compiler has already walked */
    def tailValue(k: Symbol, t: Tree): Option[Tree] = t match {
      case Apply(Select(id: Ident, TermName("apply")), List(v)) if id.symbol == k && !mentions(k, v) => Some(v)
      case Typed(e, _) => tailValue(k, e)
      case Block(stats, e) if !stats.exists(mentions(k, _)) => tailValue(k, e).map(v => Block(stats, v))
      case If(cond, th, el) if !mentions(k, cond) =>
        for (t1 <- tailValue(k, th); e1 <- tailValue(k, el)) yield If(cond, t1, e1)
      case Match(sel, cases) if !mentions(k, sel) && !cases.exists(cd => mentions(k, cd.guard)) =>
        val bodies = cases.map(cd => tailValue(k, cd.body))
        if (bodies.forall(_.isDefined)) Some(Match(sel, cases.zip(bodies).map { case (cd, b) => CaseDef(cd.pat, cd.guard, b.get) }))
        else None
      case Throw(e) if !mentions(k, e) => Some(t)
      case _ => None
    }

    lambda(f).flatMap { case (k, body) => tailValue(k, body) } match {
      case Some(v) =>
        val ev = c.inferImplicitValue(appliedType(typeOf[<:<[Any, Any]].typeConstructor, List(s, r)), silent = true)
        if (ev.isEmpty) leaf
        else v match {
          case lit: Literal => q"$prefix.tailPure[$a, $s, $r]($lit)($ev)"
          case _ => q"$prefix.tailShift[$a, $s, $r](() => ${c.untypecheck(v)})($ev)"
        }
      case None => leaf
    }
  }
}

package okay2

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

/**
 * THE MACRO BEHIND `Cont.shift` (cont-stack-okay2-macro, Layer 1 A of specs/cont-stack.md in Scala 2; the Scala 3
 * core's `okay.macros.ContMacro`, its tail case). A shift whose body only calls its continuation in TAIL position,
 * with an argument free of it — through blocks, `if` and `match`, a `throw` in a branch allowed — IS that argument
 * evaluated when the runner reaches the shift: it is rewritten to `tailShift(() => v)` (`tailPure(v)` for a
 * literal), which the runner walks in its own loop with no frame and no room counted. Sound because the body has
 * nothing left to do after a tail call; `S <:< R`, summoned here, says its answer is the shift's.
 *
 * LAYER 1 B (the Scala 3 core's cont-stack-layer1-b): a body that USES `k`'s answer — `k(1) + k(10)`, `a :: k(x)`,
 * `val y = k(x); y + 1`, a tail `if`/`match` with `k` in its branches — is CPS-transformed SELECTIVELY (Rompf, Maier
 * & Odersky, ICFP 2009) into a `ContCps.Cps` whose body is DATA (`Call`/`Done`) the runner walks in its own loop. A
 * `k`-free part evaluated ahead of a call is bound to a `val` first (A-normal form), so the order of effects is
 * kept. What the transform does not read — `k` as a value, in a by-name argument, a lambda, `try`, `while`, a
 * `var`, an assignment, a non-tail `if`/`match` — leaves the whole body the leaf as before (`shiftLeaf`).
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

    /** the body as a walked one (Layer 1 B), or None when the transform cannot read it */
    def walked(k: Symbol, body: Tree): Option[Tree] = {
      object Opaque extends Exception
      val kName = k.name.toTermName
      def plain(t: Tree): Tree = c.untypecheck(t)
      def kCall(t: Tree): Option[Tree] = t match {
        case Apply(Select(id: Ident, TermName("apply")), List(e)) if id.symbol == k => Some(e)
        case _ => None
      }
      def byName(fun: Tree): List[Boolean] = fun.tpe match {
        case mt: MethodType => mt.params.map(_.asTerm.isByNameParam)
        case _ => Nil
      }
      // `kont` builds the rest of the body from the value tree; bounded by the body's own nesting, which the
      // compiler has already walked
      def cps(t: Tree, tail: Boolean)(kont: Tree => Tree): Tree =
        if (!mentions(k, t)) kont(plain(t))
        else kCall(t) match {
          case Some(e) =>
            cps(e, tail = false) { ev =>
              val x = TermName(c.freshName("ans"))
              q"_root_.okay2.ContCps.Call[$a, $s, $r](${Ident(kName)}, $ev, ($x: $s) => ${kont(Ident(x))})"
            }
          case None => t match {
            case Typed(e, _) => cps(e, tail)(kont)
            case Block(stats, e) => block(stats, e, tail)(kont)
            case If(cond, th, el) if tail =>
              cps(cond, tail = false)(cv => q"if ($cv) ${cps(th, tail = true)(kont)} else ${cps(el, tail = true)(kont)}")
            case Match(sel, cases) if tail && !cases.exists(cd => mentions(k, cd.guard)) =>
              cps(sel, tail = false) { sv =>
                Match(sv, cases.map(cd => CaseDef(plain(cd.pat), plain(cd.guard), cps(cd.body, tail = true)(kont))))
              }
            case Apply(fun, args) =>
              val names = byName(fun)
              if (args.zipWithIndex.exists { case (arg, i) => names.lift(i).getOrElse(false) && mentions(k, arg) }) throw Opaque
              part(fun)(f2 => argList(args, Nil)(as => kont(Apply(f2, as))))
            case Select(qual, name) => cps(qual, tail = false)(qv => kont(Select(qv, name)))
            case TypeApply(fun, targs) => cps(fun, tail = false)(f2 => kont(TypeApply(f2, targs.map(plain))))
            case _ => throw Opaque
          }
        }
      // an application's function part: its qualifier evaluated first, as the call would
      def part(fun: Tree)(kont: Tree => Tree): Tree = fun match {
        case Select(qual, name) if mentions(k, qual) => cps(qual, tail = false)(qv => kont(Select(qv, name)))
        case TypeApply(Select(qual, name), targs) if mentions(k, qual) =>
          cps(qual, tail = false)(qv => kont(TypeApply(Select(qv, name), targs.map(plain))))
        case other if !mentions(k, other) => kont(plain(other))
        case _ => throw Opaque
      }
      // the arguments in order; a k-free one ahead of a call of k is bound to a val first, so it runs before it
      def argList(args: List[Tree], done: List[Tree])(kont: List[Tree] => Tree): Tree = args match {
        case Nil => kont(done.reverse)
        case arg :: rest =>
          if (mentions(k, arg)) cps(arg, tail = false)(v => argList(rest, v :: done)(kont))
          else if (rest.exists(mentions(k, _))) {
            val t = TermName(c.freshName("arg"))
            q"{ val $t = ${plain(arg)}; ${argList(rest, Ident(t) :: done)(kont)} }"
          } else argList(rest, plain(arg) :: done)(kont)
      }
      def block(stats: List[Tree], e: Tree, tail: Boolean)(kont: Tree => Tree): Tree = stats match {
        case Nil => cps(e, tail)(kont)
        case (v @ ValDef(mods, name, _, rhs)) :: rest if mentions(k, rhs) =>
          if (mods.hasFlag(Flag.MUTABLE)) throw Opaque
          cps(rhs, tail = false)(rv => q"{ val $name: ${TypeTree(v.symbol.info)} = $rv; ${block(rest, e, tail)(kont)} }")
        case st :: rest if !mentions(k, st) => q"{ ${plain(st)}; ${block(rest, e, tail)(kont)} }"
        case (_: Assign | _: ValDef) :: _ => throw Opaque
        case st :: rest => cps(st, tail = false)(sv => q"{ $sv; ${block(rest, e, tail)(kont)} }")
      }
      // `untypecheck` resets only what a fragment DEFINES: a fragment's reference to a body-local val (`y` in
      // `y + 1`, defined in another fragment) would keep the old symbol, whose definition the transform re-emits —
      // so every reference to a body-local symbol becomes its bare name again, bound by the re-emitted definition
      val locals = body.collect { case d: DefTree if d.symbol != NoSymbol => d.symbol }.toSet + k
      object rebind extends Transformer {
        override def transform(t: Tree): Tree = t match {
          case id: Ident if id.symbol != null && locals.contains(id.symbol) => Ident(id.name)
          case _ => super.transform(t)
        }
      }
      try {
        val walk = rebind.transform(cps(body, tail = true)(v => q"_root_.okay2.ContCps.Done[$r]($v)"))
        Some(q"""$prefix.cps[$a, $s, $r](new _root_.okay2.ContCps.Cps[$a, $s, $r] {
          def body($kName: _root_.scala.Function1[$a, $s]): _root_.okay2.ContCps.Body[$r] = $walk
        })""")
      } catch { case Opaque => None }
    }

    lambda(f) match {
      case Some((k, body)) => tailValue(k, body) match {
        case Some(v) =>
          val ev = c.inferImplicitValue(appliedType(typeOf[<:<[Any, Any]].typeConstructor, List(s, r)), silent = true)
          if (ev.isEmpty) walked(k, body).getOrElse(leaf)
          else v match {
            case lit: Literal => q"$prefix.tailPure[$a, $s, $r]($lit)($ev)"
            case _ => q"$prefix.tailShift[$a, $s, $r](() => ${c.untypecheck(v)})($ev)"
          }
        case None => if (mentions(k, body)) walked(k, body).getOrElse(leaf) else leaf
      }
      case None => leaf
    }
  }
}

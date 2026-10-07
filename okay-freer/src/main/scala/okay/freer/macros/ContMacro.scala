package okay.freer
package macros


import scala.quoted.*

/**
 * `shift`'s compile-time layer, an optimization over the one leaf (specs/cont-stack.md Layer 1):
 * a body calling `k` only in tail position is the value it passes (`tailShift`/`tailPure`); a body using
 * `k`'s answer is CPS-transformed selectively (Rompf, Maier & Odersky, ICFP 2009) into a program over a lazy
 * `k` (`lazyLeaf`); anything else stays the opaque leaf (`shiftLeaf`). Public for the expansions; not an API.
 */
@scala.annotation.publicInBinary private[okay] object ContMacro:

  /** the transform's "cannot read this" — caught once, at the top */
  private object Opaque extends scala.util.control.ControlThrowable

  def shift[A: Type, S: Type, R: Type](f: Expr[(A => S) => R], scope: Expr[Cps.Shifts])(using q: Quotes): Expr[Cps[A, S, R]] =
    import q.reflect.*

    /** the scope's compile-time choice (cont-safe-mode): `Cps.safe` — every body using `k` transformed or refused;
     * `Cps.noReplay` — an opaque body kept, never re-executed */
    val safeScope: Boolean = scope.asTerm.tpe.widen <:< TypeRepr.of[Cps.Safe]
    val onceScope: Boolean = safeScope || scope.asTerm.tpe.widen <:< TypeRepr.of[Cps.NoReplay]

    def fallback: Expr[Cps[A, S, R]] =
      if onceScope then '{ Cps.shiftLeafOnce[A, S, R]($f) } else '{ Cps.shiftLeaf[A, S, R]($f) }

    /** a body a safe scope cannot take: it would wait for `k` on the host stack, or be re-executed */
    def refuse(why: String): Nothing =
      report.errorAndAbort(
        s"""Cps safe mode (import okay.freer.Cps.safe.given): this shift body cannot be transformed — $why.
           |At run time it would wait for k on the host stack (or be re-executed). Call k directly in the body
           |(k(x), its answer used as a value: the macro reads that), or answer a program (its k is lazy), or take
           |this body out of the safe scope (import okay.freer.Cps.noReplay.given keeps it, never re-executed).""".stripMargin,
        f.asTerm.pos)

    /** does `t` mention `k` anywhere */
    def mentions(k: Symbol, t: Tree): Boolean =
      new TreeAccumulator[Boolean]:
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match
            case i: Ident if i.symbol == k => true
            case _ => foldOverTree(false, tree)(owner))
      .foldTree(false, t)(Symbol.spliceOwner)

    /** does `t` CALL `k` (`k(v)`, `k.apply(v)`) while it runs — outside any lambda or local method in it. A call
     * inside one (`produce(w).flatMap(_ => k(()))`, a `foldM` step) happens later, from whoever runs the program
     * the body answers, and never nests (cont-js-depth's census); only a call made by the body itself waits */
    def calls(k: Symbol, t: Tree): Boolean =
      new TreeAccumulator[Boolean]:
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match
            case Apply(i: Ident, _) if i.symbol == k => true
            case Apply(Select(i: Ident, "apply"), _) if i.symbol == k => true
            case _: DefDef => false
            case _ => foldOverTree(false, tree)(owner))
      .foldTree(false, t)(Symbol.spliceOwner)

    /** an opaque body that calls `k` and answers a PROGRAM gets the lazy `k` (cont-program-answer): its `k(a)`
     * is a lazy run, never a nested one; a body that only passes `k` on keeps the strict leaf, at no cost */
    def opaque(p: Symbol, body: Term): Expr[Cps[A, S, R]] =
      // a safe scope takes a program answer's lazy `k` whether the body calls `k` or passes it on; nothing else
      if safeScope then programLeaf.getOrElse(refuse("it uses k in a form the transform does not read (k passed to a function, a k-using guard, a lazy val, ...)"))
      else if calls(p, body) then programLeaf.getOrElse(fallback) else fallback

    /** `S` a program that can defer itself (`Cps.Later`: any `Freer`, an `A ! F` among them) */
    def programLeaf: Option[Expr[Cps[A, S, R]]] =
      Expr.summon[Cps.Later[S]].map(p => '{ Cps.programLeaf[A, S, R]($f)(using $p) })

    /** `k(v)` / `k.apply(v)`, with `v` free of `k` */
    object TailCall:
      def unapply(t: Term)(using k: Symbol): Option[Term] = t match
        case Apply(Select(i: Ident, "apply"), List(v)) if i.symbol == k && !mentions(k, v) => Some(v)
        case Apply(i: Ident, List(v)) if i.symbol == k && !mentions(k, v) => Some(v)
        case _ => None

    /** the body with every tail `k(v)` replaced by `v`, or None when the
     * body is not tail-shaped. `throw` in a tail position is kept: it
     * answers nothing, so it needs no `k`. */
    def rewrite(t: Term)(using k: Symbol): Option[Term] = t match
      case TailCall(v) => Some(v)
      case Inlined(call, bindings, e) if !bindings.exists(mentions(k, _)) =>
        rewrite(e).map(Inlined.copy(t)(call, bindings, _))
      case Typed(e, _) => rewrite(e)
      case Block(stats, e) if !stats.exists(mentions(k, _)) =>
        rewrite(e).map(Block.copy(t)(stats, _))
      case If(c, a, b) if !mentions(k, c) =>
        for a2 <- rewrite(a); b2 <- rewrite(b) yield If.copy(t)(c, a2, b2)
      case Match(s, cases) if !mentions(k, s) && !cases.exists(c => c.guard.exists(mentions(k, _))) =>
        val rhs = cases.map(c => rewrite(c.rhs))
        if rhs.forall(_.isDefined) then
          Some(Match.copy(t)(s, cases.zip(rhs).map((c, r) => CaseDef.copy(c)(c.pattern, c.guard, r.get))))
        else None
      case _ if t.tpe <:< TypeRepr.of[Nothing] && !mentions(k, t) => Some(t) // a throw
      case _ => None

    /** a value that can be read eagerly without changing when anything
     * happens: a literal, or a stable name */
    def eager(t: Term): Boolean = t match
      case Literal(_) => true
      case i: Ident => i.symbol.isValDef && !i.symbol.flags.is(Flags.Mutable) && !i.symbol.flags.is(Flags.Lazy)
      case Inlined(_, Nil, e) => eager(e)
      case Typed(e, _) => eager(e)
      case _ => false

    // ---- Layer 1 B: the selective CPS transform ----

    /** what is left to do with a value: `Done` at answer type `rt`
     * (the tail), or a rest that builds the body from it */
    case class Kont(rt: TypeRepr, rest: Option[Term => Term])

    def done(rt: TypeRepr, v: Term): Term = rt.asType match
      case '[r] => '{ Cps.done[R, r](${ v.asExprOf[r] }) }.asTerm

    def feed(kont: Kont, v: Term): Term = kont.rest match
      case None => done(kont.rt, v)
      case Some(f) => f(v)

    /** a fresh `name => rest(name)` of type `in => Lazy[rt]` */
    def lam(name: String, in: TypeRepr, rt: TypeRepr)(rest: Term => Term): Term =
      val out = rt.asType match
        case '[r] => TypeRepr.of[Cps.Lazy[R, r]]
      Lambda(Symbol.spliceOwner, MethodType(List(name))(_ => List(in), _ => out),
        (meth, ps) => rest(Ref(ps.head.symbol)).changeOwner(meth))

    /** `t` with every `from` replaced by `to` */
    def subst(t: Term, from: Symbol, to: Term): Term =
      new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
          case i: Ident if i.symbol == from => to
          case _ => super.transformTerm(tree)(owner)
      .transformTerm(t)(Symbol.spliceOwner)

    /** a stable, effect-free term: evaluating it has no order to keep */
    def trivial(t: Term): Boolean = t match
      case Literal(_) | This(_) | New(_) | Super(_, _) => true
      case i: Ident =>
        val fl = i.symbol.flags
        !(fl.is(Flags.Method) || fl.is(Flags.Mutable) || fl.is(Flags.Lazy))
      case Select(q, _) =>
        val fl = t.symbol.flags
        trivial(q) && (fl.is(Flags.Module) || fl.is(Flags.Package) || (t.symbol.isValDef && !fl.is(Flags.Mutable) && !fl.is(Flags.Lazy)))
      case Typed(e, _) => trivial(e)
      case Lambda(_, _) => true
      case _ => false

    var fresh = 0
    /** a k-free value: in place if trivial, else bound to a val FIRST,
     * so it still runs before the calls of `k` that follow it */
    def value(t: Term, kont: Kont): Term =
      if trivial(t) then feed(kont, t)
      else
        fresh += 1
        val sym = Symbol.newVal(Symbol.spliceOwner, s"cps$$$fresh", t.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
        Block(List(ValDef(sym, Some(t.changeOwner(sym)))), feed(kont, Ref(sym)))

    /** `k(e)` / `k.apply(e)`: the argument, which may mention `k` itself */
    object KCall:
      def unapply(t: Term)(using k: Symbol): Option[Term] = t match
        case Apply(Select(i: Ident, "apply"), List(e)) if i.symbol == k => Some(e)
        case Apply(i: Ident, List(e)) if i.symbol == k => Some(e)
        case _ => None

    /** the lazy `k` a transformed body's calls go to: `cpsBody`'s parameter, typed `Cps.LazyK[A, S]` */
    var lazyK: Option[Expr[Cps.LazyK[A, S]]] = None

    /** `Cps.call(k, e, s => rest)` */
    def call(e: Term, kont: Kont)(using k: Symbol): Term = kont.rt.asType match
      case '[r] =>
        '{ Cps.call[A, S, R, r](${ lazyK.get }, ${ e.asExprOf[A] },
             ${ lam("s", TypeRepr.of[S], kont.rt)(sv => feed(kont, sv)).asExprOf[S => Cps.Lazy[R, r]] }) }.asTerm

    /** the parameter types a function term's arguments are matched against */
    def params(fun: Term): List[TypeRepr] = fun.tpe.widen match
      case MethodType(_, ps, _) => ps
      case _ => Nil

    /** arguments in order; a by-name one stays where it is and may not mention `k` */
    def cpsArgs(args: List[Term], ptypes: List[TypeRepr], rt: TypeRepr)(rest: List[Term] => Term)(using k: Symbol): Term =
      def go(i: Int, acc: List[Term]): Term =
        if i == args.length then rest(acc.reverse)
        else
          val a = args(i)
          val byName = ptypes.lift(i).exists { case ByNameType(_) => true; case _ => false }
          a match
            case _ if byName => if mentions(k, a) then throw Opaque else go(i + 1, a :: acc)
            case NamedArg(n, e) => cps(e, Kont(rt, Some(e2 => go(i + 1, NamedArg(n, e2) :: acc))))
            case Typed(Repeated(elems, tpt), tpt2) =>
              cpsArgs(elems, Nil, rt)(es => go(i + 1, Typed(Repeated(es, tpt), tpt2) :: acc))
            case _ => cps(a, Kont(rt, Some(a2 => go(i + 1, a2 :: acc))))
      go(0, Nil)

    /** `fun` in function position: its qualifier and its (curried)
     * arguments are values, in order; the partial application itself
     * is never bound to a val */
    def cpsFun(fun: Term, kont: Kont)(using k: Symbol): Term = fun match
      case Select(q, n) => cps(q, Kont(kont.rt, Some(q2 => feed(kont, Select.copy(fun)(q2, n)))))
      case TypeApply(f2, targs) => cpsFun(f2, Kont(kont.rt, Some(f3 => feed(kont, TypeApply.copy(fun)(f3, targs)))))
      case Apply(f2, args) =>
        cpsFun(f2, Kont(kont.rt, Some(f3 => cpsArgs(args, params(f2), kont.rt)(as => feed(kont, Apply.copy(fun)(f3, as))))))
      case i: Ident => feed(kont, i)
      case _ if !mentions(k, fun) => value(fun, kont)
      case _ => throw Opaque

    /** a block's statements, the k-free prefix kept as it is */
    def cpsStats(stats: List[Statement], e: Term, kont: Kont)(using k: Symbol): Term =
      val (before, from) = stats.span(st => !mentions(k, st))
      val tail = from match
        case Nil => cps(e, kont)
        case (v @ ValDef(name, tpt, Some(rhs))) :: more
            if !v.symbol.flags.is(Flags.Mutable) && !v.symbol.flags.is(Flags.Lazy) =>
          cps(rhs, Kont(kont.rt, Some(r2 =>
            Block(List(ValDef.copy(v)(name, tpt, Some(r2.changeOwner(v.symbol)))), cpsStats(more, e, kont)))))
        case (st: Term) :: more =>
          cps(st, Kont(kont.rt, Some { s2 =>
            val rest = cpsStats(more, e, kont)
            if trivial(s2) then rest else Block(List(s2), rest)
          }))
        case _ => throw Opaque
      if before.isEmpty then tail else Block(before, tail)

    /**
     * A JOIN POINT (cont-stack-layer1-c (1)): a conditional whose branches call `k`, with something after it.
     * The rest is bound ONCE as a local function `j` (a lambda: binding it runs nothing), and every branch ends
     * in `j(value)` — the rest is not copied into each branch, so nested conditionals stay linear in size. In
     * tail position there is no rest, and the branches end in the body's answer as before.
     */
    def joined(t: Term, kont: Kont)(branches: Kont => Term)(using k: Symbol): Term = kont.rest match
      case None => branches(kont)
      case Some(_) =>
        fresh += 1
        val in = t.tpe.widen
        val j = lam(s"join$$$fresh", in, kont.rt)(v => feed(kont, v))
        val sym = Symbol.newVal(Symbol.spliceOwner, s"join$$$fresh", j.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
        val toJ = Kont(kont.rt, Some(v => Select.unique(Ref(sym), "apply").appliedTo(v)))
        Block(List(ValDef(sym, Some(j.changeOwner(sym)))), branches(toJ))

    /** a lambda term's parameters and body, under any wrapping */
    @scala.annotation.tailrec
    def lambdaOf(t: Term): Option[(List[ValDef], Term)] = t match
      case Lambda(ps, body) => Some((ps, body))
      case Inlined(_, Nil, e) => lambdaOf(e)
      case Typed(e, _) => lambdaOf(e)
      case Block(Nil, e) => lambdaOf(e)
      case _ => None

    /** `k` itself, passed as a function value (under any wrapping), or None */
    @scala.annotation.tailrec
    def kValue(t: Term)(using k: Symbol): Option[Term] = t match
      case i: Ident if i.symbol == k => Some(i)
      case Inlined(_, Nil, e) => kValue(e)
      case Typed(e, _) => kValue(e)
      case _ => None

    /**
     * the element type of an immutable `Iterable` receiver (`Seq`, `Set`, `Map`), or None. A mutable one stays
     * opaque on purpose: the traversal would read it after the body could have changed it.
     */
    def elemOf(q: Term): Option[TypeRepr] =
      val it = TypeRepr.of[scala.collection.immutable.Iterable[Any]].typeSymbol
      q.tpe.widen.baseType(it) match
        case AppliedType(_, List(x)) => Some(x)
        case _ => None

    /**
     * `bs: List[B]` as the type the original call answered (`List`, `Vector`, `Seq`, `Set`, a `Map`'s `Iterable`),
     * or None — a `LazyList`'s lazy `map` among them, which stays opaque
     */
    def asResult(bs: Term, want: TypeRepr, b: TypeRepr): Option[Term] =
      b.asType match
        case '[bt] =>
          val list = bs.asExprOf[List[bt]]
          List(bs, '{ $list.toVector }.asTerm, '{ $list.toSet }.asTerm).find(_.tpe.widen <:< want)

    /**
     * THE KNOWN TRAVERSALS (cont-stack-layer1-c (2)): `xs.map(f)`, `xs.foreach(f)`, `xs.foldLeft(z)(f)` on an
     * immutable `Seq`, `xs` and `z` free of `k`, `f` a lambda whose body calls it — the body a program over the
     * lazy `k`, the traversal `Cps.traverse`/`Cps.foldIn`. Anything else is not one of them (None).
     */
    def traversal(t: Term, kont: Kont)(using k: Symbol): Option[Term] =
      def step(ps: List[ValDef], body: Term, ins: List[TypeRepr], out: TypeRepr): Term =
        val mt = MethodType(ps.map(_.name))(_ => ins, _ => out.asType match { case '[o] => TypeRepr.of[Cps.Lazy[R, o]] })
        Lambda(Symbol.spliceOwner, mt, (meth, args) =>
          cps(ps.zip(args).foldLeft(body)((b, pa) => subst(b, pa._1.symbol, Ref(pa._2.symbol))), Kont(out, None)).changeOwner(meth))
      t match
        case Apply(TypeApply(Select(q, m @ ("map" | "foreach")), List(bt)), List(fn)) if !mentions(k, q) =>
          for
            x <- elemOf(q)
            // the lambda, or `k` itself passed as the function: `xs.map(k)` is `xs.map(x => k(x))`
            g <- lambdaOf(fn).filter((ps, body) => ps.length == 1 && mentions(k, body)).map(Left(_))
                   .orElse(kValue(fn).map(_ => Right(())))
            b = bt.tpe
            r <- if m == "foreach" then Some(None) else Some(Some(t.tpe.widen))
          yield kont.rt.asType match { case '[rt] => x.asType match { case '[xt] => b.asType match { case '[btp] =>
              val f = (g match
                case Left((ps, body)) => step(ps, body, List(x), b)
                case Right(()) => lam("x", x, b)(xv => call(xv, Kont(b, None)))).asExprOf[xt => Cps.Lazy[R, btp]]
              val rest = lam("bs", TypeRepr.of[List[btp]], kont.rt)(bsv =>
                r match
                  case None => feed(kont, '{ () }.asTerm)
                  case Some(want) => asResult(bsv, want, b) match
                    case Some(res) => feed(kont, res)
                    case None => throw Opaque).asExprOf[List[btp] => Cps.Lazy[R, rt]]
              cps(q, Kont(kont.rt, Some(q2 =>
                '{ Cps.traverse[R, xt, btp, rt](${ q2.asExprOf[Iterable[xt]] }, $f, $rest) }.asTerm))) } } }
        // flatMap: the traversal, then the answers flattened (their element type by the evidence the call had)
        case Apply(TypeApply(Select(q, "flatMap"), List(bt)), List(fn)) if !mentions(k, q) =>
          for
            x <- elemOf(q)
            (ps, body) <- lambdaOf(fn) if ps.length == 1 && mentions(k, body)
            c = body.tpe.widen
            res <- (kont.rt.asType, x.asType, c.asType, bt.tpe.asType) match
              case ('[rt], '[xt], '[ct], '[btp]) => Expr.summon[ct <:< IterableOnce[btp]].flatMap { ev =>
                val f = step(ps, body, List(x), c).asExprOf[xt => Cps.Lazy[R, ct]]
                val want = t.tpe.widen
                try
                  val rest = lam("bs", TypeRepr.of[List[ct]], kont.rt)(bsv =>
                    val flat = '{ ${ bsv.asExprOf[List[ct]] }.flatMap(c => $ev(c)) }.asTerm
                    asResult(flat, want, bt.tpe) match
                      case Some(r) => feed(kont, r)
                      case None => throw Opaque).asExprOf[List[ct] => Cps.Lazy[R, rt]]
                  Some(cps(q, Kont(kont.rt, Some(q2 =>
                    '{ Cps.traverse[R, xt, ct, rt](${ q2.asExprOf[Iterable[xt]] }, $f, $rest) }.asTerm))))
                catch case Opaque => None
              }
              case _ => None
          yield res
        // exists / forall / find: stopping at the first element that decides
        case Apply(Select(q, m @ ("exists" | "forall" | "find")), List(fn)) if !mentions(k, q) =>
          for
            x <- elemOf(q)
            (ps, body) <- lambdaOf(fn) if ps.length == 1 && mentions(k, body)
          yield kont.rt.asType match { case '[rt] => x.asType match { case '[xt] =>
            val p = step(ps, body, List(x), TypeRepr.of[Boolean]).asExprOf[xt => Cps.Lazy[R, Boolean]]
            m match
              case "find" =>
                val rest = lam("found", TypeRepr.of[Option[xt]], kont.rt)(v => feed(kont, v)).asExprOf[Option[xt] => Cps.Lazy[R, rt]]
                cps(q, Kont(kont.rt, Some(q2 => '{ Cps.findIn[R, xt, rt](${ q2.asExprOf[Iterable[xt]] }, $p, $rest) }.asTerm)))
              case _ =>
                val want = Expr(m == "exists")
                val rest = lam("holds", TypeRepr.of[Boolean], kont.rt)(v => feed(kont, v)).asExprOf[Boolean => Cps.Lazy[R, rt]]
                cps(q, Kont(kont.rt, Some(q2 => '{ Cps.existsIn[R, xt, rt](${ q2.asExprOf[Iterable[xt]] }, $p, $want, $rest) }.asTerm))) } }
        // foldRight: foldIn over the elements reversed, the lambda's parameters swapped
        case Apply(Apply(TypeApply(Select(q, "foldRight"), List(bt)), List(z)), List(fn)) if !mentions(k, q) && !mentions(k, z) =>
          for
            x <- elemOf(q)
            (ps, body) <- lambdaOf(fn) if ps.length == 2 && mentions(k, body)
          yield kont.rt.asType match { case '[rt] => x.asType match { case '[xt] => bt.tpe.asType match { case '[btp] =>
            val f = step(List(ps(1), ps(0)), body, List(bt.tpe, x), bt.tpe).asExprOf[(btp, xt) => Cps.Lazy[R, btp]]
            val rest = lam("acc", bt.tpe, kont.rt)(av => feed(kont, av)).asExprOf[btp => Cps.Lazy[R, rt]]
            cps(q, Kont(kont.rt, Some(q2 => cps(z, Kont(kont.rt, Some(z2 =>
              '{ Cps.foldIn[R, xt, btp, rt](${ q2.asExprOf[Iterable[xt]] }.toList.reverse, ${ z2.asExprOf[btp] }, $f, $rest) }.asTerm)))))) } } }
        case Apply(Apply(TypeApply(Select(q, "foldLeft"), List(bt)), List(z)), List(fn)) if !mentions(k, q) && !mentions(k, z) =>
          for
            x <- elemOf(q)
            (ps, body) <- lambdaOf(fn) if ps.length == 2 && mentions(k, body)
          yield kont.rt.asType match { case '[rt] => x.asType match { case '[xt] => bt.tpe.asType match { case '[btp] =>
              val f = step(ps, body, List(bt.tpe, x), bt.tpe).asExprOf[(btp, xt) => Cps.Lazy[R, btp]]
              val rest = lam("acc", bt.tpe, kont.rt)(av => feed(kont, av)).asExprOf[btp => Cps.Lazy[R, rt]]
              cps(q, Kont(kont.rt, Some(q2 => cps(z, Kont(kont.rt, Some(z2 =>
                '{ Cps.foldIn[R, xt, btp, rt](${ q2.asExprOf[Iterable[xt]] }, ${ z2.asExprOf[btp] }, $f, $rest) }.asTerm)))))) } } }
        case _ => None

    /**
     * THE KNOWN METHODS (cont-stack-layer1-c (2)): a call whose meaning is fixed, rewritten into a plain `match`
     * or `if` the transform already reads (with its join point) — `Option`'s `getOrElse`, `map`, `flatMap`,
     * `fold`, `orElse`, `Either`'s `fold`, `getOrElse`, `map`, `flatMap`, `Try`'s `getOrElse`, `&&`, `||`. The receiver is evaluated once, as the
     * scrutinee; a by-name argument only in its branch, as the method would. None: not one of them.
     */
    def known(t: Term)(using k: Symbol): Option[Term] =
      /** a one-parameter lambda's body with its parameter replaced by `v`, owned where it is spliced */
      def applied(fn: Term, v: Term): Option[Term] = lambdaOf(fn) match
        case Some((List(p), body)) => Some(subst(body, p.symbol, v).changeOwner(Symbol.spliceOwner))
        case _ => None
      def isOption(q: Term) = q.tpe.widen <:< TypeRepr.of[Option[Any]]
      def isEither(q: Term) = q.tpe.widen <:< TypeRepr.of[Either[Any, Any]]
      def isTry(q: Term) = q.tpe.widen <:< TypeRepr.of[scala.util.Try[Any]]
      // the receiver's element types, read off its own type
      def optionOf(q: Term): Option[TypeRepr] = q.tpe.widen.baseType(TypeRepr.of[Option[Any]].typeSymbol) match
        case AppliedType(_, List(a)) => Some(a)
        case _ => None
      def eitherOf(q: Term): Option[(TypeRepr, TypeRepr)] = q.tpe.widen.baseType(TypeRepr.of[Either[Any, Any]].typeSymbol) match
        case AppliedType(_, List(l, r)) => Some((l, r))
        case _ => None
      def tryOf(q: Term): Option[TypeRepr] = q.tpe.widen.baseType(TypeRepr.of[scala.util.Try[Any]].typeSymbol) match
        case AppliedType(_, List(a)) => Some(a)
        case _ => None
      // each rewrite at the receiver's own element types; `B >: A` (getOrElse, orElse) by the evidence the call
      // already had, summoned here, never a cast
      def here(t: Term): Term = t.changeOwner(Symbol.spliceOwner)
      t match
        // a && b, a || b: b only when it decides
        case Apply(Select(a, "&&"), List(b)) if a.tpe.widen <:< TypeRepr.of[Boolean] =>
          Some(If(a, b, Literal(BooleanConstant(false))))
        case Apply(Select(a, "||"), List(b)) if a.tpe.widen <:< TypeRepr.of[Boolean] =>
          Some(If(a, Literal(BooleanConstant(true)), b))
        case Apply(TypeApply(Select(o, "getOrElse"), List(bt)), List(d)) if isOption(o) =>
          optionOf(o).flatMap(a => a.asType match { case '[at] => bt.tpe.asType match { case '[b] =>
            Expr.summon[at <:< b].map(ev => '{ ${ o.asExprOf[Option[at]] } match
              case Some(x) => $ev(x)
              case None => ${ here(d).asExprOf[b] } }.asTerm) } })
        case Apply(TypeApply(Select(o, "getOrElse"), List(bt)), List(d)) if isEither(o) =>
          eitherOf(o).flatMap((l, r) => l.asType match { case '[lt] => r.asType match { case '[rt] => bt.tpe.asType match { case '[b] =>
            Expr.summon[rt <:< b].map(ev => '{ ${ o.asExprOf[Either[lt, rt]] } match
              case Right(x) => $ev(x)
              case Left(_) => ${ here(d).asExprOf[b] } }.asTerm) } } })
        case Apply(TypeApply(Select(o, "map"), List(bt)), List(fn)) if isOption(o) =>
          optionOf(o).map(a => a.asType match { case '[at] => bt.tpe.asType match { case '[b] =>
            '{ ${ o.asExprOf[Option[at]] } match
              case Some(x) => Some(${ applied(fn, 'x.asTerm).get.asExprOf[b] })
              case None => None }.asTerm } })
        case Apply(TypeApply(Select(o, "flatMap"), List(bt)), List(fn)) if isOption(o) =>
          optionOf(o).map(a => a.asType match { case '[at] => bt.tpe.asType match { case '[b] =>
            '{ ${ o.asExprOf[Option[at]] } match
              case Some(x) => ${ applied(fn, 'x.asTerm).get.asExprOf[Option[b]] }
              case None => None }.asTerm } })
        case Apply(Apply(TypeApply(Select(o, "fold"), List(bt)), List(ifEmpty)), List(fn)) if isOption(o) =>
          optionOf(o).map(a => a.asType match { case '[at] => bt.tpe.asType match { case '[b] =>
            '{ ${ o.asExprOf[Option[at]] } match
              case Some(x) => ${ applied(fn, 'x.asTerm).get.asExprOf[b] }
              case None => ${ here(ifEmpty).asExprOf[b] } }.asTerm } })
        case Apply(TypeApply(Select(o, "orElse"), List(bt)), List(alt)) if isOption(o) =>
          optionOf(o).flatMap(a => a.asType match { case '[at] => bt.tpe.asType match { case '[b] =>
            Expr.summon[at <:< b].map(ev => '{ ${ o.asExprOf[Option[at]] } match
              case Some(x) => Some($ev(x))
              case None => ${ here(alt).asExprOf[Option[b]] } }.asTerm) } })
        case Apply(TypeApply(Select(e, "map"), List(bt)), List(fn)) if isEither(e) =>
          eitherOf(e).map((l, r) => l.asType match { case '[lt] => r.asType match { case '[rt] => bt.tpe.asType match { case '[b] =>
            '{ ${ e.asExprOf[Either[lt, rt]] } match
              case Right(y) => Right(${ applied(fn, 'y.asTerm).get.asExprOf[b] })
              case Left(x) => Left(x) }.asTerm } } })
        case Apply(TypeApply(Select(e, "flatMap"), List(at, bt)), List(fn)) if isEither(e) =>
          eitherOf(e).flatMap((l, r) => l.asType match { case '[lt] => r.asType match { case '[rt] =>
            at.tpe.asType match { case '[a1] => bt.tpe.asType match { case '[b] =>
              Expr.summon[lt <:< a1].map(ev => '{ ${ e.asExprOf[Either[lt, rt]] } match
                case Right(y) => ${ applied(fn, 'y.asTerm).get.asExprOf[Either[a1, b]] }
                case Left(x) => Left($ev(x)) }.asTerm) } } } })
        // Try's getOrElse only: its default is not under a catch. Try(...), map, flatMap, fold, recover catch
        // what their function throws, and with a lazy k that would be the rest — they stay opaque on purpose
        case Apply(TypeApply(Select(o, "getOrElse"), List(ut)), List(d)) if isTry(o) =>
          tryOf(o).flatMap(a => a.asType match { case '[at] => ut.tpe.asType match { case '[u] =>
            Expr.summon[at <:< u].map(ev => '{ ${ o.asExprOf[scala.util.Try[at]] } match
              case scala.util.Success(x) => $ev(x)
              case scala.util.Failure(_) => ${ here(d).asExprOf[u] } }.asTerm) } })
        case Apply(TypeApply(Select(e, "fold"), List(ct)), List(fa, fb)) if isEither(e) =>
          eitherOf(e).map((l, r) => l.asType match { case '[lt] => r.asType match { case '[rt] => ct.tpe.asType match { case '[c] =>
            '{ ${ e.asExprOf[Either[lt, rt]] } match
              case Left(x) => ${ applied(fa, 'x.asTerm).get.asExprOf[c] }
              case Right(y) => ${ applied(fb, 'y.asTerm).get.asExprOf[c] } }.asTerm } } })
        case _ => None

    /** the transform: `t` in value position, `kont` what follows its value */
    def cps(t: Term, kont: Kont)(using k: Symbol): Term =
      if !mentions(k, t) then value(t, kont)
      else t match
        case KCall(e) => cps(e, Kont(kont.rt, Some(e2 => call(e2, kont))))
        case _ if known(t).isDefined => cps(known(t).get, kont)
        case _ if traversal(t, kont).isDefined => traversal(t, kont).get
        case Apply(fun, args) =>
          cpsFun(fun, Kont(kont.rt, Some(f2 => cpsArgs(args, params(fun), kont.rt)(as => feed(kont, Apply.copy(t)(f2, as))))))
        // AN INLINE HELPER (cont-stack-layer1-c (3)): its arguments are bindings. One that IS `k`
        // (`val f$proxy = k`) is an alias, replaced by `k` in the helper's body so its calls read as `k(…)`; the
        // others are vals in their order, as a block's — a call of `k` among them is read like any `val x = k(…)`
        case Inlined(call0, bindings, e) =>
          if !bindings.exists(mentions(k, _)) then Inlined.copy(t)(call0, bindings, cps(e, kont))
          else
            val (aliases, vals) = bindings.partition {
              case v: ValDef => v.rhs.exists(r => kValue(r).isDefined)
              case _ => false
            }
            val body = aliases.foldLeft(e)((acc, b) => subst(acc, b.symbol, Ref(k)))
            Inlined.copy(t)(call0, Nil, cpsStats(vals, body, kont))
        case Typed(e, _) => cps(e, kont)
        // an assignment whose value calls `k` (`v = k(1)`, `seen += k(x)`): the value first, in its own order
        // (a variable read on the right is bound before the call, as the strict road reads it), then the store
        case Assign(lhs, rhs) if !mentions(k, lhs) =>
          cps(rhs, Kont(kont.rt, Some(r2 => feed(kont, Assign.copy(t)(lhs, r2)))))
        case Block(stats, e) => cpsStats(stats, e, kont)
        // A LOOP (cont-stack-layer1-c (5)): `while c do body` with `k` in either, as a local function each of
        // whose iterations is `Cps.later` — a step the machine forces, so iterations that never call `k` hold
        // no host frame. The condition false: the rest after the loop, once, inside the loop function
        case While(c, body) =>
          fresh += 1
          val lazyT = kont.rt.asType match { case '[r] => TypeRepr.of[Cps.Lazy[R, r]] }
          val loop = Symbol.newMethod(Symbol.spliceOwner, s"loop$$$fresh", MethodType(Nil)(_ => Nil, _ => lazyT))
          val again = Apply(Ref(loop), Nil)
          val iteration = cps(c, Kont(kont.rt, Some(c2 =>
            If(c2, cps(body, Kont(kont.rt, Some(_ => again))), feed(kont, '{ () }.asTerm)))))
          val rhs = kont.rt.asType match
            case '[r] => '{ Cps.later[R, r](() => ${ iteration.changeOwner(Symbol.spliceOwner).asExprOf[Cps.Lazy[R, r]] }) }.asTerm
          Block(List(DefDef(loop, _ => Some(rhs.changeOwner(loop)))), again)
        case If(c, a, b) =>
          if mentions(k, a) || mentions(k, b) then
            joined(t, kont)(kb => cps(c, Kont(kont.rt, Some(c2 => If.copy(t)(c2, cps(a, kb), cps(b, kb))))))
          else cps(c, Kont(kont.rt, Some(c2 => feed(kont, If.copy(t)(c2, a, b)))))
        case Match(sc, cases) =>
          if cases.exists(c => c.guard.exists(mentions(k, _))) then throw Opaque
          if cases.exists(c => mentions(k, c.rhs)) then
            joined(t, kont)(kb => cps(sc, Kont(kont.rt, Some(s2 =>
              Match.copy(t)(s2, cases.map(c => CaseDef.copy(c)(c.pattern, c.guard, cps(c.rhs, kb))))))))
          else cps(sc, Kont(kont.rt, Some(s2 => feed(kont, Match.copy(t)(s2, cases)))))
        case _ => throw Opaque

    /** the body as a program over a lazy `k`, or None where the transform cannot read it */
    def cpsBody(body: Term)(using k: Symbol): Option[Expr[Cps.LazyK[A, S] => Cps.Lazy[R, R]]] =
      try
        Some('{ (k2: Cps.LazyK[A, S]) => ${
          lazyK = Some('k2)
          cps(body, Kont(TypeRepr.of[R], None)).changeOwner(Symbol.spliceOwner).asExprOf[Cps.Lazy[R, R]] } })
      catch case Opaque => None

    // a tail body's types say `S <: R`; searched here, where `S` and `R` are concrete. Not found: the body
    // stays a leaf, which is always right
    lazy val tailEvidence: Option[Expr[S <:< R]] = Expr.summon[S <:< R]
    // `S` is `R`: the tail body is a value and nothing else, typed by the equality
    lazy val sameEvidence: Option[Expr[S =:= R]] = Expr.summon[S =:= R]

    f.asTerm.underlyingArgument match
      case Lambda(List(p), body) =>
        given Symbol = p.symbol
        rewrite(body) match
          case Some(v) if eager(v) && sameEvidence.isDefined =>
            '{ Cps.tailPureSame[A, S, R](${ v.asExprOf[A] })(using ${ sameEvidence.get }) }
          case Some(v) if sameEvidence.isDefined =>
            '{ Cps.tailShiftSame[A, S, R](() => ${ v.changeOwner(Symbol.spliceOwner).asExprOf[A] })(using ${ sameEvidence.get }) }
          case Some(v) if eager(v) && tailEvidence.isDefined =>
            '{ Cps.tailPure[A, S, R](${ v.asExprOf[A] })(using ${ tailEvidence.get }) }
          case Some(v) if tailEvidence.isDefined =>
            '{ Cps.tailShift[A, S, R](() => ${ v.changeOwner(Symbol.spliceOwner).asExprOf[A] })(using ${ tailEvidence.get }) }
          case Some(_) => if safeScope then refuse("its k is in tail position but the types give no S <: R to pass the value on") else fallback
          case None if !mentions(p.symbol, body) => fallback
          case None => cpsBody(body) match
            case Some(b) => '{ Cps.lazyLeaf[A, S, R]($b) }
            case None => opaque(p.symbol, body)
      case _ => if safeScope then refuse("it is not a lambda literal, so the macro cannot read it") else fallback

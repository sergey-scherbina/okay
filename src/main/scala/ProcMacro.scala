package okay

import scala.quoted.*

/**
 * THE MACRO BEHIND `Proc.direct` — specs/proc-notation.md.
 *
 * `direct` is not a monad macro: it is a block normaliser whose
 * translation is chosen by the algebra the carrier PROVES (a `Monad`
 * gives flatMap binds, an `Applicative` alone gives the idiom
 * bracket, a `Selective` gives `ifS`). This is the same normalisation
 * at an ARROW, where the question Turner asked of a lambda term,
 * Marlow asked of ApplicativeDo and Paterson asked of `-<` has ONE
 * more consequence: what a monadic body keeps in its closure, an
 * arrow carries on its edge.
 *
 * So the whole macro is that threading. The environment is a
 * LEFT-NESTED TUPLE that starts as the block's input and grows by one
 * at every bound name:
 *
 *     X                      before any name is bound
 *     (X, A1)                after the first
 *     ((X, A1), A2)          after the second
 *
 * A reference to a bound name becomes a projection into it, a leaf
 * becomes `Proc.keeping`, and a run of pure statements becomes one
 * `Arr` (two `Arr`s in a row fold into one at construction, so the
 * nodes of a compiled block are its leaves and the plumbing between
 * them).
 *
 * WHAT IT REFUSES, each by name and with the reason rather than a
 * type error about a class the author never mentioned:
 *
 *   - a mark whose value is a `Proc` — a step chosen by a value the
 *     block binds. That is `app`, and an arrow with `app` is a monad
 *     (Hughes 2000 §4.5); the deploy check, `leaves` and the
 *     exhaustive cut all rest on the spine being known before it runs.
 *   - a mark inside an `if` branch or a loop — they hoist, and a
 *     hoisted mark RUNS whether or not its branch is taken. `OnRight`
 *     and `Iter` are the nodes for those and they are v1.1, named in
 *     the refusal rather than silently mis-compiled.
 *   - a mark under a lambda, and a mark inside another mark — the
 *     same corner `direct`'s v1 refuses, for the same reason.
 *
 * WHAT IT SHARES WITH `Direct.scala`: the marks themselves, and the
 * twenty lines that recognise them. Nothing else — this road is its
 * own file and touches no existing emission, which is what makes the
 * duplication measurable rather than assumed (specs/arrows-plan.md,
 * Decision 3's stage 2 gate).
 */
object ProcMacro:
  // PUBLIC, and not because it is an API: an `inline def` in a public
  // object that reaches a `private[okay]` one makes the compiler
  // generate an unstable inline accessor (E192). The object is the
  // implementation of `Proc.direct` and nothing else should call it.

  def impl[F[+_] : Type, X: Type, Y: Type](block: Expr[X => Y])
                                          (using q: Quotes): Expr[Proc[F, X, Y]] =
    import q.reflect.*

    // ── the marks, recognised exactly as Direct.scala recognises them
    val directSym = TypeRepr.of[Direct.type].typeSymbol
    val markSyms = (directSym.methodMember("reflect") ++ directSym.methodMember("!?")
      ++ directSym.methodMember("?") ++ directSym.methodMember("unary_!")).toSet

    def strip(t: Term): Term = t match
      case Inlined(_, Nil, inner) => strip(inner)
      case Typed(inner, _) => strip(inner)
      case _ => t

    def asMark(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), List(m)) if markSyms(fun.symbol) => Some(m)
      case _ => None

    def hasMark(t: Tree): Boolean =
      var found = false
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          if !found then tree match
            case term: Term if asMark(term).isDefined => found = true
            case _ => super.traverseTree(tree)(owner)
      probe.traverseTree(t)(Symbol.spliceOwner)
      found

    val procSym = TypeRepr.of[Proc[[A] =>> Any, Any, Any]].typeSymbol

    /**
     * The shapes that are refused, checked on a STRAIGHT-LINE
     * expression only: an `if` and a `while` are compiled by their
     * own routines below, and this runs on the pieces of them that
     * are straight-line (a condition, a branch's statements).
     */
    def guard(t: Tree): Unit =
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          case term: Term if asMark(term).isDefined =>
            val arg = asMark(term).get
            if hasMark(arg) then report.errorAndAbort(
              "Proc.direct: a mark inside another mark — bind the inner one to a val first",
              term.pos)
            if arg.tpe.widen.dealias.derivesFrom(procSym) then report.errorAndAbort(
              "Proc.direct: this mark's value is a Proc, so the STEP is chosen by a value " +
                "this block binds — that is `app`, and an arrow with `app` is a monad " +
                "(Hughes 2000, §4.5). A static spine is what the deploy check, `leaves` " +
                "and the exhaustive cut all rest on. Put the choice INSIDE the question " +
                "(`!ask(if c then qa else qb)`), or write this program with `direct` and " +
                "run it as one activity.", term.pos)
          case l: Term if l.isExpr && isLambda(l) && hasMark(l) =>
            report.errorAndAbort(
              "Proc.direct: a mark under a lambda — bind the marked value to a val before it. " +
                "A `for`/`foreach` over a collection is a lambda: write the loop as a `while` " +
                "over a counter, which compiles to `Iter`.",
              l.pos)
          case tree2 @ If(_, th, el) if hasMark(th) || hasMark(el) =>
            report.errorAndAbort(
              "Proc.direct: a mark inside an `if` BRANCH, and this `if` is not the whole of " +
                "a statement. Only a top-level `if` — the whole right-hand side of a val, a " +
                "statement of its own, or the block's answer — compiles to `OnRight`; one " +
                "nested inside a larger expression would have to hoist, and a hoisted mark " +
                "RUNS whether or not its branch is taken. Bind the `if` to a val first.",
              tree2.pos)
          case w @ While(c, _) if hasMark(c) =>
            report.errorAndAbort(
              "Proc.direct: a mark in a `while` CONDITION. The test would have to ask its " +
                "question once per round INSIDE the loop, which `Iter` can express and the " +
                "notation does not yet: ask before the loop and test a bound value.",
              w.pos)
          case _ => super.traverseTree(tree)(owner)
      probe.traverseTree(t)(Symbol.spliceOwner)

    def isLambda(t: Term): Boolean = strip(t) match
      case Block(List(DefDef(_, _, _, _)), Closure(_, _)) => true
      case _ => false

    // ── the block itself
    //
    // LOOKING THROUGH `Inlined` WITH BINDINGS is not optional, and
    // the way it was found is worth keeping: the macro worked at
    // every ordinary call site and produced "takes a lambda" inside
    // `compileErrors`, because an inline argument arrives there
    // wrapped in `Inlined` nodes carrying `$proxy` bindings. That is
    // the same shape Direct.scala records for `asMark`, met from the
    // other side — so the refusal tests, which exist to pin the
    // errors, found a bug in the macro before they could pin anything.
    def asLambda(t: Term): Option[(Symbol, Term)] = t match
      case Inlined(_, _, inner) => asLambda(inner)
      case Typed(inner, _) => asLambda(inner)
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(b))), Closure(_, _)) =>
        Some((p.symbol, b))
      case Block(_, inner) => asLambda(inner)
      case _ => None

    val (param, body) = asLambda(block.asTerm).getOrElse(report.errorAndAbort(
      "Proc.direct takes a lambda from the procedure's input to its answer: " +
        s"`Proc.direct[F, X, Y] { x => ... }` — got ${block.asTerm.show}", block.asTerm.pos))

    val (stats, last) = strip(body) match
      case Block(ss, e) => (ss, e)
      case e => (Nil, e)


    // ── the environment: a left-nested tuple, and where each name lives
    //
    // Index 0 is the block's own parameter, at the bottom; index i is
    // the i-th value appended. At depth d the projection to index i
    // climbs `._1` (d - i) times and then takes `._2` — except index
    // 0, which is all the way down.
    //
    // AN ASSIGNMENT IS A REBUILD, NOT A REMAPPING, and that decision
    // is what keeps this arithmetic true everywhere: `x = e` emits an
    // `Arr` that reconstructs the environment with x's slot replaced,
    // so a name always lives at its own index and the layout never
    // depends on what has been assigned. Nothing mutates; the arrow
    // carries the new value on its edge.
    def project(env: Term, depth: Int, i: Int): Term =
      if i == 0 then (1 to depth).foldLeft(env)((t, _) => Select.unique(t, "_1"))
      else
        val base = (1 to (depth - i)).foldLeft(env)((t, _) => Select.unique(t, "_1"))
        Select.unique(base, "_2")

    // NESTED rather than a pair pattern: `(a.asType, b.asType) match
    // case ('[x], '[y])` is not seen as exhaustive and a wildcard arm
    // would be a lie, so each type is opened on its own — which IS
    // exhaustive, and needs no `@unchecked`.
    def pairT(a: TypeRepr, b: TypeRepr): TypeRepr =
      a.asType match
        case '[x] => b.asType match
          case '[y] => TypeRepr.of[(x, y)]

    def eitherT(a: TypeRepr, b: TypeRepr): TypeRepr =
      a.asType match
        case '[x] => b.asType match
          case '[y] => TypeRepr.of[Either[x, y]]

    def lam(fromT: TypeRepr, toT: TypeRepr)(rhs: (Symbol, Term) => Term): Term =
      val mt = MethodType(List("env"))(_ => List(fromT), _ => toT)
      Lambda(Symbol.spliceOwner, mt, {
        case (owner, List(p: Term)) => rhs(owner, p).changeOwner(owner)
        case (_, other) => report.errorAndAbort(s"Proc.direct: unexpected lambda shape $other")
      })

    /** a pair, built at its own types — `New` plus `<init>` does not
     * work for a tuple, whose constructor wants them applied */
    def pairTerm(aT: TypeRepr, bT: TypeRepr, a: Term, b: Term): Term =
      aT.asType match
        case '[x] => bT.asType match
          case '[y] => '{ (${ a.asExprOf[x] }, ${ b.asExprOf[y] }) }.asTerm

    def leftTerm(aT: TypeRepr, bT: TypeRepr, v: Term): Term =
      aT.asType match
        case '[x] => bT.asType match
          case '[y] => '{ Left[x, y](${ v.asExprOf[x] }) }.asTerm

    def rightTerm(aT: TypeRepr, bT: TypeRepr, v: Term): Term =
      aT.asType match
        case '[x] => bT.asType match
          case '[y] => '{ Right[x, y](${ v.asExprOf[y] }) }.asTerm

    def arrTerm(fromT: TypeRepr, toT: TypeRepr, fn: Term): Term =
      fromT.asType match
        case '[a] => toT.asType match
          case '[b] => '{ Proc.arr[F, a, b](${ fn.asExprOf[a => b] }) }.asTerm

    def opTerm(envT: TypeRepr, vT: TypeRepr, name: String, fn: Term): Term =
      envT.asType match
        case '[e] => vT.asType match
          case '[v] =>
            '{ Proc.keeping[F, e, v](${ Expr(name) })(${ fn.asExprOf[e => F[v]] }) }.asTerm

    /** a sub-procedure's answer, appended to the environment */
    def alongsideTerm(envT: TypeRepr, vT: TypeRepr, p: Term): Term =
      envT.asType match
        case '[e] => vT.asType match
          case '[v] => '{ Proc.alongside[F, e, v](${ p.asExprOf[Proc[F, e, v]] }) }.asTerm

    def onRightTerm(xT: TypeRepr, yT: TypeRepr, cT: TypeRepr, p: Term): Term =
      xT.asType match
        case '[x] => yT.asType match
          case '[y] => cT.asType match
            case '[c] =>
              '{ Proc.onRight[F, x, y, c](${ p.asExprOf[Proc[F, x, y]] }) }.asTerm

    def iterTerm(xT: TypeRepr, yT: TypeRepr, b: Term): Term =
      xT.asType match
        case '[x] => yT.asType match
          case '[y] =>
            '{ Proc.iter[F, x, y](${ b.asExprOf[Proc[F, x, Either[x, y]]] }) }.asTerm

    def thenTerm(aT: TypeRepr, bT: TypeRepr, cT: TypeRepr, f: Term, g: Term): Term =
      aT.asType match
        case '[a] => bT.asType match
          case '[b] => cT.asType match
            case '[c] =>
              '{ Proc.andThen[F, a, b, c](${ f.asExprOf[Proc[F, a, b]] },
                ${ g.asExprOf[Proc[F, b, c]] }) }.asTerm

    /** a leaf's name, taken from what the author called the operation */
    def nameOf(t: Term): String =
      def root(x: Term): String = x match
        case Apply(f, _) => root(f)
        case TypeApply(f, _) => root(f)
        case Inlined(_, Nil, i) => root(i)
        case Select(_, n) if n == "apply" => x.symbol.owner.name.stripSuffix("$")
        case Select(_, n) => n
        case Ident(n) => n
        case _ => "op"
      val n = root(strip(t))
      if n.isEmpty then "op" else s"${n.head.toLower}${n.tail}"

    /** the marks of one expression, in evaluation order */
    def marksOf(t: Term): List[Term] =
      val out = List.newBuilder[Term]
      val tm = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term =
          asMark(tree) match
            case Some(arg) => out += arg; tree
            case None => super.transformTerm(tree)(owner)
      val _ = tm.transformTerm(t)(Symbol.spliceOwner)
      out.result()

    /**
     * The expression with every bound name replaced by its projection
     * and the k-th mark replaced by the projection of the value that
     * mark's leaf appended. The two passes walk with the SAME
     * `TreeMap`, so the k-th mark here is the k-th mark there.
     */
    def rewrite(t: Term, env: Term, depth: Int, idx: Map[Symbol, Int], firstMark: Int): Term =
      var k = 0
      val tm = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term =
          asMark(tree) match
            case Some(_) =>
              val here = project(env, depth, firstMark + k)
              k += 1
              here
            case None => tree match
              case id: Ident if idx.contains(id.symbol) => project(env, depth, idx(id.symbol))
              case _ => super.transformTerm(tree)(owner)
      tm.transformTerm(t)(Symbol.spliceOwner)

    /**
     * WHERE THE COMPILER STANDS: the type of every slot of the
     * environment, and which slot each name lives in. The environment
     * type is derived rather than stored, because it is exactly the
     * left fold of the slots.
     */
    final case class St(slots: Vector[TypeRepr], idx: Map[Symbol, Int]):
      def depth: Int = slots.length - 1
      def envT: TypeRepr = slots.tail.foldLeft(slots.head)(pairT)
      def push(t: TypeRepr): St = St(slots :+ t, idx)
      def bind(sym: Symbol): St = St(slots, idx + (sym -> depth))

    /** compose two steps, or take the second when there is no first */
    def chain(from: TypeRepr, mid: TypeRepr, to: TypeRepr,
              a: Option[Term], b: Term): Term =
      a match
        case None => b
        case Some(f) => thenTerm(from, mid, to, f, b)

    // ── the compiler proper ──────────────────────────────────────────

    /**
     * EVERY MARK OF AN EXPRESSION, AS LEAVES THAT APPEND THEIR
     * ANSWERS — written once, because it has to be, and the reason is
     * an arithmetic that has been got wrong twice.
     *
     * It answers the depth it STARTED at. The k-th mark's value sits
     * at `base + 1 + k`, so a residual rewritten against the depth
     * AFTER the leaves projects every mark one place too high for
     * each mark before it. That was measured once as `r|r` where
     * `l|r` was meant; the second time it was about to be an `if`
     * whose condition asks a question, and the fix that stuck was to
     * stop having two copies of this loop.
     */
    def hoist(t: Term, st0: St): (Int, St, Option[Term]) =
      guard(t)
      val base = st0.depth
      var st = st0
      var acc: Option[Term] = None
      marksOf(t).foreach: arg =>
        val vT = arg.tpe.widen.dealias match
          case AppliedType(_, args) if args.nonEmpty => args.last
          case other => report.errorAndAbort(
            s"Proc.direct: a mark's value must be an operation of this procedure's " +
              s"signature — got ${other.show}", arg.pos)
        val before = st.envT
        val d = st.depth
        val here = st.idx
        val fn = lam(before, TypeRepr.of[F].appliedTo(vT))((_, env) =>
          rewrite(arg, env, d, here, d + 1))
        st = st.push(vT)
        acc = Some(chain(st0.envT, before, st.envT, acc, opTerm(before, vT, nameOf(arg), fn)))
      (base, st, acc)


    /** an `if` whose branches ask questions: both compiled at the SAME
     * environment, joined by two `OnRight`s. `leaves` reports both,
     * and a run asks only the taken one — the over-approximation this
     * whole shape is built on. */
    def compileIf(cond: Term, condBase: Int, th: Term, el: Term,
                  st: St, outT: TypeRepr): Term =
      val e = st.envT
      val eE = eitherT(e, e)
      val sel = arrTerm(e, eE, lam(e, eE): (_, env) =>
        If(rewrite(cond, env, st.depth, st.idx, condBase + 1),
          rightTerm(e, e, env), leftTerm(e, e, env)))
      val thenP = compileValue(th, st, outT)
      val elseP = compileValue(el, st, outT)
      val eV = eitherT(e, outT)
      val vE = eitherT(outT, e)
      val vV = eitherT(outT, outT)
      val s1 = onRightTerm(e, outT, e, thenP)
      val s2 = arrTerm(eV, vE, lam(eV, vE)((_, x) => Select.unique(x, "swap")))
      val s3 = onRightTerm(e, outT, outT, elseP)
      val s4 = arrTerm(vV, outT, lam(vV, outT): (_, x) =>
        outT.asType match
          case '[v] => '{ ${ x.asExprOf[Either[v, v]] }.fold(identity, identity) }.asTerm)
      val a = thenTerm(e, eE, eV, sel, s1)
      val b = thenTerm(e, eV, vE, a, s2)
      val c = thenTerm(e, vE, vV, b, s3)
      thenTerm(e, vV, outT, c, s4)

    /** a `while`: `Iter` with the test in front of the body, and the
     * loop-carried state is the environment itself */
    def compileWhile(cond: Term, body: Term, st: St): Term =
      val e = st.envT
      val eE = eitherT(e, e)
      val test = arrTerm(e, eE, lam(e, eE): (_, env) =>
        If(rewrite(cond, env, st.depth, st.idx, st.depth + 1),
          rightTerm(e, e, env), leftTerm(e, e, env)))
      val bodyP = compileEnvBody(body, st)
      val s1 = onRightTerm(e, e, e, bodyP)
      val s2 = arrTerm(eE, eE, lam(eE, eE)((_, x) => Select.unique(x, "swap")))
      val b = thenTerm(e, eE, eE, thenTerm(e, eE, eE, test, s1), s2)
      iterTerm(e, e, b)

    /** the statements of a block, compiled onto the environment */
    def compileStats(stats: List[Statement], st0: St): (Option[Term], St) =
      var st = st0
      var acc: Option[Term] = None
      def add(step: Term, from: TypeRepr, to: TypeRepr): Unit =
        acc = Some(chain(st0.envT, from, to, acc, step))

      /** the shared `hoist`, with its steps composed onto this block */
      def emitLeaves(t: Term): Int =
        val from = st.envT
        val (base, stAfter, steps) = hoist(t, st)
        steps.foreach(add(_, from, stAfter.envT))
        st = stAfter
        base

      /** rebuild the environment as it was at `target`, with one slot
       * optionally replaced — how an assignment and a loop body both
       * get back to the shape they started in */
      def rebuild(target: St, replace: Option[(Int, Term => Term)]): Unit =
        val from = st.envT
        val d = st.depth
        if st.depth == target.depth && replace.isEmpty then () else
          val fn = lam(from, target.envT): (_, env) =>
            (0 to target.depth).foldLeft(Option.empty[(TypeRepr, Term)]): (soFar, i) =>
              val piece = replace match
                case Some((s, f)) if s == i => f(env)
                case _ => project(env, d, i)
              Some(soFar match
                case None => (target.slots.head, piece)
                case Some((accT, accV)) =>
                  (pairT(accT, target.slots(i)), pairTerm(accT, target.slots(i), accV, piece)))
            .get._2
          add(arrTerm(from, target.envT, fn), from, target.envT)
          st = target

      stats.foreach:
        case vd @ ValDef(_, _, Some(rhs0)) =>
          val rhs = strip(rhs0)
          val vT = vd.symbol.termRef.widen.dealias
          asTopIf(rhs) match
            case Some((c, th, el)) =>
              val condBase = emitLeaves(c)
              val before = st.envT
              val sub = compileIf(c, condBase, th, el, st, vT)
              st = st.push(vT)
              add(alongsideTerm(before, vT, sub), before, st.envT)
            case None =>
              val base = emitLeaves(rhs)
              val before = st.envT
              val d = st.depth
              val here = st.idx
              st = st.push(vT)
              val fn = lam(before, st.envT): (_, env) =>
                pairTerm(before, vT, env, rewrite(rhs, env, d, here, base + 1))
              add(arrTerm(before, st.envT, fn), before, st.envT)
          st = st.bind(vd.symbol)

        case Assign(lhs, rhs0) =>
          val rhs = strip(rhs0)
          val sym = lhs.symbol
          val slot = st.idx.getOrElse(sym, report.errorAndAbort(
            s"Proc.direct: `${sym.name}` is assigned but not bound in this block", lhs.pos))
          val target = st
          val base = emitLeaves(rhs)
          val d = st.depth
          val here = st.idx
          rebuild(target, Some((slot, (env: Term) => rewrite(rhs, env, d, here, base + 1))))

        case w @ While(c, b) =>
          guard(w)
          val before = st.envT
          add(compileWhile(c, strip(b), st), before, before)

        case t: Term =>
          asTopIf(t) match
            case Some((c, th, el)) =>
              val condBase = emitLeaves(c)
              val before = st.envT
              val sub = compileIf(c, condBase, th, el, st, TypeRepr.of[Unit])
              val d = st.depth
              val fn = lam(pairT(before, TypeRepr.of[Unit]), before): (_, env) =>
                Select.unique(env, "_1")
              add(thenTerm(before, pairT(before, TypeRepr.of[Unit]), before,
                alongsideTerm(before, TypeRepr.of[Unit], sub), fn), before, before)
              val _ = d
            case None =>
              val base = emitLeaves(t)
              val before = st.envT
              val d = st.depth
              val here = st.idx
              val fn = lam(before, before): (_, env) =>
                Block(List(rewrite(t, env, d, here, base + 1)), env)
              add(arrTerm(before, before, fn), before, before)

        case other => report.errorAndAbort(
          "Proc.direct: only `val`s, assignments, `while` loops and plain statements can " +
            s"appear in a block — got ${other.show}", other.pos)

      (acc, st)

    /** an `if` that is the WHOLE of a statement — the only shape that
     * compiles to `OnRight`, because one nested in a larger expression
     * would have to hoist and a hoisted mark runs either way */
    def asTopIf(t: Term): Option[(Term, Term, Term)] = strip(t) match
      case If(c, th, el) if hasMark(th) || hasMark(el) => Some((c, th, el))
      case _ => None

    /** a block compiled to a VALUE of the given type — a branch of an
     * `if` is one of these, compiled at the environment the `if` sees */
    def compileValue(t: Term, st: St, outT: TypeRepr): Term =
      val (stats, lastE) = strip(t) match
        case Block(ss, e) => (ss, e)
        case e => (Nil, e)
      val (body, stEnd) = compileStats(stats, st)
      finish(body, stEnd, lastE, st.envT, outT)

    /** the statements are compiled; this adds the answer */
    def finish(body: Option[Term], stEnd: St, lastE: Term,
               fromT: TypeRepr, outT: TypeRepr): Term =
      var st = stEnd
      var acc = body
      def add(step: Term, from: TypeRepr, to: TypeRepr): Unit =
        acc = Some(chain(fromT, from, to, acc, step))
      // an `if` that IS the block's answer is the third top-level
      // position, beside a val's right-hand side and a statement of
      // its own — and the one a first cut forgot, so a block ending
      // on a branch was refused by the guard that exists for nested
      // ones
      asTopIf(lastE) match
        case Some((c, th, el)) =>
          val from = st.envT
          val (condBase, stAfter, steps) = hoist(c, st)
          steps.foreach(add(_, from, stAfter.envT))
          st = stAfter
          add(compileIf(c, condBase, th, el, st, outT), st.envT, outT)
          return acc.get
        case None => ()
      val from0 = st.envT
      val (base, stAfter, steps) = hoist(strip(lastE), st)
      steps.foreach(add(_, from0, stAfter.envT))
      st = stAfter
      val before = st.envT
      val d = st.depth
      val here = st.idx
      val fn = lam(before, outT): (_, env) =>
        rewrite(strip(lastE), env, d, here, base + 1)
      add(arrTerm(before, outT, fn), before, outT)
      acc.get

    /** a loop body: compiled, then put back into the shape it started
     * in, so the environment that goes round is the one that came in */
    def compileEnvBody(t: Term, st0: St): Term =
      val stats = strip(t) match
        case Block(ss, e) => ss :+ e
        case e => List(e)
      val (body, stEnd) = compileStats(stats.filterNot(isUnitLiteral), st0)
      val e0 = st0.envT
      if stEnd.depth == st0.depth then body.getOrElse(arrTerm(e0, e0,
        lam(e0, e0)((_, env) => env)))
      else
        val fn = lam(stEnd.envT, e0): (_, env) =>
          (0 to st0.depth).foldLeft(Option.empty[(TypeRepr, Term)]): (soFar, i) =>
            val piece = project(env, stEnd.depth, i)
            Some(soFar match
              case None => (st0.slots.head, piece)
              case Some((accT, accV)) =>
                (pairT(accT, st0.slots(i)), pairTerm(accT, st0.slots(i), accV, piece)))
          .get._2
        chain(e0, stEnd.envT, e0, body, arrTerm(stEnd.envT, e0, fn))

    def isUnitLiteral(s: Statement): Boolean = s match
      case Literal(UnitConstant()) => true
      case _ => false

    val st0 = St(Vector(TypeRepr.of[X]), Map(param -> 0))
    val (compiled, stEnd) = compileStats(stats, st0)
    finish(compiled, stEnd, last, TypeRepr.of[X], TypeRepr.of[Y])
      .asExprOf[Proc[F, X, Y]]

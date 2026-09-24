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

  def impl[F[+_] : Type, X: Type, Y: Type](block: Expr[Proc.ProcCtx[F] ?=> X => Y])
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

    /**
     * The colouring conversion, recognised the way Direct.scala
     * recognises its own: the typer inserts `procColor(...).apply(q)`
     * where a question stands in an answer's place, and the macro
     * rewrites that call exactly as it rewrites a mark. One dispatch
     * serves both, because both name the same thing — an operation
     * whose answer the block wants.
     */
    val colorSym = TypeRepr.of[Proc.type].typeSymbol.methodMember("procColor").toSet

    def calleeRoot(t: Term): Symbol = t match
      case Apply(f, _) => calleeRoot(f)
      case TypeApply(f, _) => calleeRoot(f)
      case Inlined(_, Nil, inner) => calleeRoot(inner)
      case _ => t.symbol

    def asMark(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), List(m)) if markSyms(fun.symbol) => Some(m)
      case Apply(Select(conv, "apply"), List(x)) if colorSym(calleeRoot(conv)) => Some(x)
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
     * THE ANSWER A MARKED OPERATION PROMISES, and it is NOT simply the
     * last type argument.
     *
     * A signature may be a GADT whose cases carry their own parameter
     * lists: `Wf.Question.Now[Q, A]` extends `Question[Q, A, Long]`, so
     * reading `args.last` off the CASE gives `A` where `Long` is meant
     * — and the block then ascribes its environment slot at the wrong
     * type, which fails at the splice with "Expected type: scala.Long,
     * Actual type: java.lang.String". Measured by a block that named a
     * case directly instead of going through a door whose result type
     * is the parent. So the answer is read off the base type at the
     * SIGNATURE's own symbol, and the plain reading is the fallback for
     * a signature that is not a class at all.
     */
    lazy val sigSym = TypeRepr.of[F].appliedTo(TypeRepr.of[Any]).typeSymbol

    def answerType(t: TypeRepr): Option[TypeRepr] =
      val w = t.widen.dealias
      w.baseType(sigSym) match
        case AppliedType(_, args) if args.nonEmpty => Some(args.last)
        case _ => w match
          case AppliedType(_, args) if args.nonEmpty => Some(args.last)
          case _ => None

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
          case m @ Match(_, cs) if cs.exists(c => hasMark(c.rhs)) =>
            report.errorAndAbort(
              "Proc.direct: a mark inside a `match` CASE, and this `match` is not the whole of " +
                "a statement. Only a top-level `match` — the whole right-hand side of a val, a " +
                "statement of its own, or the block's answer — compiles to its cases' branches; " +
                "one nested inside a larger expression would have to hoist, and a hoisted mark " +
                "RUNS whichever case is taken. Bind the `match` to a val first.",
              m.pos)
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
    //
    // AND THROUGH THE CONTEXT LAMBDA the entry wraps the block in:
    // `Proc.direct { x => … }` arrives as `(ctx: ProcCtx[F]) => (x: X)
    // => …`, because the capability has to be ambient in the body for
    // the colouring conversion to resolve there and nowhere else. The
    // first lambda is the capability's and is never called — taking
    // it for the block's own is how the first cut compiled a
    // procedure whose input was a `ProcCtx`.
    val ctxSym = TypeRepr.of[Proc.ProcCtx[[A] =>> Any]].typeSymbol

    def asLambda(t: Term): Option[(Symbol, Term)] = t match
      case Inlined(_, _, inner) => asLambda(inner)
      case Typed(inner, _) => asLambda(inner)
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(b))), Closure(_, _)) =>
        if p.tpt.tpe.derivesFrom(ctxSym) then asLambda(b) else Some((p.symbol, b))
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
    //
    // THE PROJECTION IS ASCRIBED, and it has to be: `Select.unique(env,
    // "_2")` types as the path-dependent `env._2`, which unifies with
    // a reference type by luck and NOT with a primitive. Measured by a
    // block whose bound value was a `Long`: "Expected type: scala.Long,
    // Actual type: env._2". So every projection is written at the slot
    // type the compiler recorded when the slot was pushed.
    def projectAt(env: Term, depth: Int, i: Int, slots: Vector[TypeRepr]): Term =
      val raw =
        if i == 0 then (1 to depth).foldLeft(env)((t, _) => Select.unique(t, "_1"))
        else
          val base = (1 to (depth - i)).foldLeft(env)((t, _) => Select.unique(t, "_1"))
          Select.unique(base, "_2")
      if i < slots.length then Typed(raw, Inferred(slots(i))) else raw

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
    def rewrite(t: Term, env: Term, depth: Int, idx: Map[Symbol, Int], firstMark: Int,
                slots: Vector[TypeRepr],
                isLeafArg: Boolean = false): Term =
      var k = 0
      val tm = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term =
          asMark(tree) match
            case Some(_) =>
              val here = projectAt(env, depth, firstMark + k, slots)
              k += 1
              here
            case None => tree match
              case id: Ident if idx.contains(id.symbol) =>
                projectAt(env, depth, idx(id.symbol), slots)
              case _ => super.transformTerm(tree)(owner)
      val out = tm.transformTerm(t)(Symbol.spliceOwner)
      // a LEAF's own argument IS a question at its root — that is what
      // makes it a leaf. Only its subterms can hold a stray one.
      noStrayQuestion(out, skipRoot = isLeafArg)
      out

    /** is this term an operation of the block's own signature? */
    def isQuestion(tpe: TypeRepr): Boolean =
      tpe.widen.dealias match
        case a @ AppliedType(_, args) if args.nonEmpty =>
          a <:< TypeRepr.of[F].appliedTo(args.last)
        case _ => false

    /**
     * A QUESTION THAT SURVIVED THE REWRITE IS A QUESTION NOBODY ASKED,
     * and without this check it is the one failure auto-colouring can
     * produce silently.
     *
     * The conversion fires where an ANSWER is expected. `"a" + q` does
     * not expect one — `String.+` takes `Any` — so the question is
     * quietly stringified and the program asks one question where it
     * reads as asking two. Measured the hour colouring landed:
     * `ask("left?") + "|" + ask("right?")` answered `l|Ask(right?)`
     * and asked once.
     *
     * So after rewriting, nothing of the signature's type may remain.
     * A block that genuinely wants an operation AS A VALUE builds it
     * outside, where it is an ordinary value and not a leaf.
     */
    def noStrayQuestion(t: Term, skipRoot: Boolean): Unit =
      // "skip the root" is the FIRST QUESTION, not the first node: a
      // leaf's argument arrives wrapped in `Inlined`/`Typed`, so a
      // positional root check skipped a wrapper and then reported the
      // question under it — which refused every marked val in the
      // repository until the traversal was made to count questions
      // instead of nodes.
      var toSkip = if skipRoot then 1 else 0
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          // a wrapper carries its content's type, so counting it as a
          // question spends the skip on nothing and reports the real
          // one underneath — which is what it did
          case Inlined(_, _, _) | Typed(_, _) => super.traverseTree(tree)(owner)
          case term: Term if term.isExpr && isQuestion(term.tpe) =>
            if toSkip > 0 then
              toSkip -= 1
              super.traverseTree(tree)(owner)
            else report.errorAndAbort(
              "Proc.direct: this question is never asked — it stands where ANY value is " +
                "accepted, so nothing asked for its answer and auto-colouring had nothing " +
                "to fire on. Three shapes reach this message: `\"a\" + q` and an " +
                "interpolation (String takes Any), a `println`, and a question ALONE ON A " +
                "LINE — which reads like `!q` and is not, because colouring fires where an " +
                "ANSWER is expected and a statement expects nothing. MARK IT: `!q`. Or " +
                "ascribe what you want (`val a: String = q`). If you really meant the " +
                "operation as a VALUE, build it outside the block.", term.pos)
          case _ => super.traverseTree(tree)(owner)
      probe.traverseTree(t)(Symbol.spliceOwner)

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
        val vT = answerType(arg.tpe).getOrElse(report.errorAndAbort(
          s"Proc.direct: a mark's value must be an operation of this procedure's " +
            s"signature — got ${arg.tpe.widen.dealias.show}", arg.pos))
        val before = st.envT
        val d = st.depth
        val here = st.idx
        val slots0 = st.slots
        val fn = lam(before, TypeRepr.of[F].appliedTo(vT))((_, env) =>
          rewrite(arg, env, d, here, d + 1, slots0, isLeafArg = true))
        st = st.push(vT)
        acc = Some(chain(st0.envT, before, st.envT, acc, opTerm(before, vT, nameOf(arg), fn)))
      (base, st, acc)


    /** an `if` whose branches ask questions: both compiled at the SAME
     * environment, joined by two `OnRight`s. `leaves` reports both,
     * and a run asks only the taken one — the over-approximation this
     * whole shape is built on. */
    def compileIf(cond: Term, condBase: Int, th: Term, el: Term,
                  st: St, outT: TypeRepr): Term =
      noOuterAssign(th, st)
      noOuterAssign(el, st)
      val e = st.envT
      val eE = eitherT(e, e)
      val sel = arrTerm(e, eE, lam(e, eE): (_, env) =>
        If(rewrite(cond, env, st.depth, st.idx, condBase + 1, st.slots),
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

    /**
     * A `match` whose cases ask questions (proc-notation stage 4): Paterson's
     * `case`. ONE pure step runs the original `match` — its patterns and
     * guards untouched — and each case answers an injection, into a nested
     * `Either`, of the environment extended by that case's BINDERS. Each
     * case body is compiled at its own extended environment, where a binder
     * is a slot like any `val`, and the bodies are joined by `|||`. No
     * pattern is duplicated and no binder renamed: the symbols the pattern
     * defines are the ones the branch's slots are bound to.
     */
    def compileMatch(scrut: Term, scrutBase: Int, cases: List[CaseDef], st: St, outT: TypeRepr): Term =
      cases.foreach(c => c.guard.foreach(g =>
        if hasMark(g) then report.errorAndAbort(
          "Proc.direct: a mark in a case GUARD. A guard is tested for cases that are not " +
            "taken, so its question would be asked whichever case runs. Ask it before the " +
            "`match` and test the bound value.", g.pos)))
      cases.foreach(c => noOuterAssign(c.rhs, st))
      val e = st.envT
      val binders: List[List[Symbol]] = cases.map(c => bindersOf(c.pattern))
      val sts: List[St] = binders.map(_.foldLeft(st)((s, b) => s.push(b.termRef.widen.dealias).bind(b)))
      val envs: Vector[TypeRepr] = sts.map(_.envT).toVector
      val n = envs.length
      /** the nested Either of the cases from k on */
      def restT(k: Int): TypeRepr = if k == n - 1 then envs(k) else eitherT(envs(k), restT(k + 1))
      /** case i's value, injected into the Either of the cases from k on */
      def inject(k: Int, i: Int, v: Term): Term =
        if k == n - 1 then v
        else if i == k then leftTerm(envs(k), restT(k + 1), v)
        else rightTerm(envs(k), restT(k + 1), inject(k + 1, i, v))
      val selT = restT(0)
      val sel = arrTerm(e, selT, lam(e, selT): (_, env) =>
        val scrutinee = rewrite(scrut, env, st.depth, st.idx, scrutBase + 1, st.slots)
        Match(scrutinee, cases.zipWithIndex.map: (c, i) =>
          val extended = binders(i).foldLeft((e, env: Term)): (acc, b) =>
            val bT = b.termRef.widen.dealias
            (pairT(acc._1, bT), pairTerm(acc._1, bT, acc._2, Ref(b)))
          CaseDef(c.pattern, c.guard.map(g => rewrite(g, env, st.depth, st.idx, st.depth + 1, st.slots)),
            inject(0, i, extended._2))))
      val branches = cases.zipWithIndex.map((c, i) => compileValue(c.rhs, sts(i), outT))
      /** `pa ||| pb` over Either[A, B], as compileIf joins its two */
      def fanin(aT: TypeRepr, bT: TypeRepr, pa: Term, pb: Term): Term =
        val aB = eitherT(aT, bT)
        val aV = eitherT(aT, outT)
        val vA = eitherT(outT, aT)
        val vV = eitherT(outT, outT)
        val s1 = onRightTerm(bT, outT, aT, pb)
        val s2 = arrTerm(aV, vA, lam(aV, vA)((_, x) => Select.unique(x, "swap")))
        val s3 = onRightTerm(aT, outT, outT, pa)
        val s4 = arrTerm(vV, outT, lam(vV, outT): (_, x) =>
          outT.asType match
            case '[v] => '{ ${ x.asExprOf[Either[v, v]] }.fold(identity, identity) }.asTerm)
        thenTerm(aB, vV, outT, thenTerm(aB, vA, vV, thenTerm(aB, aV, vA, s1, s2), s3), s4)
      def dispatch(k: Int): Term =
        if k == n - 1 then branches(k)
        else fanin(envs(k), restT(k + 1), branches(k), dispatch(k + 1))
      thenTerm(e, selT, outT, sel, dispatch(0))

    /**
     * A BRANCH CANNOT ASSIGN A NAME BOUND BEFORE IT. The branch is compiled
     * at a copy of the environment and answers only its value, so a write
     * to an outer `var` inside it would be lost when the branches join —
     * and the compiler's own words for it were "Reassignment to val _2".
     * Found by proc-notation-case-binders beside the `match` it added: the
     * same held for an `if` since branches were compiled.
     */
    def noOuterAssign(branch: Term, st: St): Unit =
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          case a @ Assign(lhs, _) if st.idx.contains(lhs.symbol) =>
            report.errorAndAbort(
              s"Proc.direct: `${lhs.symbol.name}` is assigned inside a branch, but it was bound " +
                "before the branch. A branch answers only its value, so the write would be lost " +
                s"when the branches join. Make the branch's VALUE the new one: " +
                s"`${lhs.symbol.name} = if … then … else …` (or `= x match …`).", a.pos)
          case _ => super.traverseTree(tree)(owner)
      probe.traverseTree(branch)(Symbol.spliceOwner)

    /** the names a pattern binds, in the order they appear */
    def bindersOf(pat: Tree): List[Symbol] =
      val out = List.newBuilder[Symbol]
      val tr = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          case b @ Bind(_, inner) =>
            out += b.symbol
            traverseTree(inner)(owner)
          case _ => super.traverseTree(tree)(owner)
      tr.traverseTree(pat)(Symbol.spliceOwner)
      out.result()

    /** a `while`: `Iter` with the test in front of the body, and the
     * loop-carried state is the environment itself */
    def compileWhile(cond: Term, body: Term, st: St): Term =
      val e = st.envT
      val eE = eitherT(e, e)
      val test = arrTerm(e, eE, lam(e, eE): (_, env) =>
        If(rewrite(cond, env, st.depth, st.idx, st.depth + 1, st.slots),
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
                case _ => projectAt(env, d, i, st.slots)
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
          (asTopIf(rhs), asTopMatch(rhs)) match
            case (Some((c, th, el)), _) =>
              val condBase = emitLeaves(c)
              val before = st.envT
              val sub = compileIf(c, condBase, th, el, st, vT)
              st = st.push(vT)
              add(alongsideTerm(before, vT, sub), before, st.envT)
            case (None, Some((scrut, cases))) =>
              val scrutBase = emitLeaves(scrut)
              val before = st.envT
              val sub = compileMatch(scrut, scrutBase, cases, st, vT)
              st = st.push(vT)
              add(alongsideTerm(before, vT, sub), before, st.envT)
            case (None, None) =>
              val base = emitLeaves(rhs)
              val before = st.envT
              val d = st.depth
              val here = st.idx
              val sl = st.slots
              st = st.push(vT)
              val fn = lam(before, st.envT): (_, env) =>
                pairTerm(before, vT, env, rewrite(rhs, env, d, here, base + 1, sl))
              add(arrTerm(before, st.envT, fn), before, st.envT)
          st = st.bind(vd.symbol)

        case Assign(lhs, rhs0) if asTopIf(strip(rhs0)).isDefined || asTopMatch(strip(rhs0)).isDefined =>
          // `x = if … then … else …` / `x = s match …` with questions in the
          // branches: the branch's VALUE becomes a slot, then the environment
          // is rebuilt with x's slot replaced by it and that slot dropped —
          // the way a branch updates a name, since it cannot assign one
          val rhs = strip(rhs0)
          val sym = lhs.symbol
          val slot = st.idx.getOrElse(sym, report.errorAndAbort(
            s"Proc.direct: `${sym.name}` is assigned but not bound in this block", lhs.pos))
          val target = st
          val vT = st.slots(slot)
          val sub = (asTopIf(rhs), asTopMatch(rhs)) match
            case (Some((c, th, el)), _) =>
              val condBase = emitLeaves(c)
              compileIf(c, condBase, th, el, st, vT)
            case (_, Some((scrut, cases))) =>
              val scrutBase = emitLeaves(scrut)
              compileMatch(scrut, scrutBase, cases, st, vT)
            case _ => report.errorAndAbort("Proc.direct: unreachable", rhs.pos)
          val before = st.envT
          st = st.push(vT)
          add(alongsideTerm(before, vT, sub), before, st.envT)
          val last = st.depth
          val slotsNow = st.slots
          // the condition's or scrutinee's hoisted answers go with it:
          // `rebuild` returns to the shape the assignment started in
          rebuild(target, Some((slot, (env: Term) => projectAt(env, last, last, slotsNow))))
          st = target

        case Assign(lhs, rhs0) =>
          val rhs = strip(rhs0)
          val sym = lhs.symbol
          val slot = st.idx.getOrElse(sym, report.errorAndAbort(
            s"Proc.direct: `${sym.name}` is assigned but not bound in this block", lhs.pos))
          val target = st
          val base = emitLeaves(rhs)
          val d = st.depth
          val here = st.idx
          val sl = st.slots
          rebuild(target, Some((slot, (env: Term) => rewrite(rhs, env, d, here, base + 1, sl))))

        case w @ While(c, b) =>
          guard(w)
          val before = st.envT
          add(compileWhile(c, strip(b), st), before, before)

        case t: Term if asTopIf(t).isEmpty && asTopMatch(t).isDefined =>
          val (scrut, cases) = asTopMatch(t).get
          val scrutBase = emitLeaves(scrut)
          val before = st.envT
          val sub = compileMatch(scrut, scrutBase, cases, st, TypeRepr.of[Unit])
          val fn = lam(pairT(before, TypeRepr.of[Unit]), before): (_, env) =>
            Select.unique(env, "_1")
          add(thenTerm(before, pairT(before, TypeRepr.of[Unit]), before,
            alongsideTerm(before, TypeRepr.of[Unit], sub),
            arrTerm(pairT(before, TypeRepr.of[Unit]), before, fn)), before, before)

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
                alongsideTerm(before, TypeRepr.of[Unit], sub),
                arrTerm(pairT(before, TypeRepr.of[Unit]), before, fn)), before, before)
              val _ = d
            case None =>
              val base = emitLeaves(t)
              val before = st.envT
              val d = st.depth
              val here = st.idx
              val sl = st.slots
              val fn = lam(before, before): (_, env) =>
                Block(List(rewrite(t, env, d, here, base + 1, sl)), env)
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

    /** the same position rule for a `match` whose cases ask questions */
    def asTopMatch(t: Term): Option[(Term, List[CaseDef])] = strip(t) match
      case Match(s, cases) if cases.exists(c => hasMark(c.rhs)) => Some((s, cases))
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
      asTopMatch(lastE) match
        case Some((scrut, cases)) =>
          val from = st.envT
          val (scrutBase, stAfter, steps) = hoist(scrut, st)
          steps.foreach(add(_, from, stAfter.envT))
          st = stAfter
          add(compileMatch(scrut, scrutBase, cases, st, outT), st.envT, outT)
          return acc.get
        case None => ()
      val from0 = st.envT
      val (base, stAfter, steps) = hoist(strip(lastE), st)
      steps.foreach(add(_, from0, stAfter.envT))
      st = stAfter
      val before = st.envT
      val d = st.depth
      val here = st.idx
      val sl = st.slots
      val fn = lam(before, outT): (_, env) =>
        rewrite(strip(lastE), env, d, here, base + 1, sl)
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
            val piece = projectAt(env, stEnd.depth, i, stEnd.slots)
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

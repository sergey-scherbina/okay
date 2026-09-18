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

    /** the shapes v1 cannot compile, each refused where it is written */
    def guard(t: Tree): Unit =
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          case term: Term if asMark(term).isDefined =>
            val arg = asMark(term).get
            if hasMark(arg) then report.errorAndAbort(
              "Proc.direct: a mark inside another mark — bind the inner one to a val first",
              term.pos)
            if arg.tpe.widen.dealias.baseType(procSym) != TypeRepr.of[Nothing] &&
              arg.tpe.widen.dealias.derivesFrom(procSym) then report.errorAndAbort(
              "Proc.direct: this mark's value is a Proc, so the STEP is chosen by a value " +
                "this block binds — that is `app`, and an arrow with `app` is a monad " +
                "(Hughes 2000, §4.5). A static spine is what the deploy check, `leaves` " +
                "and the exhaustive cut all rest on. Put the choice INSIDE the question " +
                "(`!ask(if c then qa else qb)`), or write this program with `direct` and " +
                "run it as one activity.", term.pos)
          case l: Term if l.isExpr && isLambda(l) && hasMark(l) =>
            report.errorAndAbort(
              "Proc.direct: a mark under a lambda — bind the marked value to a val before it",
              l.pos)
          case If(c, th, el) =>
            if hasMark(th) || hasMark(el) then report.errorAndAbort(
              "Proc.direct: a mark inside an `if` BRANCH. Hoisting it would run it whether " +
                "or not the branch is taken, so v1 refuses rather than mis-compile: the node " +
                "for a branch is `OnRight` and it is not wired to the notation yet. A mark " +
                "in the CONDITION is fine, and so is an `if` over values already bound.",
              tree.pos)
            traverseTree(c)(owner)
          case w @ While(c, b) =>
            if hasMark(c) || hasMark(b) then report.errorAndAbort(
              "Proc.direct: a mark inside a loop. The node for iteration is `Iter` and it is " +
                "not wired to the notation yet; write the loop with `Proc.iter` and compose.",
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

    (stats :+ last).foreach(guard)

    // ── the environment: a left-nested tuple, and where each name lives
    //
    // index 0 is the block's own parameter, at the bottom; index i is
    // the i-th value appended. At depth d the projection to index i
    // climbs `._1` (d - i) times and then takes `._2` — except index
    // 0, which is all the way down.
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

    def arrTerm(fromT: TypeRepr, toT: TypeRepr, fn: Term): Term =
      fromT.asType match
        case '[a] => toT.asType match
          case '[b] => '{ Proc.arr[F, a, b](${ fn.asExprOf[a => b] }) }.asTerm

    def keepTerm(envT: TypeRepr, vT: TypeRepr, name: String, fn: Term): Term =
      envT.asType match
        case '[e] => vT.asType match
          case '[v] =>
            '{ Proc.keeping[F, e, v](${ Expr(name) })(${ fn.asExprOf[e => F[v]] }) }.asTerm

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

    /** the marks of one statement, in evaluation order */
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
     * The statement with every bound name replaced by its projection
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

    // ── the walk: one Proc term per statement, composed
    var envT: TypeRepr = TypeRepr.of[X]
    var depth = 0
    var idx: Map[Symbol, Int] = Map(param -> 0)
    var acc: Option[Term] = None

    def compose(step: Term, fromT: TypeRepr, toT: TypeRepr): Unit =
      acc = Some(acc match
        case None => step
        case Some(a) => thenTerm(TypeRepr.of[X], fromT, toT, a, step))
      envT = toT

    /**
     * Every mark of this statement becomes a leaf that APPENDS its
     * answer; the statement's own value is appended afterwards. It
     * returns the depth it STARTED at, and that number is load
     * bearing: the k-th mark's value sits at `base + 1 + k`, so a
     * residual rewritten against the depth AFTER the leaves would
     * project every mark one place too high for each mark before it.
     * Measured as `r|r` where `l|r` was meant — a statement with two
     * marks read the second answer twice.
     */
    def emitLeaves(t: Term): Int =
      val base = depth
      marksOf(t).foreach: arg =>
        val vT = arg.tpe.widen.dealias match
          case AppliedType(_, args) if args.nonEmpty => args.last
          case other => report.errorAndAbort(
            s"Proc.direct: a mark's value must be an operation of this procedure's " +
              s"signature — got ${other.show}", arg.pos)
        val before = envT
        val after = pairT(before, vT)
        val fn = lam(before, TypeRepr.of[F].appliedTo(vT)): (_, env) =>
          rewrite(arg, env, depth, idx, depth + 1)
        compose(keepTerm(before, vT, nameOf(arg), fn), before, after)
        depth += 1
      base

    stats.foreach:
      case vd @ ValDef(_, _, Some(rhs0)) =>
        val rhs = strip(rhs0)
        val base = emitLeaves(rhs)
        val vT = vd.symbol.termRef.widen.dealias
        val before = envT
        val after = pairT(before, vT)
        val fn = lam(before, after): (_, env) =>
          val value = rewrite(rhs, env, depth, idx, base + 1)
          pairTerm(before, vT, env, value)
        compose(arrTerm(before, after, fn), before, after)
        depth += 1
        idx = idx + (vd.symbol -> depth)
      case t: Term =>
        val base = emitLeaves(t)
        val before = envT
        val fn = lam(before, before): (_, env) =>
          Block(List(rewrite(t, env, depth, idx, base + 1)), env)
        compose(arrTerm(before, before, fn), before, before)
      case other => report.errorAndAbort(
        "Proc.direct: only `val`s and plain statements can appear in a block — " +
          s"got ${other.show}", other.pos)

    val lastBase = emitLeaves(strip(last))
    val before = envT
    val fn = lam(before, TypeRepr.of[Y]): (_, env) =>
      rewrite(strip(last), env, depth, idx, lastBase + 1)
    compose(arrTerm(before, TypeRepr.of[Y], fn), before, TypeRepr.of[Y])

    acc.get.asExprOf[Proc[F, X, Y]]

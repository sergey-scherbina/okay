package okay
package macros

import scala.quoted.*
import scala.annotation.tailrec

/**
 * THE DEFER PRE-PASS: a Term-to-Term rewrite that runs BEFORE any
 * bind is emitted, wrapping the calls that would otherwise recurse
 * on the JVM stack in `Free.delay` — self-calls under a mark, and
 * (unless `eagerCalls`) any call at the block's program type; a call
 * in tail position whoever it calls. Marks-and-row only: it reads
 * the block's syntax and its row, and emits nothing of the monad.
 */
private[okay] trait DirectDefer[F[_]] extends DirectMarks[F] with DirectRow[F]:
  import q.reflect.*

  /**
   * DEEP RECURSION (deep-recursive-direct, specs/direct-macro.md): a
   * call to the def this block is the body of, at this block's own
   * program type, is deferred wherever it is marked or auto-coloured
   * — `fib(n - 1)` under `.reflect` or under `selfColor` becomes
   * `Free.delay(() => fib(n - 1))` under the same mark — so a direct
   * block may recurse a million deep and the recursion trampolines
   * through the tree instead of the JVM stack. With `import
   * Direct.given` and `scala.language.implicitConversions` in scope
   * the self-call needs no annotation at all:
   *
   *     def fib(n: Int): Long ! Pure = direct:
   *       if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)
   *
   * which is the rewrite the deepRecursive macro of "Deep recursion
   * in Scala 3" (Kozak) performs on `TailRec`, at the one place it
   * matters: the DEFERRAL, since a self-call evaluated at
   * construction is the native recursion the block was written to
   * avoid. The rest — binds for `a + b`, branches, blocks — is the
   * lowering below, and her `TailRec` is this tree (tailcall =
   * `Delay`, flatMap = `Bind`, done = `Pure`, `.result` = `!.run`).
   *
   * Only a call to the ENCLOSING def, only at the block's program
   * type, only under a mark or a colouring conversion, and never
   * under a lambda (v1 does not look there): a self-call used as a
   * VALUE — passed along, stored — is left as it is.
   *
   * MUTUAL recursion needs no word either, by a second rule: a call
   * in the block's TAIL POSITION at the block's program type is
   * deferred whoever it calls (direct-tail-defer, 2026-09-16). The
   * macro expanding `isEven` cannot know that `isOdd` calls back —
   * a cycle spans files and the other def may not be typed yet — so
   * the enclosing-symbol test cannot see it; the tail position can,
   * and is exactly where a node costs one allocation and saves a
   * frame. Without it `else isOdd(n - 1)` COMPILED, answered at
   * small n and overflowed the stack at depth, which is the worst
   * failure mode there is.
   *
   * Two restrictions keep it honest. A call already wrapped in
   * `!.tailcall`/`Free.delay`/`Free.defer` is left alone, so the
   * explicit spelling does not pay for two nodes. And the rule is
   * only for a call (an `Apply`): a tail-position program VALUE is
   * not deferred, since nothing is built by naming it.
   */
  def deferSelfCalls(t: Term): Term =
    val self: Symbol =
      var o = Symbol.spliceOwner
      while o != Symbol.noSymbol && !o.isDefDef do o = o.owner
      o
    /** the block's lazy vals: a use of one whose rhs is effectful
     * becomes a MARK at compile time (direct-once), and a mark cannot
     * live under the thunk a deferral would build — so a call whose
     * arguments mention one is built where it stands (direct-tell,
     * 2026-09-16: `lazy val plan = !loadPlan(user.planId)` with `user`
     * lazy was "a mark under a lambda", the thunk's) */
    val lazySyms: Set[Symbol] =
      var acc = Set.empty[Symbol]
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case vd: ValDef if vd.symbol.flags.is(Flags.Lazy) => acc += vd.symbol
            case _ => ()
          super.traverseTree(tree)(owner)
      probe.traverseTree(t)(Symbol.spliceOwner)
      acc
    def mentionsLazy(app: Term): Boolean =
      lazySyms.nonEmpty && {
        var found = false
        val probe = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            if !found then tree match
              case id: Ident if lazySyms(id.symbol) => found = true
              case _ => super.traverseTree(tree)(owner)
        probe.traverseTree(app)(Symbol.spliceOwner)
        found
      }
    rowOf match
      case Some(row) if self != Symbol.noSymbol =>
        /** the element type of a self-call at this block's program type */
        def selfProgram(app: Term): Option[TypeRepr] =
          if calleeRoot(app) != self || mentionsLazy(app) || hasMark(app) then None
          else app.tpe.widen.dealias match
            case AppliedType(f, List(r, elem)) if f.typeSymbol == freeClass && r =:= row => Some(elem)
            case _ => None
        lazy val delayApply = Symbol.requiredModule("okay.Free").methodMember("delay").head
        lazy val reflectSym = directSym.methodMember("reflect").head
        /**
         * `Free.delay[row, elem](() => app)`, the thunk built as a term
         * under the owner it is being placed under — `at`, not
         * `Symbol.spliceOwner`. The difference is invisible while the
         * rewrite only fires at the top of a block, and fatal once it
         * fires inside one: a lambda owned by the splice while it sits
         * under a local definition pickles to `assertion failed: method
         * $anonfun`, reproducibly (direct-defer-default, 2026-09-16).
         */
        def delayed(app: Term, elem: TypeRepr, at: Symbol): Term =
          val thunk = Lambda(at, MethodType(Nil)(_ => Nil, _ => app.tpe.widen),
            (owner, _) => app.changeOwner(owner))
          Apply(TypeApply(Ref(delayApply), List(Inferred(row), Inferred(elem.widen))), List(thunk))
        /** Direct.reflect[F, elem](m) — a mark the pipeline recognises */
        def marked(m: Term, elem: TypeRepr): Term =
          Apply(TypeApply(Ref(reflectSym), List(Inferred(TypeRepr.of[F]), Inferred(elem.widen))), List(m))
        /**
         * Does this term ALREADY build a deferring node? `!.tailcall(p)`
         * is inline and reaches the macro as `Free.delay(…)` wrapped in
         * an `Inlined` WITH BINDINGS (the thunk proxy), which `stripped`
         * and `calleeRoot` both leave alone — so the first cut of this
         * check read no name and wrapped the node twice, measured in the
         * expansion. This peels whatever stands between, bindings and
         * all, and asks the name at the bottom.
         */
        def alreadyDefers(t: Term): Boolean =
          @tailrec def root(x: Term): String = x match
            case Inlined(_, _, inner) => root(inner)
            case Typed(inner, _) => root(inner)
            case Block(_, expr) => root(expr)
            case Apply(f, _) => root(f)
            case TypeApply(f, _) => root(f)
            case other => if other.symbol == Symbol.noSymbol then "" else other.symbol.name
          val n = root(t)
          n == "delay" || n == "defer" || n == "tailcall"

        /** a program-typed call, whoever it calls — the tail rule's test */
        /** a CALL, under whatever the typer wrapped it in — a program
         * VALUE is not deferred, since naming one builds nothing */
        @tailrec def isCall(t: Term): Boolean = t match
          case Inlined(_, _, inner) => isCall(inner)
          case Typed(inner, _) => isCall(inner)
          case Block(_, expr) => isCall(expr)
          case _: Apply => true
          case _ => false

        /**
         * Does the term carry definitions of its own — a lambda, a
         * local val or def, a nested `direct` block's context
         * function? Such a tree cannot simply be moved under a new
         * thunk: its symbols are owned where they stand, and the
         * owner surgery that would move them is not what this rule
         * is for. Measured rather than guessed: without this test,
         * `TestConditionDirect`'s `Condition.frame("skip")(ctx ?=>
         * …)` — a call whose argument is a nested block — crashed
         * the compiler in `TreePickler` with `assertion failed:
         * method $anonfun`, reproducibly. Such a call is left where
         * it stands; if it also recurses, `!.tailcall` is the word.
         */
        def carriesDefinitions(t: Term): Boolean =
          var found = false
          val probe = new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit =
              if !found then tree match
                case _: DefDef | _: ValDef | _: ClassDef => found = true
                case Lambda(_, _) => found = true
                case _ => super.traverseTree(tree)(owner)
          probe.traverseTree(t)(Symbol.spliceOwner)
          found

        /**
         * A call whose ARGUMENTS carry marks is not deferred
         * (direct-marked-args, 2026-09-16). The deferral wraps the
         * call in `Free.delay(() => …)`, a thunk this pass builds
         * before anything is compiled — so a mark in an argument
         * would land under a LAMBDA, and the general refusal fired
         * with a message naming a lambda the user never wrote
         * (`!k(!k(5))`, the natural spelling of a continuation
         * invoked twice). The arguments bind first and the call is
         * built inside the continuation, where there is nothing left
         * to defer; deep recursion through such a call is
         * `!.tailcall`'s job, as it is under `eagerCalls`.
         */
        def anyProgram(app: Term): Option[TypeRepr] =
          if !isCall(app) || carriesDefinitions(app) || mentionsLazy(app) || hasMark(app) then None
          else app.tpe.widen.dealias match
            case AppliedType(f, List(r, elem))
              if f.typeSymbol == freeClass && r =:= row && !alreadyDefers(app) => Some(elem)
            case _ => None

        /** the block's tail positions: what it finally hands back. A
         * call there is the last thing the block does, so deferring
         * it costs one node and spares the frame. */
        def tails(t: Term): Term = t match
          case Inlined(c, b, inner) => Inlined(c, b, tails(inner))
          case Typed(inner, tpt) => Typed(tails(inner), tpt)
          case Block(stats, expr) => Block(stats, tails(expr))
          case If(c, th, el) => If(c, tails(th), tails(el))
          case Match(sel, cases) =>
            Match(sel, cases.map(cd => CaseDef.copy(cd)(cd.pattern, cd.guard, tails(cd.rhs))))
          case Apply(TypeApply(fun, targs), List(m)) if markSyms(fun.symbol) =>
            anyProgram(stripped(m)) match
              case Some(elem) => Apply(TypeApply(fun, targs), List(delayed(stripped(m), elem, Symbol.spliceOwner)))
              case None => t
          case Apply(sel @ Select(conv, "apply"), List(m)) if colorSyms(calleeRoot(conv)) =>
            anyProgram(stripped(m)) match
              case Some(elem) => Apply(sel, List(delayed(stripped(m), elem, Symbol.spliceOwner)))
              case None => t
          case _ => t

        val walk = new TreeMap:
          override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
            // v1 does not look under lambdas: a self-call there is a value —
            // and a thunk the tail rule already built is one, so this is
            // also what stops the two rules deferring the same call twice
            case Lambda(_, _) => tree
            // already marked: defer the call, keep the one mark
            case Apply(TypeApply(fun, targs), List(m)) if markSyms(fun.symbol) =>
              (if eager then selfProgram(stripped(m)) else anyProgram(stripped(m))) match
                case Some(elem) =>
                  val inner = super.transformTerm(stripped(m))(owner)
                  Apply(TypeApply(fun, targs), List(delayed(inner, elem, owner)))
                case None => super.transformTerm(tree)(owner)
            // auto-coloured (`val x: Long = fib(n - 1)`): the same, under the conversion
            case Apply(sel @ Select(conv, "apply"), List(m)) if colorSyms(calleeRoot(conv)) =>
              (if eager then selfProgram(stripped(m)) else anyProgram(stripped(m))) match
                case Some(elem) =>
                  val inner = super.transformTerm(stripped(m))(owner)
                  Apply(sel, List(delayed(inner, elem, owner)))
                case None => super.transformTerm(tree)(owner)
            case app: Apply =>
              selfProgram(app) match
                case Some(elem) =>
                  val inner = super.transformTerm(app)(owner)   // arguments first
                  marked(delayed(inner, elem, owner), elem)
                case None => super.transformTerm(tree)(owner)
            case _ => super.transformTerm(tree)(owner)
        walk.transformTerm(tails(t))(Symbol.spliceOwner)
      case _ => t

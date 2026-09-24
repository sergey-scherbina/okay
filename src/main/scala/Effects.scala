package okay

import okay.RowLift.{at, plus}
import scala.annotation.tailrec

/**
 * Extensible effects, founded on the continuation paramonad.
 *
 * A computation A ! F is a freer-monad tree over the signature F; its
 * meaning is its image in Cont, given by foldCont, where a handler is
 * an interpretation F !> S = F ==> ([X] =>> X /> S) — that is,
 * handlers are continuations. The Effects interface is final tagless;
 * Free (the initial encoding) and Eager (Eager.scala, pure binds at
 * construction) are its instances, and object Effects (aliased as `!`
 * for every existing call site) is the concrete toolkit over Free:
 * stepping (resume, next, ?), running, and the tail-resumptive relay.
 * reflect and reify move programs between the encodings.
 *
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * https://blog.higher-order.com/assets/trampolines.pdf
 */

/** a computation of A performing the operations of F: A ! F */
infix type ![A, F[+_]] = Free[F, A]

/** the term-level name, kept as the alias every existing `!.run` /
 * `!.resume` / `!.relay` call site already spells — the object's own
 * name is `Effects`, which is what a NEW call site should write, and
 * what `Effects[M]` (the staging entry, now its `apply`) always meant. */
val ! = Effects

/** a value as a computation */
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)

/** an operation as a computation */
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

/**
 * The same thing postfix, which is what removes the last piece of
 * boilerplate from declaring an effect:
 *
 * enum Users[+A] derives TypeableK:
 * case Find(id: Long) extends Users[Option[String]]
 *
 * Users.Find(7).perform   :  Option[String] ! Users
 *
 * The answer type comes from the CASE — `Find` extends
 * `Users[Option[String]]`, so unifying the receiver against `F[A]`
 * recovers both the signature and what it answers, with nothing
 * written down twice.
 *
 * Named constructors (`def find(id: Long) = effect(Find(id))`) are
 * still worth writing for an effect anyone else will use: they are its
 * API, they read better at every call site, and they cost one line
 * each. This is for the ones nobody but the handler will ever say.
 *
 * It applies to any `F[A]`, including types nobody declared as a
 * signature — and that is not the hazard it first looks like. A freer
 * monad takes ANY type constructor, so `List(1, 2).perform` is not
 * nonsense: it is nondeterminism, and `runSeq` (Choice.scala) is its
 * handler, the same one `Choose` uses. The type that cannot be handled
 * is the one you find out about at the handler, where the row has to
 * be answered — which is the only place the question can be asked.
 */
extension [F[+_], A](op: F[A])
  inline def perform: A ! F = effect(op)

/** the union of two signatures: F + G */
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/** the empty signature: no operations, so a computation over it is
 * PURE — A ! Pure has nothing to perform. The zero of the union
 * algebra (F + Pure = F). The answer NODE of the tree is `Return`
 * (free-return-rename, 2026-09-23), so importing `!.*` no longer
 * shadows this name — it used to, and every such file wrote
 * `okay.Pure`; those spellings still compile. */
type Pure = Nothing

/** fix the parameter of a binary signature: State % S, Throws % E */
infix type %[F[_, _], S] = F[S, *]

/**
 * A partial function, infix: `Request |=> Response ! Async`.
 *
 * The type this stack writes most and reads worst — every route in
 * every server is one. The spelling is the operator's choice, made
 * against THIS file's own `!`: an infix type's precedence comes from
 * its FIRST character, `!` sits at the `=`/`!` level, and anything
 * tighter binds the wrong way — `A ~> B ! F`, `A -?> B ! F` and
 * `A =?> B ! F` all parse as `(A ~> B) ! F`, measured. Only `|`, `^`
 * and `&` are looser, `^` is already `Cont`, and `=?>` would sit one
 * transposition away from the language's `?=>` besides.
 *
 * `|` reads as the alternatives a partial function is made of, and a
 * union on the left binds first, so `Get | Post |=> Res` means what
 * it looks like.
 */
infix type |=>[A, B] = PartialFunction[A, B]

/**
 * Final tagless interface of extensible effects: M[F, A] computes A
 * performing the operations of the signature F. The meaning of a
 * computation is its image in the continuation paramonad, given by
 * foldCont; run and handle are founded on it.
 */
trait Effects[M[_[+_], _]]:
  def pure[F[+_], A](a: A): M[F, A]
  def perform[F[+_], A](e: F[A]): M[F, A]
  /** a bind whose left side is deferred: the thunk is not forced at
   * construction, only when the encoding's own interpreter reaches this
   * node — Free's runners (fold/runFree/resume) force it one hop at a
   * time in their own tailrec loop. This is what lets two
   * mutually-recursive functions returning M[F, A] call each other in
   * tail position without nesting a JVM stack frame per call. */
  def defer[F[+_], A, B](thunk: () => M[F, A])(f: A => M[F, B]): M[F, B]
  /** mark a call to a mutually-recursive function as a tail call — the
   * tagless counterpart of `!.tailcall` (object Effects, below, aliased
   * as `!`), for code written polymorphically over `M: Effects` rather
   * than committed to one encoding. */
  def tailcall[F[+_], A](thunk: => M[F, A]): M[F, A] = defer(() => thunk)(pure)

  extension [F[+_], A](m: M[F, A])
    def flatMap[B](f: A => M[F, B]): M[F, B]
    inline def map[B](f: A => B): M[F, B] = m.flatMap(a => pure(f(a)))
    /** interpret the operations, i.e. reflect the computation into Cont */
    def foldCont[S](h: F !> S): A /> S
    /** run all the effects by a comonadic Handler (the foldCont definition; encodings may override with an equivalent fast path) */
    def runWith(using Handler[F]): A = m.foldCont(handler[F, A]) / identity

  /** handle the effect F by h (and the values by ret), forwarding the
   * effects G; for mass tail-resumption prefer !.relay (measured) */
  def handle[F[+_], G[+_]](using TypeableK[F])[A, B](m: M[F + G, A])
                          (ret: A => M[G, B])
                          (h: F !> M[G, B]): M[G, B] =
    m.foldCont[M[G, B]]([X] => e => split[F, G](e)(e => h(e))(e => shift(k => perform(e).flatMap(k)))) / ret

/**
 * The freer monad is the initial (defunctionalized) encoding of Effects:
 * Inject is a suspended shift, given its meaning by foldCont's !> interpretation.
 * Choose Free when the program is a thing: to step it, inspect it,
 * relay it in stages — and stay stack-safe on any bind shape.
 */
given Effects[Free] with
  override inline def pure[F[+_], A](a: A): Free[F, A] = Free.Return(a)
  override inline def perform[F[+_], A](e: F[A]): Free[F, A] = Free.Inject(e)
  override inline def defer[F[+_], A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    Free.defer(thunk)(f)
  /** the tree has a node for exactly this (delay-node) */
  override def tailcall[F[+_], A](thunk: => Free[F, A]): Free[F, A] = Free.delay(() => thunk)

  extension [F[+_], A](m: Free[F, A])
    override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = m.flatMap(f)
    override def foldCont[S](h: F !> S): A /> S =
      m.fold(Cont.Pure(_))([X] => e => k => h(e).flatMap(k(_).foldCont(h)))
    /** the same answer as the foldCont definition, in one pass instead of two */
    override def runWith(using Handler[F]): A = runFree(m)

  @tailrec private def runFree[F[+_], A](m: Free[F, A])(using H: Handler[F]): A =
    (m.resume: @unchecked) match
      case Free.Return(a) => a
      case Free.Inject(e) => H.handle(e)
      case Free.Bind(Free.Inject(e), f) => runFree(f(H.handle(e)))

  /**
   * The definition (Effects.handle) answers EVERY operation in `Cont`,
   * including the ones the handler never claims: a forwarded operation
   * costs `shift(k => perform(e).flatMap(k))`, a continuation capture
   * spent on work that is pure copying. Measured at +112.7 bytes per
   * forwarded operation and 1.51x overall against `relay` on the same
   * pre-built tree (handle-decompose, docs/benchmarks.md §2, rows
   * `hd-*`), and forwarding is the common case: in a row of four
   * effects every handler forwards three quarters of what it sees.
   *
   * So the tree keeps what belongs to the tree. A forwarded operation
   * is re-emitted on the G side exactly as `relay` does it, and `Cont`
   * is entered ONLY for an operation the handler claims — where the
   * capture is the point rather than an accident.
   *
   * WHY THIS IS THE SAME FUNCTION, and the argument is asymmetric on
   * purpose: a forwarded operation is not the handler's business. It
   * has already been committed to the G program, and a later abort
   * cannot un-perform it — which is exactly what the definition does
   * too, since `perform(e).flatMap(k)` puts `e` before `k` and an
   * abort inside `k` cannot reach back past it. TestHandleForward is
   * that claim as assertions rather than as this paragraph: what an
   * ABORTING handler forwards, what a MULTI-SHOT handler forwards
   * twice, and the order of both. Those tests were written against
   * the definition, watched to FAIL against a deliberately wrong
   * forwarding arm, and only then was this written.
   *
   * THE HANDLED ARM IS WHERE THE COST MOVED TO, and the measurement
   * is worth more than the code. A handler that does not capture
   * answers with `Cont.Pure`, and the loop simply CONTINUES on that
   * answer — one tail call, nothing allocated. Only a handler that
   * really captures needs the rest of the program reified, and it
   * gets a `Delay` so that deep programs trampoline through the
   * interpreter rather than the JVM stack (which is what `foldCont`'s
   * `Cont` runner used to do for them).
   *
   * Taking that `Defer` on EVERY handled operation — the first version
   * of this method — cost 59 µs and 730 328 B on the 10 000-operation
   * lane, against a total gap of 61 µs: a `Defer` whose continuation
   * is `Pure` rotates into a LEFT-nested `Bind`, left-nesting is the
   * one shape `resume` rewrites, and every following operation pays
   * for it. Measured, not reasoned: rows `hff-*`.
   */
  override def handle[F[+_], G[+_]](using TypeableK[F])[A, B](m: Free[F + G, A])
                                   (ret: A => Free[G, B])
                                   (h: F !> Free[G, B]): Free[G, B] =
    // NOT @tailrec, and the reason is a limitation of the annotation
    // rather than of the loop: the two arms that DEFER mention `loop`
    // inside a closure, which @tailrec reads as a non-tail recursive
    // call even though the closure is a separate method that the
    // interpreter, not this loop, will enter. The answered arm below
    // is a real tail call and is compiled as one; what guarantees the
    // depth is TestHandleForward's three stack-safety tests, which is
    // where a guarantee of this kind belongs anyway.
    //
    // The TERMINAL case and the CAPTURING fallback live in their own
    // methods, as `relay.last` does, and the reason is `Free.resume`'s
    // size: since defer-eff-removal it is 323 bytes, under HotSpot's
    // FreqInlineSize of 325, so the JIT pastes it into every loop that
    // calls it. Pasted into `relay`'s 244-byte loop that is worth -6%;
    // pasted into this loop at 388 bytes it cost +15% on handlePrebuilt
    // and handleCapture (rows `de-*`) — a loop inlined into a loop that
    // is itself "hot method too big". handle-loop-inlining made this
    // same extraction when `resume` was 495 bytes and never inlined,
    // and measured nothing; the shape only matters once `resume` fits.
    def last(e: F[A] | G[A]): Free[G, B] =
      split[F, G](e)(e => h(e) / ret)(e => Free.Inject(e).flatMap(ret))

    def capture[X](c: Cont[X, Free[G, B], Free[G, B]], k: X => Free[F + G, A]): Free[G, B] =
      c / (x => Free.delay(() => loop(k(x))))

    def loop(x: Free[F + G, A]): Free[G, B] = (x.resume: @unchecked) match
      case Free.Return(a) => ret(a)
      case Free.Inject(e) => last(e)
      case Free.Bind(Free.Inject(e), k) =>
        split[F, G](e)
          // `h` is asked ONCE: the answered test and the fallback both
          // read the same program, and a handler is not assumed pure
            (e => {
              val c = h(e)
              Cont.onAnswer(c)(a => loop(k(a)))(capture(c, k))
            })
          (e => Free.Inject(e).flatMap(x => loop(k(x))))

    loop(m)

/**
 * Any Effects program in ANY other Effects encoding.
 *
 * This is the initiality of the interface made a function: an
 * encoding is fixed by `pure` and `perform`, `foldCont` is the fold,
 * and so there is exactly one structure-preserving way across. The
 * handler rebuilds each operation in the target — `N.perform(e)` —
 * and the values land through `N.pure`.
 *
 * `reify` and `reflect` below are this at the two ends, and naming
 * them separately is worth it because the two directions are used for
 * different reasons, not because they are different functions.
 */
inline def convert[M[_[+_], _] : Effects,
  N[_[+_], _] : Effects as N, F[+_], A](m: M[F, A]): N[F, A] =
  m.foldCont[N[F, A]]([X] => e => shift(k =>
    N.perform(e).flatMap(k))) / (a => N.pure(a))

/**
 * any Effects program materializes back as a Free tree: building
 * the syntax is itself an interpretation !>, with the answers A ! F
 */
inline def reify[M[_[+_], _] : Effects, F[+_], A](m: M[F, A]): A ! F =
  convert[M, Free, F, A](m)

/**
 * The other direction: a Free tree read INTO any encoding — the
 * eager one, or another of your own.
 *
 * `reify` observes an abstract encoding as syntax, which is what a
 * debugger, a rewriter or `Pipeline`'s optimizer wants. `reflect`
 * spends syntax at an encoding, which is what running it fast wants:
 * a program built once as a tree can be reflected into `Eager` where
 * pure binds apply at construction.
 *
 * Together they are a round trip, and `TestReflect` asserts it is one
 * — the same answers, both ways, for every encoding this library has.
 *
 * One cost of the name, since it is the right name: inside package
 * `okay` it shadows `scala.reflect`, so a `Typeable` or `ClassTag`
 * referred to as `reflect.X` there must be spelled `scala.reflect.X`.
 */
def reflect[M[_[+_], _] : Effects as M, F[+_], A](m: A ! F): M[F, A] =
  // `convert[Free, M]` would say the same through Cont; a tree is
  // already syntax, so it folds straight into the target with no
  // continuation reified on the way (this was `fromFree`, the same
  // function under a second name — core-cleanup)
  m.fold(M.pure)([X] => e => k => M.perform(e).flatMap(x => reflect[M, F, A](k(x))))

object Effects {
  export Free.*

  import Free.*

  /** the staging entry for effect programs, as staged is for Control:
   * `Effects[Free]`, `Effects[Eager]`, `Effects[M]` for any M with an
   * instance in scope. Moved here from a bare top-level def of the
   * same name so this object and `trait Effects[M[_[+_], _]]` above
   * form one door, the way a class and its companion do. */
  transparent inline def apply[M[_[+_], _]]: Effects[M] =
    compiletime.summonInline[Effects[M]]

  // `Effect` used to be a second name for `Inject` here (type + val),
  // kept by freer-base so the match sites would not move. It collided
  // with `okay.Effect`, the `derives` marker — every file importing
  // `!.*` had to write `derives Effect` — and went in
  // inject-not-effect (2026-09-15): the node's name is `Inject`.

  extension [F[+_], A](self: A ! F) {

    /** `resume` is a MEMBER of `Free` now (Free.scala), where the
     * rotation and the invariant every `@unchecked` match relies on
     * are documented together. A member wins resolution, so every
     * `.resume` in the library reaches that one loop. */

    /** step through the next n operations by the Handler */
    @tailrec def next(steps: Long = 1)(using H: Handler[F]): A ! F = (self.resume: @unchecked) match
      case Bind(Inject(e), k) if steps > 0 => k(H.handle(e)).next(steps - 1)
      case a => a

    /**
     * Peek the nearest answer: the value, or the first operation
     * handled.
     *
     * A WORD, not a glyph, since unwrap-glyph: this method RUNS
     * operations through the `Handler`, which is a great deal to hide
     * behind one character — and the character was wanted by the
     * thing users write far more often, the `direct` block's mark.
     * It was `?` until 2026-09-17, and every call site it had was in
     * the core's own tests and benchmarks, which is most of the
     * argument for which spelling gave way (specs/unwrap-glyph.md).
     */
    @tailrec def peek: Handler[F] ?=> ? = self match
      case Bind(a, _) => a.peek
      case Inject(e) => summon[Handler[F]].handle(e)
      case Return(a) => a
      // a peek forces the thunk too, same as `Bind(a, _) => a.peek`
      // discards its own continuation without applying it
      case Delay(t) => t().peek
  }

  /** run a closed computation */
  inline def run[A](e: A ! Nothing): A = e.runWith

  /**
   * mark a call to a mutually-recursive function returning `A ! F` as a
   * tail call, so the interpreter (`fold`/`runFree`/`resume`) trampolines
   * it instead of nesting a JVM stack frame per call. `Free.delay`, a
   * node with no continuation — NOT `Free.defer` with `pure` as the
   * continuation, which was the spelling until delay-node and cost a
   * rotated `.flatMap(pure)` tail down every hop (see `Free.delay`).
   * `Cont.delay` is the same door on the Cont side.
   */
  inline def tailcall[F[+_], A](thunk: => A ! F): A ! F =
    Free.delay(() => thunk)

  /**
   * `tailRecM` for programs (specs/fold-until.md, stage 2): run `f`
   * from `s`, continue from a `Left`, answer a `Right`. The state
   * decides when the iteration ends — the same form as `FoldUntil`
   * over an input and `Proc.Iter` over an arrow, here over a program
   * whose every iteration may perform `F`.
   *
   * Stack-safe without a trampoline of its own: the recursive call
   * sits INSIDE the `flatMap`'s continuation, so it is made when the
   * interpreter resumes that Bind, never on the caller's stack — the
   * discipline direct-loops' `while` relies on for the same reason.
   * Here rather than top-level because Generate.scala's `loop(f)(a)`
   * (the Cont fixpoint) has the same two-list shape, and the overload
   * would be ambiguous at every one of its calls.
   */
  def loop[S, A, F[+_]](s: S)(f: S => Either[S, A] ! F): A ! F =
    f(s).flatMap {
      case Left(next) => loop(next)(f)
      case Right(a) => Return(a)
    }

  /** run p at most once under `Once.run`: the by-need word, an effect —
   * `Once.once`, here because `!.tailcall` (by-name) is its sibling */
  inline def once[A, F[+_]](p: => A ! Once + F): A ! Once + F = Once.once(p)

  /**
   * The same program in a wider row: effect subsumption, as a
   * COERCION (widen-split, 2026-09-23). `Free` is invariant in its
   * row by a measured choice (free-row-variance, 2026-09-03: the
   * covariant `enum Free[+F[+_], A]` passes the variance check, and
   * was not taken), so the type system cannot see that a program at
   * `F` is one at `F + G`; `RowLift.into` says it once, by the erasure
   * argument RowLift.scala states — an `Inject(e)` with `e: F[X]` IS a
   * value of `(F + G)[X]`, since the row is a union. Nothing is
   * forced and nothing is walked: a program whose state is made under
   * `Free.delay` stays deferred until it runs, which is what
   * windows-stage-rerun-loses-pane had to fix in the walk this used to
   * be. The walk is `normalize`, below, under the name of what it
   * does.
   */
  def widen[A, F[+_], G[+_]](p: A ! F): A ! F + G = RowLift.into[A, F, F + G](p)

  /**
   * The WALK `widen` used to be: resume the head and rebuild the tree
   * node by node into the wider row, one re-injected node per
   * operation, deferred as it goes — a NORMALISATION rather than an
   * upcast, and the two are told apart by name now (widen-split).
   *
   * When it pays: a rotation the walk does up front is one a runner
   * would otherwise do per pull — which is why `Source.merge` keeps
   * `Writer.widen`'s walk over its element type (measured,
   * free-row-variance-widen-in-merge: 5.3% slower without it). No
   * caller of THIS name is known; it is here so the walk keeps a
   * name and its reason, not because anything wants it.
   *
   * A DEFERRED HEAD STAYS DEFERRED (windows-stage-rerun-loses-pane,
   * 2026-09-23): `resume` forces a `Delay`, so the two deferred
   * shapes are rebuilt as deferred and the walk begins only when the
   * program runs.
   */
  def normalize[A, F[+_], G[+_]](p: A ! F): A ! F + G = p match
    case Free.Delay(t) => Free.Delay(() => normalize[A, F, G](t()))
    case Bind(Free.Delay(t), f) => Free.defer(() => normalize(t()))(x => normalize[A, F, G](f(x)))
    case _ => (p.resume: @unchecked) match
      case Return(a) => Return(a)
      case Inject(e) => Inject(e)
      case Bind(Inject(e), k) => Inject(e).flatMap(x => normalize[A, F, G](k(x)))

  /**
   * Interpret F into ANOTHER ROW rather than into a value.
   *
   * `Handler[F]` is `F ==> Id`, and Id is exactly where a suspension
   * cannot go — which is why a comonadic handler can never do I/O on
   * a platform with no thread to park (it must ANSWER, so it must
   * finish). The general form is the natural transformation this
   * library already names: a handler valued in a PROGRAM,
   * `F ==> ([X] =>> X ! G)`, so an operation may answer with more
   * computation instead of with a value.
   *
   * Three points on one line, then: `F ==> Id` is the comonadic
   * handler (`runWith`), `F ==> ([X] =>> X ! G)` is this — the
   * forwarding interpreter — and `F !> S` is the Cont-valued handler
   * that `Effects.handle` takes, which adds abort and multi-shot at
   * the price of going through Cont. `translate` is the
   * tail-resumptive middle: one walk, no Cont, G forwarded.
   *
   * `Free.run(f: F ==> M)` is the same idea when the row is handled
   * ENTIRELY; this is the version that leaves a residue.
   */
  /**
   * `translate`, with the widening done for you — and this is the one
   * to reach for when the target row is BIGGER than the source's.
   *
   * `translate` interprets F into a row the program is already in.
   * Interpreting one effect into OTHERS means arriving somewhere new:
   * `A ! Users + F` becomes `A ! State % Store + Writer % String +
   * F`, where F is whatever the caller was already doing and is
   * carried through untouched. Written by hand that is a widen and a
   * translate and three type arguments; here the expected type solves
   * every row:
   *
   * def tracked[A, F[+_]](p: A ! Users + F): A ! Tracked + F =
   * !.interpret(p):
   * [X] => (e: Users[X]) => e match
   * case Users.Find(id) => ...   // a PROGRAM in Tracked + F
   *
   * (Not `interpr`, which builds a handler out of one. This rewrites
   * a program.)
   */
  def interpret[A, F[+_] : TypeableK, G[+_], H[+_]](prog: A ! F + H)
                                                   (h: F ==> ([X] =>> X ! G + H))
  : A ! G + H =
    translate[A, F, G + H](prog.plus[G])(h)

  /**
   * RECORD what a program asks for, without answering any of it: each
   * operation of F is told to a `Writer` and then performed exactly
   * as before, so the row keeps F and gains `Writer % W`.
   *
   * !.tracing(prog)([X] => (e: Users[X]) => e.toString)
   * : A ! Users + Writer % String + G
   *
   * The program-level counterpart of `h.tracing`, and the same idea:
   * the operations are already data, so recording is a layer, not a
   * second implementation that can drift from the first. This one
   * records BEFORE anything is interpreted, so it sees the program's
   * own asks whatever eventually answers them — and it knows nothing
   * about F beyond `show`.
   *
   * The interpreter re-emits `e` into the target row, which does not
   * loop: `translate` walks the SOURCE program and never re-walks
   * what a branch answers with.
   */
  def tracing[A, F[+_] : TypeableK, W, G[+_]](prog: A ! F + G)
                                             (show: [X] => F[X] => W)
  : A ! F + Writer % W + G =
    type R = F + Writer % W + G
    interpret[A, F, Writer % W, F + G](prog):
      [X] => (e: F[X]) =>
        Writer.tell(show(e)).at[R].flatMap(_ => effect[R, X](e))

  def translate[A, F[+_] : TypeableK, G[+_]](prog: A ! F + G)
                                            (h: F ==> ([X] =>> X ! G)): A ! G =
    // every step suspends under a flatMap (the answer is a PROGRAM,
    // not a value), so the recursion lives in closures rather than on
    // the stack — the State.handle shape, and the reason no @tailrec
    // annotation belongs here
    // `split`, not `<|>`: no Either per operation (core-cleanup); the
    // recursion is not a loop, so the inlined arms cost no inlining
    // budget the way they would inside `relay`
    (prog.resume: @unchecked) match
      case Return(a) => Return(a)
      case Inject(e) => split[F, G](e)(f => h(f))(g => Inject(g))
      case Bind(Inject(e), k) =>
        // the Bind node types e and k together
        split[F, G](e)
          (f => h(f).flatMap(x => translate[A, F, G](k(x))(h)))
          (g => Inject(g).flatMap(x => translate[A, F, G](k(x))(h)))

  /**
   * handle_relay (Kiselyov): tail-resumptive handling. It was 1.51x
   * faster than `Effects.handle` on forwarding-heavy work; since
   * handle-forward-fast (2026-09-15) it is **1.03x**, and the two
   * allocate the SAME NUMBER OF BYTES to the digit, because `handle`
   * was given this loop's forwarding arm. What is left of the reason
   * to reach for `relay` is therefore not speed: it is that an
   * answer-polymorphic `g` cannot abort or perform G, which is a
   * CLAIM about the handler that the type makes and `handle` cannot.
   * docs/benchmarks.md §2, rows `hd-*` and `hff-*`. g is
   * answer-polymorphic, so by parametricity it must resume the
   * continuation (exactly once), which keeps the loop tail-recursive,
   * i.e. stack-safe on any number of handled operations. For handlers
   * that abort or perform G, use Effects.handle instead.
   */
  def relay[A, B, F[+_] : TypeableK, G[+_]](a: A ! F + G)(f: A => B ! G)
                                           (g: [X, Y] => F[X] => X /> Y): B ! G = {
    /**
     * The TERMINAL case — a bare operation with no continuation, which
     * a program reaches at most once — in its own method, so that it
     * does not occupy the hot loop's bytecode.
     *
     * `split` is an `inline def` taking `inline` branches, so both of
     * its arms expand into whatever encloses them, and this loop is
     * made of them. It compiles to 305 bytes against HotSpot's
     * `FreqInlineSize` of 325 (read with -XX:+PrintInlining): twenty
     * bytes from the cliff where it stops being inlined into `relay`
     * and the lane loses over 10% at once. That is not a hypothetical
     * — a sibling branch added 24 bytes here and paid exactly that,
     * for five measurement sessions, while its allocation stayed
     * identical to the digit and no data-structure theory fit.
     * Extracting the cold arm leaves the loop at 244 bytes.
     */
    def last(e: F[A] | G[A]): B ! G =
      split[F, G](e)(e => g(e) / f)(e => Inject(e).flatMap(f))

    @tailrec def loop(x: A ! F + G): B ! G = (x.resume: @unchecked) match
      // `g(e) / k`, not `g(e)(k)`: the Cont carrier's application is
      // `/` since Cont became a facade over Free (specs/freer-base.md)
      case Bind(Inject(e), k) => split[F, G](e)(e => loop(g(e) / k))(e => Inject(e).flatMap(x => relay[A, B, F, G](k(x))(f)(g)))
      case Inject(e) => last(e)
      case Return(a) => f(a)

    loop(a)
  }

}

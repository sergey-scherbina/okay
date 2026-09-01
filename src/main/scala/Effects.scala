package okay

import scala.annotation.implicitNotFound

import scala.annotation.tailrec
import scala.reflect.Typeable

/**
 * Extensible effects, founded on the continuation paramonad.
 *
 * A computation A ! F is a freer-monad tree over the signature F; its
 * meaning is its image in Cont, given by foldCont, where a handler is
 * an interpretation F !> S = F ==> ([X] =>> X /> S) — that is,
 * handlers are continuations. The Effects interface is final tagless
 * with two encodings (Free — initial, Eff — final, Church), and the
 * object ! is the concrete toolkit over the Free encoding: stepping
 * (resume, next, ?), running, and the tail-resumptive relay.
 *
 * Choosing an encoding: the tree is for tools — stepping, staged
 * relay, stack safety on any bind shape; the function is for speed —
 * fused build-and-run pipelines with no tree at all; the interface is
 * for not choosing too early. fromFree and reify move programs
 * between the encodings.
 *
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * https://blog.higher-order.com/assets/trampolines.pdf
 */

/** fix the parameter of a binary signature: State % S, Throws % E */
infix type %[F[_, _], S] = F[S, *]

/** the empty signature: no operations, so a computation over it is
 * PURE — A ! Pure has nothing to perform. The zero of the union
 * algebra (F + Pure = F). In scopes that import !.* the name is
 * shadowed by the Free.Pure case: write okay.Pure there. */
type Pure = Nothing

/** the union of two signatures: F + G */
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/** a computation of A performing the operations of F: A ! F */
infix type ![A, F[+_]] = Free[F, A]

/** a value as a computation */
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)

/** an operation as a computation */
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

/** an interpretation of F into any Control carrier C, with the answers S */
type Interpr[F[_], C[_, _, _], S] = F ==> C[*, S, S]

/**
 * A handler of the operations F, with the answers S, is an interpretation
 * of F in the continuation paramonad: the natural transformation
 *
 * F ==> ([X] =>> X /> S)
 *
 * That is, handlers are continuations.
 */
infix type !>[F[_], S] = Interpr[F, Cont, S]

/** A comonadic handler interprets each operation by its own value */
@implicitNotFound("no Handler[${F}].\nA Handler answers each operation with a plain value (trait Handler: def handle[A](a: F[A]): A).\nFor a ROW, build the union from the parts: given Handler[F + G] = Handler.union[F, G]\n(each part needs its own Handler in scope first).")
trait Handler[F[_]]:
  def handle[A](a: F[A]): A

/** A comonadic (per-operation) Handler at every answer type. */
inline def handler[F[_] : Handler as H, S]: F !> S =
  [X] => e => Cont.Pure(H.handle(e))

/** the same, at any Control carrier */
inline def interpr[C[_, _, _] : Control as C, F[_] : Handler as H, S]: Interpr[F, C, S] =
  [X] => e => C.pure(H.handle(e))

/** named, with a PUBLIC `C`, for the same binary-compatibility reason
 * as `DiagonalMonad`: an inline method reaching a privately captured
 * given makes the compiler synthesize an accessor with an unstable
 * name, and a downstream JAR compiled against it breaks when this
 * library is recompiled. */
final class ComonadHandler[F[_]](val C: Comonad[F]) extends Handler[F]:
  inline def handle[A](a: F[A]): A = C.extract(a)

given [F[_] : Comonad as C]: Handler[F] = ComonadHandler[F](C)

/** Pure has no operations left to handle */
given Handler[Pure] with
  inline def handle[A](a: Pure): A = a

/**
 * Handlers compose along the union: split the operation by the F
 * test and delegate. This is what lets a multi-effect row be run by
 * `runWith` with one handler per effect, assembled by the compiler —
 * an agent's `Model + (Tool + (Context + Async))` needs no bespoke
 * interpreter, only its four handlers in scope.
 */
object Handler {
  /**
   * Handlers compose along the union: split the operation by the F
   * test and delegate — one handler per effect, one row. Spelled as
   * an EXPLICIT combinator, not a given, on purpose: a given whose
   * subject is a union type lambda enters implicit scope for every
   * Handler query and crashes the 3.7.1 type comparer ("Failure to
   * join alternatives F and G") while it is being compared against
   * unrelated handlers. Called by name, the same code is fine — the
   * types at a call site are concrete.
   */
  def union[F[+_], G[+_]](using T: TypeableK[F], hf: Handler[F], hg: Handler[G])
  : Handler[F + G] = new Handler[F + G]:
    def handle[A](a: F[A] | G[A]): A =
      val split: Either[F[A], G[A]] = a match
        case T(f) => Left(f)
        case other => Right(other.asInstanceOf[G[A]])
      split.fold(f => hf.handle(f), g => hg.handle(g))
}

/**
 * Final tagless interface of extensible effects: M[F, A] computes A
 * performing the operations of the signature F. The meaning of a
 * computation is its image in the continuation paramonad, given by
 * foldCont; run and handle are founded on it.
 */
trait Effects[M[_[+_], _]]:
  def pure[F[+_], A](a: A): M[F, A]
  def perform[F[+_], A](e: F[A]): M[F, A]

  extension [F[+_], A](m: M[F, A])
    def flatMap[B](f: A => M[F, B]): M[F, B]
    inline def map[B](f: A => B): M[F, B] = m.flatMap(a => pure(f(a)))
    /** interpret the operations, i.e. reflect the computation into Cont */
    def foldCont[S](h: F !> S): A /> S
    /** the same at any Control carrier (foldCont is its Cont fast path) */
    def foldIn[C[_, _, _] : Control, S](h: Interpr[F, C, S]): C[A, S, S]
    /** run all the effects by a comonadic Handler (the foldCont definition; encodings may override with an equivalent fast path) */
    def runWith(using Handler[F]): A = m.foldCont(handler[F, A]) / identity
    /**
     * run at a chosen Control carrier. Cont is the stack-safe default;
     * Func composes closures at run time — measured no faster than
     * Cont here (see specs/staged-effects.md): true staged effects are
     * inline handler-passing programs over Control, not a carrier.
     */
    inline def runIn[C[_, _, _]](using Handler[F], Control[C]): A =
      m.foldIn[C, A](interpr[C, F, A]) / identity

  /** handle the effect F by h (and the values by ret), forwarding the
   * effects G; for mass tail-resumption prefer !.relay (measured) */
  def handle[F[+_] : TypeableK, G[+_], A, B](m: M[F + G, A])
                                            (ret: A => M[G, B])
                                            (h: F !> M[G, B]): M[G, B] =
    m.foldCont[M[G, B]]([X] => e => <|>[F, G](e) match
      case Left(e) => h(e)
      case Right(e) => shift(k => perform(e).flatMap(k))
    ) / ret

/** the staging entry for effect programs, as staged is for Control */
transparent inline def Effects[M[_[+_], _]]: Effects[M] =
  compiletime.summonInline[Effects[M]]

/** ∀X, the runtime test for F[X], by the erasure of F */
@implicitNotFound("no TypeableK[${F}].\nSplitting a row needs a runtime test for ${F}'s operations. If one class carries the whole\nsignature (the answer type is the only parameter), declare:\n  given TypeableK[${F}] = typeableK(classOf[YourOp[?]])\n— the test is then TOTAL (see Delim.scala's precedent and the typepedia entry).")
trait TypeableK[F[_]]:
  def unapply[A](x: Any): Option[x.type & F[A]]

/**
 * A `TypeableK` by the runtime CLASS of a signature's values.
 *
 * For a signature whose ONLY parameter is the answer type — `Async`,
 * `Choose`, `Resource`, an agent's `Model` — this test is COMPLETE:
 * the answer type is erased anyway, so the class is the whole
 * identity of the operation, and there is nothing left to check.
 * Say that once, here, rather than let the compiler say "cannot be
 * checked at runtime" at every one of a hundred use sites for a test
 * that is in fact total.
 *
 * For a PARAMETERISED signature (`Writer % W`, `State % S`,
 * `Throws % E`) the class is NOT the whole identity, and this is the
 * wrong instance to reach for: see `TypeableK.byClassPartial`.
 */

def typeableK[F[_]](cls: Class[?]): TypeableK[F] = new TypeableK[F]:
  def unapply[A](x: Any): Option[x.type & F[A]] =
    if cls.isInstance(x) then Some(x.asInstanceOf[x.type & F[A]]) else None

/**
 * A `TypeableK` for a signature whose PARAMETER leaves no runtime
 * trace — `Reader % R`, `State % S`, `Take % V`. The test is by class
 * and therefore says only "this is a Reader", not "this is a Reader
 * of Int".
 *
 * That is exactly as sound as what was there before, and no less: the
 * generic instance below tests the same erasure. What this one adds is
 * a NAME for the limitation and one place to read about it, instead of
 * "the type test cannot be checked at runtime" repeated at every use
 * site, where it is unactionable and drowns out the warnings that can
 * be acted on.
 *
 * The limitation, stated: a row may hold ONE instance of such a
 * signature. Two — `Reader % Int + Reader % String` — misroute, and
 * `TestRowIdentity` demonstrates exactly how (the first handler
 * answers both asks and the second continuation gets a
 * ClassCastException, so it fails loudly at the first wrong answer
 * rather than returning a plausible wrong result).
 */
def typeableKByClass[F[_]](cls: Class[?]): TypeableK[F] = typeableK(cls)

/**
 * The fallback lives in the TYPECLASS'S COMPANION, and that placement
 * is the whole point: a given in lexical scope (which `import
 * okay.given` puts there) BEATS one in a type's implicit scope, so a
 * toplevel generic instance would shadow every specific one — which
 * is exactly what happened, and why `Model`, `Tool` and `Context`
 * kept getting the erasure-based test after being given a total one.
 * From the companion it is implicit scope too, and specificity picks
 * the better instance.
 */
object TypeableK:
  /** by the compiler-synthesized class test — no cast: sound by
   * covariance, F[Nothing] <: F[X] for every X. Complete only when
   * the erasure IS the signature's identity; a signature that wants
   * better should say so with its own instance. */
  given [F[+_]](using t: Typeable[F[Nothing]]): TypeableK[F] = new:
    def unapply[A](x: Any): Option[x.type & F[A]] = t.unapply(x)

  /** the empty signature is trivially splittable: nothing inhabits
   * it, so the test never matches — which lets row-generic code
   * (Logic, the effectful streams) instantiate at F = Pure */
  given TypeableK[Pure] = new:
    def unapply[A](x: Any): Option[x.type & Nothing] = None

/**
 * Split the union by testing only the F side (the erasure of F, by
 * TypeableK), taking G by exclusion: a type test on an abstract G
 * would erase to an always-true test.
 */
inline def <|>[F[+_] : TypeableK as T, G[+_]]: [A] => (F[A] | G[A]) => Either[F[A], G[A]] =
  [A] => e => e match
    case T(e) => Left(e)
    // the trusted kernel, sound by the excluded middle of the union:
    // a value of F[A] | G[A] that is not an F[A] is a G[A]
    case e => Right(e.asInstanceOf[G[A]])

/**
 * The freer monad is the initial (defunctionalized) encoding of Effects:
 * Inject is a suspended shift, given its meaning by foldCont's !> interpretation.
 * Choose Free when the program is a thing: to step it, inspect it,
 * relay it in stages — and stay stack-safe on any bind shape.
 */
given Effects[Free] with
  override inline def pure[F[+_], A](a: A): Free[F, A] = Free.Pure(a)
  override inline def perform[F[+_], A](e: F[A]): Free[F, A] = Free.Inject(e)

  extension [F[+_], A](m: Free[F, A])
    override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = m.flatMap(f)
    override def foldCont[S](h: F !> S): A /> S =
      m.fold(Cont.Pure(_))([X] => e => k => h(e).flatMap(k(_).foldCont(h)))
    override def foldIn[C[_, _, _], S](h: Interpr[F, C, S])(using C: Control[C]): C[A, S, S] =
      m.fold(C.pure)([X] => e => k => C.flatMap(h(e))(x => k(x).foldIn[C, S](h)))
    /** the same answer as the foldCont definition, in one pass instead of two */
    override def runWith(using Handler[F]): A = runFree(m)

  @tailrec private def runFree[F[+_], A](m: Free[F, A])(using H: Handler[F]): A = m match
    case Free.Pure(a) => a
    case Free.Inject(e) => H.handle(e)
    case Free.Bind(Free.Bind(a, f), g) => runFree(Free.Bind(a, f(_).flatMap(g)))
    case Free.Bind(Free.Pure(a), f) => runFree(f(a))
    case Free.Bind(Free.Inject(e), f) => runFree(f(H.handle(e)))

/**
 * Effects are continuation programs, literally: Eff is the final
 * (Church) encoding of the interface — a computation as the function
 * of its handler, where foldCont is the program itself. This is how
 * extensible effects were first defined (Kiselyov–Sabry–Swords 2013,
 * by continuations), before the freer tree of 2015 — so Eff and Free
 * reenact the history, and "Free and Eff agree" is the claim that the
 * two papers describe one thing. Choose Eff when the program is a
 * pipeline: built once and run, the handler fusing into the closures
 * with no tree materialized at all. (Unlike Free, Eff is not
 * stack-safe on a left-nested flatMap, and cannot be stepped.)
 */
type Eff[F[+_], A] = [S] => F !> S => A /> S

/** every Eff[F, *] is a Monad, by its Effects instance (Free[F, *]
 * has the same, in Free.scala) — for-comprehensions on either encoding */
given [F[+_]]: Monad[[A] =>> Eff[F, A]] with
  override def pure[A](a: A): Eff[F, A] = summon[Effects[Eff]].pure(a)
  extension [A](m: Eff[F, A])
    override def flatMap[B](f: A => Eff[F, B]): Eff[F, B] =
      summon[Effects[Eff]].flatMap(m)(f)

given Effects[Eff] with
  override inline def pure[F[+_], A](a: A): Eff[F, A] =
    [S] => (_: F !> S) => Cont.Pure(a)
  override inline def perform[F[+_], A](e: F[A]): Eff[F, A] =
    [S] => (h: F !> S) => h(e)

  extension [F[+_], A](m: Eff[F, A])
    override inline def flatMap[B](f: A => Eff[F, B]): Eff[F, B] =
      [S] => (h: F !> S) => m[S](h).flatMap(a => f(a)[S](h))
    override inline def foldCont[S](h: F !> S): A /> S = m[S](h)
    /** Eff is committed to Cont; changing the carrier reifies the tree first */
    override inline def foldIn[C[_, _, _], S](h: Interpr[F, C, S])(using Control[C]): C[A, S, S] =
      (m[A ! F]([X] => e => shift(k => effect(e).flatMap(k))) / (a => Free.pure(a))).foldIn[C, S](h)

/**
 * Free is initial: the tree interprets uniquely into every Effects
 * instance (fromFree). Eff is final: every instance observes into it
 * by its own foldCont (toEff). So all the encodings live between the
 * tree and its behavior, and reify closes the circle: any program
 * materializes back as syntax.
 */
def fromFree[M[_[+_], _] : Effects as E, F[+_], A](m: A ! F): M[F, A] =
  m.fold(E.pure)([X] => e => k => E.perform(e).flatMap(x => fromFree[M, F, A](k(x))))

/** every Effects instance observes into Eff, by its own foldCont */
inline def toEff[M[_[+_], _] : Effects, F[+_], A](m: M[F, A]): Eff[F, A] =
  [S] => (h: F !> S) => m.foldCont(h)

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
inline def convert[M[_[+_], _] : Effects, N[_[+_], _] : Effects as N, F[+_], A]
                  (m: M[F, A]): N[F, A] =
  m.foldCont[N[F, A]]([X] => e => shift(k => N.perform(e).flatMap(k))) / (a => N.pure(a))

/**
 * any Effects program materializes back as a Free tree: building
 * the syntax is itself an interpretation !>, with the answers A ! F
 */
inline def reify[M[_[+_], _] : Effects, F[+_], A](m: M[F, A]): A ! F =
  m.foldCont[A ! F]([X] => e => shift(k => effect(e).flatMap(k))) / (a => pure(a))

/**
 * The other direction: a Free tree read INTO any encoding — the
 * Church one (`Eff`), the eager one, or another of your own.
 *
 * `reify` observes an abstract encoding as syntax, which is what a
 * debugger, a rewriter or `Pipeline`'s optimizer wants. `reflect`
 * spends syntax at an encoding, which is what running it fast wants:
 * a program built once as a tree can be reflected into `Eager` where
 * pure binds apply at construction, or into `Eff` where there is no
 * tree to walk.
 *
 * Together they are a round trip, and `TestReflect` asserts it is one
 * — the same answers, both ways, for every encoding this library has.
 *
 * One cost of the name, since it is the right name: inside package
 * `okay` it shadows `scala.reflect`, so a `Typeable` or `ClassTag`
 * referred to as `reflect.X` there must be spelled `scala.reflect.X`.
 */
inline def reflect[M[_[+_], _] : Effects as M, F[+_], A](m: A ! F): M[F, A] =
  m.foldCont[M[F, A]]([X] => e => shift(k => M.perform(e).flatMap(k))) / (a => M.pure(a))

object ! {
  export Free.*

  import Free.*

  /** the domain name of Inject: an operation node */
  type Effect[F[+_], A] = Inject[F, A]
  val Effect = Inject

  extension [F[+_], A](self: A ! F) {

    /** normalize to a head form: Pure, Effect, or Bind(Effect, k) */
    @tailrec def resume: A ! F = self match
      case Bind(Bind(a, h), k) => a.flatMap(h(_).flatMap(k)).resume
      case Bind(Pure(a), k) => k(a).resume
      case a => a

    /**
     * THE INVARIANT `resume` ESTABLISHES, and why every match over it
     * is written `(x.resume: @unchecked) match`.
     *
     * By construction the result is one of exactly three shapes —
     * `Pure(a)`, `Effect(e)`, `Bind(Effect(e), k)` — because the
     * rotation above normalizes `Bind(Bind(…), k)` and
     * `Bind(Pure(…), k)` away. The TYPE cannot say so: it is still
     * `A ! F`, whose cases include the two that cannot occur, so a
     * correct three-case match reads as inexhaustive to the compiler
     * and did so at forty-two sites — enough to bury every warning it
     * had that was worth reading.
     *
     * The alternatives all cost something real. A three-case view ADT
     * would let the compiler check it, at one allocation per step on
     * the hottest path in the library. Explicit impossible branches
     * would too, at one more type test per step. `@unchecked` costs
     * nothing at runtime and marks exactly the claim being made, at
     * the place it is made — so that is what is used, and this is the
     * one place that says what the claim is.
     */

    /** step through the next n operations by the Handler */
    @tailrec def next(steps: Long = 1)(using H: Handler[F]): A ! F = (self.resume: @unchecked) match
      case Bind(Effect(e), k) if steps > 0 => k(H.handle(e)).next(steps - 1)
      case a => a

    /** peek the nearest answer: the value, or the first operation handled */
    @tailrec def ? : Handler[F] ?=> ? = self match
      case Bind(a, _) => a.?
      case Effect(e) => summon[Handler[F]].handle(e)
      case Pure(a) => a
  }

  /** run a closed computation */
  inline def run[A](e: A ! Nothing): A = e.runWith

  /** re-inject into a wider row: effect subsumption. Free is invariant
   * in its signature, so widening walks the tree — one re-injected
   * node per operation, deferred as it goes. */
  def widen[A, F[+_], G[+_]](p: A ! F): A ! (F + G) = (p.resume: @unchecked) match
    case Pure(a) => Pure(a)
    case Effect(e) => Effect(e)
    case Bind(Effect(e), k) => Effect(e).flatMap(x => widen[A, F, G](k(x)))

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
  def translate[A, F[+_] : TypeableK, G[+_]](prog: A ! (F + G))
                                            (h: F ==> ([X] =>> X ! G)): A ! G =
    // every step suspends under a flatMap (the answer is a PROGRAM,
    // not a value), so the recursion lives in closures rather than on
    // the stack — the State.handle shape, and the reason no @tailrec
    // annotation belongs here
    (prog.resume: @unchecked) match
      case Pure(a) => Pure(a)
      case Effect(e) => <|>[F, G](e) match
        case Left(f) => h(f)
        case Right(g) => Effect(g)
      case Bind(Effect(e), k) =>
        val cont = k.asInstanceOf[Any => A ! (F + G)]
        <|>[F, G](e) match
          case Left(f) => h(f).flatMap(x => translate[A, F, G](cont(x))(h))
          case Right(g) => Effect(g).flatMap(x => translate[A, F, G](cont(x))(h))

  /**
   * handle_relay (Kiselyov): tail-resumptive handling, measured 1.45x
   * faster than Effects.handle on forwarding-heavy work. g is
   * answer-polymorphic, so by parametricity it must resume the
   * continuation (exactly once), which keeps the loop tail-recursive,
   * i.e. stack-safe on any number of handled operations. For handlers
   * that abort or perform G, use Effects.handle instead.
   */
  def relay[A, B, F[+_] : TypeableK, G[+_]](a: A ! F + G)(f: A => B ! G)
                                           (g: [X, Y] => F[X] => X /> Y): B ! G = {
    @tailrec def loop(x: A ! F + G): B ! G = (x.resume: @unchecked) match
      case Bind(Effect(e), k) => <|>[F, G](e) match
        case Left(e) => loop(g(e)(k))
        case Right(e) => Effect(e).flatMap(x => relay[A, B, F, G](k(x))(f)(g))
      case Effect(e) => <|>[F, G](e) match
        case Left(e) => g(e)(f)
        case Right(e) => Effect(e).flatMap(f)
      case Pure(a) => f(a)

    loop(a)
  }

}

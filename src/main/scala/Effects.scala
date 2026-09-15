package okay

import scala.quoted.*
import okay.RowLift.{at, plus}

import scala.annotation.implicitNotFound

import scala.annotation.tailrec

/**
 * Extensible effects, founded on the continuation paramonad.
 *
 * A computation A ! F is a freer-monad tree over the signature F; its
 * meaning is its image in Cont, given by foldCont, where a handler is
 * an interpretation F !> S = F ==> ([X] =>> X /> S) — that is,
 * handlers are continuations. The Effects interface is final tagless;
 * Free (the initial encoding) and Eager (Eager.scala, pure binds at
 * construction) are its instances, and the object ! is the concrete
 * toolkit over Free: stepping (resume, next, ?), running, and the
 * tail-resumptive relay. reflect and reify move programs between the
 * encodings.
 *
 * There WAS a third instance, Eff — the Church encoding, a program as
 * the function of its handler (Kiselyov–Sabry–Swords 2013). It proved
 * the interface honest ("Free and Eff agree") and it was measured: the
 * no-tree road ran at 0.58–0.86x of the fused Free loop (handler-fusion
 * stage B), stack safety cost it a Cont.defer per bind (eff-stack-
 * safety, +11%), and nothing outside its own tests ever built one.
 * Removed 2026-09-15 (defer-eff-removal); the history is in those two
 * specs and in history.tsv rows `effSW*`.
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

/** a value as a computation */
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)

/** an operation as a computation */
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

/**
 * The same thing postfix, which is what removes the last piece of
 * boilerplate from declaring an effect:
 *
 *     enum Users[+A] derives TypeableK:
 *       case Find(id: Long) extends Users[Option[String]]
 *
 *     Users.Find(7).perform   :  Option[String] ! Users
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

/** an interpretation of F into any Control carrier C, with the answers
 * S — the handler type of an inline handler-passing program (`Fused`,
 * specs/staged-effects.md), which is what "staged effects" means here:
 * a carrier-generic fold on the ENCODING (`foldIn`/`runIn`) was
 * measured no faster than Cont and is gone (core-cleanup) */
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

extension [F[_]](h: Handler[F])
  /**
   * Every handler can be a recording one, without being written
   * twice.
   *
   *     rename(7, "grace").runWith(using live(c).tracing(log += _))
   *
   * "What did this program ask for, and in what order" is the
   * question a test wants answered, and the operations are ALREADY
   * data — so the answer is a decorator, not a second handler. It
   * sees exactly what the real one sees, because it IS the real one
   * with a line in front.
   */
  def tracing(log: Any => Unit): Handler[F] = new:
    def handle[A](a: F[A]): A = { log(a); h.handle(a) }

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
                         (using Distinct[F + G])
  : Handler[F + G] = new Handler[F + G]:
    def handle[A](a: F[A] | G[A]): A =
      // the split is the kernel's (`split`), the one place the
      // union's excluded middle is claimed — and with no Either on
      // the way (split-without-either)
      split[F, G](a)(f => hf.handle(f))(g => hg.handle(g))
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
  /** a bind whose left side is deferred: the thunk is not forced at
   * construction, only when the encoding's own interpreter reaches this
   * node — Free's runners (fold/runFree/resume) force it one hop at a
   * time in their own tailrec loop. This is what lets two
   * mutually-recursive functions returning M[F, A] call each other in
   * tail position without nesting a JVM stack frame per call. */
  def defer[F[+_], A, B](thunk: () => M[F, A])(f: A => M[F, B]): M[F, B]
  /** mark a call to a mutually-recursive function as a tail call — the
   * tagless counterpart of `!.tailcall` (object !, this file), for code
   * written polymorphically over `M: Effects` rather than committed to
   * one encoding. */
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

/** the staging entry for effect programs, as staged is for Control */
transparent inline def Effects[M[_[+_], _]]: Effects[M] =
  compiletime.summonInline[Effects[M]]

/** ∀X, the runtime test for F[X], by the erasure of F */
@implicitNotFound("no TypeableK[${F}].\nSplitting a row needs a runtime test for ${F}'s operations, and a signature declares its own:\n  enum YourOp[+A] derives Effect\nA parameterised one says the same: `enum YourOp[S, +A] derives Effect` abstracts the LAST\nparameter, and the test is then by class only (a row may hold one of it).\nA ROW needs no instance: the split tests one side and takes the other by exclusion.")
trait TypeableK[F[_]]:
  /** is `x` an operation of F — asked by `split` on every operation
   * of every runner (split-without-either), and the WHOLE interface:
   * there used to be an `unapply` beside it answering
   * `Option[x.type & F[A]]`, and nothing in the repository ever
   * matched with it — every runner refines through `split` and then
   * matches the constructor. The extractor cost each instance a
   * method and a cast for a question `test` answers with neither
   * (core-cleanup, 2026-09-15). */
  def test(x: Any): Boolean

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

def typeableK[F[_]](cls: Class[?]): TypeableK[F] = Effect.ByClass[F](cls)

/**
 * The limitation of a class test, stated once — for a signature whose
 * PARAMETER leaves no runtime trace (`Reader % R`, `State % S`,
 * `Take % V`) the test says only "this is a Reader", not "this is a
 * Reader of Int". So a row may hold ONE instance of such a signature,
 * and `Distinct[R]`, which `Handler.union` requires, refuses the row
 * at COMPILE time rather than leaving it to the first wrong answer. A
 * test that is finer than the class says so in its declared type
 * (`TypeableK.ByValue`) and is allowed to repeat; `writerK` is the one
 * that does. Two — `Reader % Int + Reader % String` — misroute, and
 * `TestRowIdentity` demonstrates exactly how (the first handler
 * answers both asks and the second continuation gets a
 * ClassCastException: loud, at the first wrong answer).
 *
 * (`typeableKByClass` used to be a second name for `typeableK` that
 * carried this paragraph; nothing called it — core-cleanup.)
 */

/**
 * There is NO generic instance any more, and that is the point.
 *
 * There used to be one — `given [F[+_]](using Typeable[F[Nothing]])`,
 * an erasure test derived for any signature that had not declared
 * one. It cost more than it saved. It made every effect that forgot
 * to declare a test work anyway, at a warning per USE site ("the type
 * test for F[Nothing] cannot be checked at runtime") that the author
 * of the effect never saw. It shadowed better instances when brought
 * into lexical scope by `import okay.given`, which is why `Model`,
 * `Tool` and `Context` kept getting the erasure test after being
 * given a total one. And it was the one place in this library that
 * NEEDED the row's covariance, since `F[Nothing] <: F[X]` is what
 * made it sound (specs/writer-covariance.md, signature-covariance).
 *
 * Now a signature says `derives Effect` and its instance lives in its
 * own companion, where implicit search finds it with no import and
 * nothing can shadow it. What was lost with the fallback: a COMPOSITE
 * row can no longer be given a test implicitly. Nothing needs one —
 * `Handler.union[F, G]` and `<|>` test one side and take the other by
 * exclusion, so every tested signature is atomic.
 */
object TypeableK:

  /**
   * A TEST THAT READS THE OPERATION'S VALUE, and so tells two
   * instances of one signature apart.
   *
   * The default is the opposite: a test is the erasure, and a row may
   * hold ONE member of a signature (see `typeableKByClass`). An
   * instance that does better says so HERE, in its declared type,
   * because nothing else can be read by a macro — and `Distinct[R]`
   * reads exactly this to decide whether `Writer % String + Writer %
   * Int` is the good row it is, or the misroute that the same shape
   * over `Reader` would be.
   *
   * One instance in this tree carries it: `writerK`, whose test is
   * `Typeable[W]` on the told value. Marking a test that is NOT finer
   * than the class defeats the check for that signature, so mark it
   * only after reading the `unapply`.
   */
  trait ByValue[F[_]] extends TypeableK[F]

  /**
   * `enum Users[+A] derives TypeableK` — the instance every effect
   * needs, written by the compiler.
   *
   * No macro: a `ClassTag[F[Any]]` IS the erasure of F, which is what
   * `typeableK` wants, and the compiler synthesizes it for any
   * concrete signature. So this is the hand-written
   * `typeableK(classOf[Users[?]])` with the class no longer spelled
   * out — same instance, same totality (see `typeableK`: complete
   * when the answer type is the signature's only parameter, partial
   * for `State % S` and friends, which say so themselves).
   */
  inline def derived[F[_]](using ct: scala.reflect.ClassTag[F[Any]]): TypeableK[F] =
    ${ derivedImpl[F]('ct) }

  /** `Effect.derived`'s half of the same macro: the class is an
   * `Effect` already, so `derives Effect` needs no wrapper around a
   * `TypeableK` (it had one — `Effect.of(TypeableK.derived)` — which
   * put two virtual calls under every `split`) */
  inline def derivedEffect[F[_]](using ct: scala.reflect.ClassTag[F[Any]]): Effect[F] =
    ${ derivedImpl[F]('ct) }

  /**
   * The check is the reason this is a macro and not one line.
   *
   * A `ClassTag` of a UNION is its LUB, and a LUB is useless as a
   * test: measured, `ClassTag[(Choose + Writer % String)[Any]]` is
   * `interface java.io.Serializable` and `ClassTag[(Db + Writer %
   * String)[Any]]` is `interface scala.reflect.Enum` — classes every
   * operation in the program matches. A row derived this way would
   * send every operation left and say nothing, which is the failure
   * mode this library refuses on principle.
   *
   * A blacklist of such classes is whack-a-mole (the two above are
   * already different). The type says it exactly: refuse a union,
   * accept a signature. And a row does not need this anyway — the
   * generic instance below handles a composite row correctly, by
   * testing the parts.
   */
  def derivedImpl[F[_] : Type](ct: Expr[scala.reflect.ClassTag[F[Any]]])
                              (using Quotes): Expr[Effect[F]] =
    import quotes.reflect.*
    val body = TypeRepr.of[F].dealias match
      case tl: TypeLambda => tl.resType.dealias
      case other => other.appliedTo(TypeRepr.of[Any]).dealias
    body match
      case OrType(_, _) =>
        report.errorAndAbort(
          "TypeableK.derived is for ONE signature, and this is a row.\n" +
          "A ClassTag of a union is its LUB, a class every operation matches, so the\n" +
          "split would send all of them left and say nothing.\n" +
          "A row needs no instance of its own: let each signature derive one, and the\n" +
          "row split will find them.")
      case _ => '{ Effect.ByClass[F]($ct.runtimeClass) }


  /** the empty signature is trivially splittable: nothing inhabits
   * it, so the test never matches — which lets row-generic code
   * (Logic, the effectful streams) instantiate at F = Pure */
  given TypeableK[Pure] = new:
    def test(x: Any): Boolean = false

/**
 * WHAT A SIGNATURE SAYS ABOUT ITSELF: `enum Users[+A] derives Effect`.
 *
 * One word, and it reads as what it is — a declaration that this type
 * is an effect signature — where `derives TypeableK` reads as a
 * mechanism. What it currently carries is exactly the mechanism: a
 * row is an untagged union, unions erase, and a handler meeting an
 * operation in `F + G` decides by class test. `Effect` IS that test
 * (it extends `TypeableK`), so everything that asks for one finds
 * this instance in the signature's own companion.
 *
 * It also carries `Direct.Effect`, the marker that lets a signature's
 * operations auto-color inside a `direct` block:
 *
 *     val prog: Option[String] ! Users = direct {
 *       val old: Option[String] = find(7)   // no mark
 *       old
 *     }
 *
 * That marker was originally a separate, per-project decision
 * (specs/direct-auto-coloring.md): auto-coloring is invasive, so
 * arbitrary `G[A]`s must never silently color. Bundling it moves the
 * decision to the signature's author — which is the operator's call
 * (2026-09-08) and is defensible on its own terms: `derives Effect`
 * is not arbitrary, it is a type declaring that its values ARE
 * operations, which is exactly the claim the marker wanted. The other
 * gate is untouched and does the heavier work: the conversion needs
 * `DirectCtx[F]`, which exists ONLY inside a direct block, so nothing
 * colors anywhere else. An effect that wants the row-split test and
 * NOT auto-coloring writes `derives TypeableK` instead.
 *
 * It is a trait rather than a type alias so that it has room. What
 * joins it has to be DERIVABLE from the declaration alone, which
 * rules out most things and is the point.
 */
trait Effect[F[_]] extends TypeableK[F], Direct.Effect[F]

object Effect:
  /** delegates to `TypeableK`'s macro, which is where the check lives
   * that refuses a row */
  inline def derived[F[_]](using ct: scala.reflect.ClassTag[F[Any]]): Effect[F] =
    TypeableK.derivedEffect[F]

  /**
   * THE class test, as one class: what `derives Effect`, `derives
   * TypeableK` and `typeableK(cls)` all build. Named rather than
   * anonymous so the macro can name it, and so that a derived
   * signature's `test` is one call to `Class.isInstance` under
   * `split` — not a wrapper's call to a delegate's call.
   */
  final class ByClass[F[_]](cls: Class[?]) extends Effect[F]:
    def test(x: Any): Boolean = cls.isInstance(x)

  /** an `Effect` over a test that is NOT by class — `Instances.of`
   * and `Tag.of` read a key or an inner operation. Not inlined,
   * deliberately: an anonymous class in an inline body is duplicated
   * at every derivation site */
  def of[F[_]](t: TypeableK[F]): Effect[F] = new Effect[F]:
    def test(x: Any): Boolean = t.test(x)

/**
 * Split the union by testing only the F side (the erasure of F, by
 * TypeableK), taking G by exclusion: a type test on an abstract G
 * would erase to an always-true test.
 */
inline def <|>[F[+_], G[+_]](using T: TypeableK[F])[A](e: F[A] | G[A]): Either[F[A], G[A]] =
  // the trusted kernel, sound by the excluded middle of the union: a
  // value of F[A] | G[A] that passes F's test is an F[A], and one that
  // does not is a G[A]. `test` rather than the extractor
  // (split-without-either, 2026-09-09): the extractor answered an
  // Option per operation on top of this Either, and B/op showed both
  // survive escape analysis. The left cast is what the extractor's
  // `x.type & F[A]` said, made explicit; nothing outside this
  // function, `split` and `over` casts on a row.
  if T.test(e) then Left(e.asInstanceOf[F[A]]) else Right(e.asInstanceOf[G[A]])

/**
 * The same split with NO wrapper on the way out (split-without-either,
 * specs/handler-fusion.md stage A): `<|>` answers an `Either` per
 * operation and the extractor an `Option` per test, on the hottest
 * path of every runner. Here the two continuations are `inline`, so
 * they beta-reduce into the caller's match — no closure, no Either,
 * no Option — and the test is `TypeableK.test`, a plain class test for
 * a derived signature.
 *
 * Both casts live HERE — with `over`'s below, the reverse direction —
 * and nowhere else, licensed by the one test:
 * the left one is what the extractor's `x.type & F[A]` said, made
 * explicit; the right one is `<|>`'s excluded middle. A runner that
 * uses `split` still refines the answer type by matching the
 * constructor inside `onF` (`case Get() =>`), exactly as after
 * `case Left(...)` — so no cast reaches a runner.
 */
inline def split[F[+_], G[+_]](using T: TypeableK[F])[A, R]
                              (e: F[A] | G[A])
                              (inline onF: F[A] => R)
                              (inline onG: G[A] => R): R =
  if T.test(e) then onF(e.asInstanceOf[F[A]]) else onG(e.asInstanceOf[G[A]])

/**
 * Rewrite the operations of ONE member of a row in place and leave the
 * others as they are — a prism's modify, over the row: the class test
 * proves the operation IS an F, `f` keeps it an F at the same answer
 * type, and the row is erased, so the result goes back under the
 * row's type by the claim `split` makes, made once more here. Any
 * nesting, any position, an abstract row: what the test reads is the
 * OPERATION, not the shape. This is how a typeclass instance written
 * for one effect is lifted into an instance for every row that holds
 * it (`Failing.anyRow` over `Failing.async`).
 */
inline def over[F[+_], R[+_]](using T: TypeableK[F])[A]
                             (e: R[A])(inline f: F[A] => F[A]): R[A] =
  if T.test(e) then f(e.asInstanceOf[F[A]]).asInstanceOf[R[A]] else e

/**
 * The freer monad is the initial (defunctionalized) encoding of Effects:
 * Inject is a suspended shift, given its meaning by foldCont's !> interpretation.
 * Choose Free when the program is a thing: to step it, inspect it,
 * relay it in stages — and stay stack-safe on any bind shape.
 */
given Effects[Free] with
  override inline def pure[F[+_], A](a: A): Free[F, A] = Free.Pure(a)
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
      case Free.Pure(a) => a
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
      case Free.Pure(a) => ret(a)
      case Free.Inject(e) => last(e)
      case Free.Bind(Free.Inject(e), k) =>
        split[F, G](e)
          // `h` is asked ONCE: the answered test and the fallback both
          // read the same program, and a handler is not assumed pure
          (e => { val c = h(e)
                  Cont.onAnswer(c)(a => loop(k(a)))(capture(c, k)) })
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
inline def convert[M[_[+_], _] : Effects, N[_[+_], _] : Effects as N, F[+_], A]
                  (m: M[F, A]): N[F, A] =
  m.foldCont[N[F, A]]([X] => e => shift(k => N.perform(e).flatMap(k))) / (a => N.pure(a))

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

object ! {
  export Free.*

  import Free.*

  /** the domain name of Inject: an operation node */
  type Effect[F[+_], A] = Inject[F, A]
  val Effect = Inject

  extension [F[+_], A](self: A ! F) {

    /** `resume` is a MEMBER of `Free` now (Free.scala), where the
     * rotation and the invariant every `@unchecked` match relies on
     * are documented together. A member wins resolution, so every
     * `.resume` in the library reaches that one loop. */

    /** step through the next n operations by the Handler */
    @tailrec def next(steps: Long = 1)(using H: Handler[F]): A ! F = (self.resume: @unchecked) match
      case Bind(Effect(e), k) if steps > 0 => k(H.handle(e)).next(steps - 1)
      case a => a

    /** peek the nearest answer: the value, or the first operation handled */
    @tailrec def ? : Handler[F] ?=> ? = self match
      case Bind(a, _) => a.?
      case Effect(e) => summon[Handler[F]].handle(e)
      case Pure(a) => a
      // a peek forces the thunk too, same as `Bind(a, _) => a.?` discards
      // its own continuation without applying it
      case Delay(t) => t().?
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

  /** re-inject into a wider row: effect subsumption. Free is invariant
   * in its signature, so widening walks the tree — one re-injected
   * node per operation, deferred as it goes.
   *
   * The invariance is a CHOICE, and measured (free-row-variance,
   * 2026-09-03): `enum Free[+F[+_], A]` does pass the variance check
   * — F occurs only covariantly — and the row subtyping then holds at
   * concrete rows, which would delete this walk from every widening
   * call site. It was not taken, because deleting the walk makes
   * things SLOWER where it matters: the walk is also a NORMALIZATION,
   * and `Source.merge` without it runs 5-7% slower (specs/writer-
   * covariance.md), since the rotation it saves would otherwise be
   * paid per pull inside the merge's contended region. An upcast free
   * at the type level is not free operationally. */
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
  /**
   * `translate`, with the widening done for you — and this is the one
   * to reach for when the target row is BIGGER than the source's.
   *
   * `translate` interprets F into a row the program is already in.
   * Interpreting one effect into OTHERS means arriving somewhere new:
   * `A ! (Users + F)` becomes `A ! (State % Store + Writer % String +
   * F)`, where F is whatever the caller was already doing and is
   * carried through untouched. Written by hand that is a widen and a
   * translate and three type arguments; here the expected type solves
   * every row:
   *
   *     def tracked[A, F[+_]](p: A ! (Users + F)): A ! (Tracked + F) =
   *       !.interpret(p):
   *         [X] => (e: Users[X]) => e match
   *           case Users.Find(id) => ...   // a PROGRAM in Tracked + F
   *
   * (Not `interpr`, which builds a handler out of one. This rewrites
   * a program.)
   */
  def interpret[A, F[+_] : TypeableK, G[+_], H[+_]](prog: A ! (F + H))
                                                   (h: F ==> ([X] =>> X ! (G + H)))
  : A ! (G + H) =
    translate[A, F, G + H](prog.plus[G])(h)

  /**
   * RECORD what a program asks for, without answering any of it: each
   * operation of F is told to a `Writer` and then performed exactly
   * as before, so the row keeps F and gains `Writer % W`.
   *
   *     !.tracing(prog)([X] => (e: Users[X]) => e.toString)
   *       : A ! (Users + Writer % String + G)
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
  def tracing[A, F[+_] : TypeableK, W, G[+_]](prog: A ! (F + G))
                                             (show: [X] => F[X] => W)
  : A ! (F + Writer % W + G) =
    type R = F + Writer % W + G
    interpret[A, F, Writer % W, F + G](prog):
      [X] => (e: F[X]) =>
        Writer.tell(show(e)).at[R].flatMap(_ => effect[R, X](e))

  def translate[A, F[+_] : TypeableK, G[+_]](prog: A ! (F + G))
                                            (h: F ==> ([X] =>> X ! G)): A ! G =
    // every step suspends under a flatMap (the answer is a PROGRAM,
    // not a value), so the recursion lives in closures rather than on
    // the stack — the State.handle shape, and the reason no @tailrec
    // annotation belongs here
    // `split`, not `<|>`: no Either per operation (core-cleanup); the
    // recursion is not a loop, so the inlined arms cost no inlining
    // budget the way they would inside `relay`
    (prog.resume: @unchecked) match
      case Pure(a) => Pure(a)
      case Effect(e) => split[F, G](e)(f => h(f))(g => Effect(g))
      case Bind(Effect(e), k) =>
        // the Bind node types e and k together
        split[F, G](e)
          (f => h(f).flatMap(x => translate[A, F, G](k(x))(h)))
          (g => Effect(g).flatMap(x => translate[A, F, G](k(x))(h)))

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
      split[F, G](e)(e => g(e) / f)(e => Effect(e).flatMap(f))

    @tailrec def loop(x: A ! F + G): B ! G = (x.resume: @unchecked) match
      // `g(e) / k`, not `g(e)(k)`: the Cont carrier's application is
      // `/` since Cont became a facade over Free (specs/freer-base.md)
      case Bind(Effect(e), k) => split[F, G](e)(e => loop(g(e) / k))(e => Effect(e).flatMap(x => relay[A, B, F, G](k(x))(f)(g)))
      case Effect(e) => last(e)
      case Pure(a) => f(a)

    loop(a)
  }

}

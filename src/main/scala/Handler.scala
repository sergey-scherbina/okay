package okay

import scala.annotation.implicitNotFound
import scala.quoted.*

/**
 * What a signature says about itself, and the tiny capability surface
 * that lets it say it to the optional direct DSL too — the runtime
 * evidence a row split needs (`TypeableK`), the marker that promotes
 * it to an effect (`Effect`), and the two-type contract
 * (`DirectEffect`/`DirectCtx`) `Effect` carries so that `okay` stays
 * usable without `okay-direct`: no macro or runtime implementation
 * lives here, only the capability `okay-direct`'s bridge exposes this
 * same evidence through. `Handler`, below, is what actually PERFORMS
 * an effect once split — the two live in one file because `Handler
 * .union` reaches for `TypeableK`/`split` directly, not just by
 * convention.
 */

/** The tiny capability surface shared by the effect kernel and the
 * optional direct DSL. It deliberately contains no macro or runtime
 * implementation, so `okay` remains usable without `okay-direct`. */
@implicitNotFound("no Direct.Effect[${F}]: auto-coloring is OPT-IN per signature.\nRegister the effect once — `given Direct.Effect[${F}] with {}` — or use the explicit marks\n(.reflect / .? / !prog), which need no marker.")
trait DirectEffect[F[_]]

/** Evidence installed only while a `direct` block is being compiled. */
@implicitNotFound("no DirectCtx[${F}]: auto-coloring works only INSIDE a direct block.\nWrap the code in direct[F] { ... } — or use the explicit marks (.reflect / .? / !prog),\nwhich need no capability.")
final class DirectCtx[F[_]] private[okay] ()

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
 * (`TypeableK.ByValue`) and is allowed to repeat; `Writer.byValue.writerK`
 * is the one that does, an opt-in — Writer's DEFAULT test is the class
 * of `Say`, total and warning-free (writer-typeablek-by-class). Two — `Reader % Int + Reader % String` — misroute, and
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
   * One instance in this tree carries it: `Writer.byValue.writerK`,
   * whose test is `Typeable[W]` on the told value — an OPT-IN
   * (`import okay.Writer.byValue.given`), since the default `writerK`
   * tests the class of `Say` alone and pays no E092 for it. Marking a
   * test that is NOT finer
   * than the class defeats the check for that signature, so mark it
   * only after reading the `unapply`.
   */
  trait ByValue[F[_]] extends TypeableK[F]

  /**
   * `enum Users[+A] derives TypeableK` — the instance every effect
   * needs, written by the compiler.
   *
   * The erasure of F is what the test is, and the macro reads it off
   * the type — so this is the hand-written `typeableK(classOf[Users[?]])`
   * with the class no longer spelled out AND compiled to a constant
   * `instanceof` rather than read from a field (see `derivedImpl`) —
   * same totality (see `typeableK`: complete when the answer type is
   * the signature's only parameter, partial for `State % S` and
   * friends, which say so themselves).
   */
  inline def derived[F[_]]: TypeableK[F] =
    ${ derivedImpl[F] }

  /** `Effect.derived`'s half of the same macro: the class is an
   * `Effect` already, so `derives Effect` needs no wrapper around a
   * `TypeableK` (it had one — `Effect.of(TypeableK.derived)` — which
   * put two virtual calls under every `split`) */
  inline def derivedEffect[F[_]]: Effect[F] =
    ${ derivedImpl[F] }

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
  def derivedImpl[F[_] : Type](using Quotes): Expr[Effect[F]] =
    import quotes.reflect.*
    val body = TypeRepr.of[F].dealias match
      case tl: TypeLambda => tl.resType.dealias
      case other => other.appliedTo(TypeRepr.of[Any]).dealias
    body match
      case OrType(_, _) =>
        report.errorAndAbort(
          "TypeableK.derived is for ONE signature, and this is a row.\n" +
          "The erasure of a union is its LUB, a class every operation matches, so the\n" +
          "split would send all of them left and say nothing.\n" +
          "A row needs no instance of its own: let each signature derive one, and the\n" +
          "row split will find them.")
      case _ =>
        // the signature's class with every argument a wildcard — what
        // `x.isInstanceOf[Users[?]]` tests. Emitted as a class of its
        // own per `derives` site (one per signature) so that the test
        // is a CONSTANT-class `instanceof` in the bytecode, where
        // `ByClass` reads its class from a field and calls
        // `Class.isInstance` (typeablek-instanceof: the residual of
        // handler-fusion-flat, 5.6% on a lane that is nothing but
        // dispatch). `ByClass` stays for `typeableK(cls)`, whose class
        // is a run-time value.
        val erased = body match
          case AppliedType(tycon, args) => AppliedType(tycon, args.map(_ => TypeBounds.empty))
          case other => other
        if !erased.typeSymbol.isClassDef then
          report.errorAndAbort(s"TypeableK.derived needs a class to test for, and ${erased.show} is not one")
        erased.asType match
          case '[t] => '{ new Effect[F] { def test(x: Any): Boolean = x.isInstanceOf[t] } }


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
 * When `okay-direct` is present, its bridge exposes this same evidence
 * as the marker that lets a signature's
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
trait Effect[F[_]] extends TypeableK[F], DirectEffect[F]

object Effect:
  /** delegates to `TypeableK`'s macro, which is where the check lives
   * that refuses a row */
  inline def derived[F[_]]: Effect[F] =
    TypeableK.derivedEffect[F]

  /**
   * THE class test over a RUN-TIME class: what `typeableK(cls)`
   * builds. A derived signature (`derives Effect`/`TypeableK`) no
   * longer uses it — its test is a constant-class `instanceof` in a
   * class of its own (typeablek-instanceof), which this cannot be:
   * `cls` is a field.
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
 * would erase to an always-true test. The `Either` form, for drains
 * and tests, where `case Left(a) => ... case Right(Say(w)) => ...`
 * reads better than two lambdas and the wrapper is scalar-replaced
 * anyway (split-over-either measured it byte-identical on every such
 * walker). It IS `split` at `Left` and `Right` — the operator's
 * proposal (either-via-split, 2026-09-16) — so the union's two casts
 * live in one function below, and these inline lambdas beta-reduce to
 * the same bytes the hand-written test had.
 */
inline def <|>[F[+_], G[+_]](using T: TypeableK[F])[A](e: F[A] | G[A]): Either[F[A], G[A]] =
  split[F, G](e)(Left(_))(Right(_))

/**
 * THE trusted kernel: the union split with NO wrapper on the way out
 * (split-without-either, specs/handler-fusion.md stage A), on the
 * hottest path of every runner. The two continuations are `inline`,
 * so they beta-reduce into the caller's match — no closure, no
 * Either, no Option — and the test is `TypeableK.test`, a plain class
 * test for a derived signature.
 *
 * Sound by the excluded middle of the union: a value of `F[A] | G[A]`
 * that passes F's test is an `F[A]`, and one that does not is a
 * `G[A]`. Both casts live HERE — with `over`'s below, the reverse
 * direction, which no split can express — and nowhere else, licensed
 * by the one test: the left one is what the old extractor's `x.type &
 * F[A]` said, made explicit; the right one is the excluded middle.
 * `<|>` above is this at `Left`/`Right`. A runner that uses `split`
 * still refines the answer type by matching the constructor inside
 * `onF` (`case Get() =>`), exactly as after `case Left(...)` — so no
 * cast reaches a runner.
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

/** an interpretation of F into any Control carrier C, with the answers
 * S — the handler type of an inline handler-passing program
 * (specs/staged-effects.md; the measured probes are `Fused` in the
 * test sources), which is what "staged effects" means here:
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

  /**
   * The same row handler as `union`, assembled by a macro into ONE
   * dispatch expression. The row is read APPLIED (the `Distinct` trick:
   * only a union's body is an `OrType`, whatever the call site's
   * spelling), its members taken in row order, and `handle` emitted as
   * `if T1.test(a) then h1.handle(a) else if T2.test(a) … else hk.handle(a)`
   * — the last member by exclusion. Those are exactly the tests the
   * nested `union` chain performs; what is gone is the k−1 handler
   * OBJECTS between the test and the answer and their virtual `handle`
   * calls. Measured (handler-fusion-flat, 2026-09-22, FlatDispatchBenchmark,
   * a four-effect row, bytes identical on every lane): this macro is
   * 1.08x over the nested chain at position 4 and at parity at
   * position 1; the hand-written flat match that CALLS the four
   * handlers is 1.14x, and the one that inlines their bodies 1.24x —
   * which no macro over opaque `Handler` givens can reach. The 5%
   * between this and the calling form is the test: `Class.isInstance`
   * through a field against a constant-class `instanceof`
   * (specs/handler-fusion.md, and BACKLOG typeablek-instanceof).
   *
   * Every member needs a `Handler` in scope and every member but the
   * last a `TypeableK`; a missing one is a compile error naming the
   * member. `Distinct[R]` is required as for `union`, for the same
   * reason: a class test cannot tell two `Reader % _` apart.
   */
  inline def flat[R[+_]](using Distinct[R]): Handler[R] = ${ flatImpl[R] }

  /** public because an inline def's splice reaches it from outside
   * (E192, "unstable inline accessor"), as `Distinct.impl` */
  def flatImpl[R[+_] : Type](using q: Quotes): Expr[Handler[R]] =
    import q.reflect.*

    def members(t: TypeRepr): List[TypeRepr] = t.dealias match
      case OrType(l, r) => members(l) ++ members(r)
      case m => List(m)

    /** `F[Any]` back to `F`: the constructor itself when `Any` is its
     * only argument, a lambda over the last argument otherwise —
     * `(Writer % W)[Any]` is `Writer[W, Any]`, `Tag.Of[K, F][Any]` is
     * `Tag[K, F, Any]` */
    def constructor(m: TypeRepr): TypeRepr = m.dealias match
      case AppliedType(tc, args) if args.nonEmpty && args.last =:= TypeRepr.of[Any] =>
        if args.size == 1 then tc
        else TypeLambda(List("A"), _ => List(TypeBounds.empty),
          tl => AppliedType(tc, args.init :+ tl.param(0)))
      case other =>
        report.errorAndAbort(s"Handler.flat: ${other.show} is not an effect signature applied to Any")

    val parts = members(TypeRepr.of[R[Any]]).map(constructor)
    if parts.sizeIs < 2 then
      report.errorAndAbort(s"Handler.flat: ${TypeRepr.of[R].show} is not a row (one member — use its Handler directly)")

    /** a member, its handler and (all but the last) its test, BOUND to
     * vals outside the handler object so that each is evaluated once
     * and captured as a field — the first cut spliced the givens
     * straight into `handle`, and a `given x: T = …` in a class body
     * is a lazy val, so every operation paid its accessor: measured no
     * faster than the nested chain it replaced (inline4 106.5 µs
     * against union4 109.1, and SLOWER at position 1) */
    type Bound = (TypeRepr, Term, Option[Term])

    /**
     * THE ONE CAST, emitted once per member: `split`'s claim, made at
     * the same kind of site. The test that guards the branch proves
     * the operation is this member's; for the last member, every
     * other test having failed proves it. No other cast is emitted.
     */
    def chain[A: Type](a: Expr[R[A]], bs: List[Bound]): Expr[A] = bs match
      case (m, hT, tO) :: rest => m.asType match
        case '[type f[x]; f] =>
          val h = hT.asExprOf[Handler[f]]
          tO match
            case None => '{ $h.handle($a.asInstanceOf[f[A]]) }
            case Some(tT) =>
              val t = tT.asExprOf[TypeableK[f]]
              '{ if $t.test($a) then $h.handle($a.asInstanceOf[f[A]]) else ${ chain[A](a, rest) } }
      case Nil => report.errorAndAbort("Handler.flat: empty row")

    def build(ms: List[TypeRepr], bound: List[Bound]): Expr[Handler[R]] = ms match
      case m :: rest => m.asType match
        case '[type f[x]; f] =>
          val h = Expr.summon[Handler[f]].getOrElse(
            report.errorAndAbort(s"Handler.flat: no Handler[${m.show}] in scope"))
          if rest.isEmpty then
            '{ val hv: Handler[f] = $h; ${ build(Nil, bound :+ (m, 'hv.asTerm, None)) } }
          else
            val t = Expr.summon[TypeableK[f]].getOrElse(
              report.errorAndAbort(s"Handler.flat: no TypeableK[${m.show}] in scope"))
            '{ val hv: Handler[f] = $h; val tv: TypeableK[f] = $t
               ${ build(rest, bound :+ (m, 'hv.asTerm, Some('tv.asTerm))) } }
      case Nil =>
        '{ new Handler[R] { def handle[A](a: R[A]): A = ${ chain[A]('a, bound) } } }

    build(parts, Nil)
}

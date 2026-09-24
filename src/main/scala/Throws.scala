package okay

import scala.reflect.*
import scala.util.*
import scala.annotation.implicitNotFound
import okay.Row.{In, at}

/**
 * Errors on two levels, with the bridges between them.
 *
 * The Throws effect fails with any E, and a handler decides what
 * failing means: runEither reifies it into Either, runThrows into the
 * throws union, runUnsafe into a JVM throw — so the JVM exception
 * mechanism is just one of the handlers. A throws E is the direct-style
 * union of a value and a JVM-compatible error (E <: Unsafe, so that
 * erasure can tell them apart), and catching reflects such a
 * computation back into the effect, closing the circle.
 */

/** the effect of failing with E */
/**
 * PARAMETERISED, so the derived test is by CLASS only: the operations
 * carry no runtime trace of E, and a row may therefore hold ONE
 * Throws. Two — `Throws % A + Throws % B` — misroute, loudly
 * (TestRowIdentity): the first handler answers both asks and the
 * second continuation gets a ClassCastException, rather than a
 * plausible wrong answer.
 */
case class Throws[E, +A](e: E) derives Effect

/** perform the failure */
inline def raise[E, A](e: E): A ! Throws % E = effect(Throws(e))

/** handle Throws by aborting into Either, forwarding the effects F */
inline def runEither[A, F[+_], E](a: A ! Throws % E + F)(using Distinct[Throws % E + F]): Either[E, A] ! F =
  // `B` pinned: with `Free[F, +A]` the first lambda alone would infer
  // it as `Right[E, A]` (free-answer-variance, 2026-09-23)
  Effects[Free].handle[Throws % E, F](a)(a => pure[F, Either[E, A]](Right(a))):
    [X] => e => shift(_ => pure[F, Either[E, A]](Left(e.e)))

/** handle Throws into the throws union (an Either already is one) */
inline def runThrows[A, F[+_], E <: Unsafe](a: A ! Throws % E + F)(using Distinct[Throws % E + F]): (A throws E) ! F =
  runEither(a).map(e => e)

/**
 * FAILURE WITH NOTHING TO SAY. Not every failure carries a reason: a
 * lookup that found nothing, a pattern that did not match, a guard
 * that did not hold. `Throws % Unit` is exactly that error, and
 * because it is an ordinary Throws it needs no new node, no new
 * handler and no new interpreter — only names.
 *
 * The Option does not disappear, it MOVES: out of every signature
 * along the way and into the handler at the end. That is what makes
 * `for case Some(x) <- p` work (see CanFail below) — the pattern says what
 * happens when the value is not there, and the type says the program
 * may stop.
 */
type Abort = Throws % Unit

/** stop: nothing to answer with */
inline def abort[A]: A ! Abort = raise[Unit, A](())

/** handle Abort into Option, forwarding the effects F */
inline def runOption[A, F[+_]](a: A ! Abort + F): Option[A] ! F =
  runEither[A, F, Unit](a).map(_.toOption)

/** handle Throws by actually throwing: the JVM is the handler */
inline def runUnsafe[A, F[+_], E <: Unsafe](a: A ! Throws % E + F)(using Distinct[Throws % E + F]): A ! F =
  Effects[Free].handle[Throws % E, F](a)(a => pure(a)):
    [X] => e => shift(_ => throw e.e)

/**
 * RECOVERY, in the row rather than around it: run the alternative
 * when the first program raises.
 *
 *     load(id).orElse(pure(default))
 *     load(id).recover(e => pure(fallbackFor(e)))
 *
 * Both are `runEither` applied to a PART of the program instead of to
 * all of it — the handler is installed here, the failure is answered
 * here, and the row comes out unchanged, so what follows neither knows
 * nor cares that anything went wrong. `orElse` is `recover` for the
 * case where the error had nothing to say, which is every `Abort`.
 *
 * NOT free, and the cost is where you would guess: one handler per
 * call, over the program it guards. Wrap the smallest piece that can
 * fail, not the whole thing.
 *
 * This is the Alternative structure of a failing row (`empty` is
 * `abort`, `append` is this), spelled as methods rather than as an
 * instance. An instance would not be FOUND: matching
 * `Alternative[[A] =>> A ! Throws % E + F]` against a concrete row
 * is the higher-order unification the compiler declines (see
 * CanFail below). A method's receiver is unified, not searched for, and
 * that does work — including with the row written in the other order.
 */
extension [A, E, F[+_]](p: A ! Throws % E + F)
  /** answer the failure, seeing the error */
  def recover(h: E => A ! Throws % E + F): A ! Throws % E + F =
    runEither[A, F, E](p).at[Throws % E + F].flatMap(_.fold(h, pure))

  /** answer the failure, ignoring the error */
  inline def orElse(q: => A ! Throws % E + F)(using Distinct[Throws % E + F]): A ! Throws % E + F =
    recover(_ => q)

/** reflect a direct-style computation into the effect */
inline def catching[A, E <: Unsafe : Typeable](a: => A throws E): A ! Throws % (E | Unsafe) =
  unsafe(a).wrap match
    case Left(e) => raise(e)
    case Right(x) => pure(x)

/** no error: A throws Safe always holds a value */
type Safe = Nothing

/** any JVM error */
type Unsafe = Throwable

/**
 * The direct-style union of a value and a JVM-compatible error:
 * a plain A, a raw error E, an Either or a Try are all accepted as
 * they are (see the Conversion givens below) and only normalized
 * on elimination by wrap.
 *
 * `into` (Scala 3.9, throws-into): the four conversions BELOW may
 * then be applied without the caller writing
 * `import scala.language.implicitConversions`. That import is the
 * language asking for consent per CALL SITE; `into` is the same
 * consent given once, here, by the type that was designed to absorb
 * those four shapes and nothing else. It is written on exactly one
 * type in this repository, which is the discipline the feature asks
 * for — `Json` was measured as a candidate (850 construction sites)
 * and REFUSED, because a conversion from String would turn an
 * already-serialized document into a string literal silently.
 */
into opaque infix type throws[+A, +E <: Unsafe] =
  A | E | Either[E, A] | Try[A]

/**
 * for-comprehension in the direct style — `import throws.*` where
 * used. The extensions live in the companion of the union on purpose:
 * at the package level they would capture foreign flatMaps through
 * the Conversion givens below — the local import wins.
 */
object throws {
  /** the union absorbs values, errors, Either and Try as they are */
  given [A, E <: Throwable] => Conversion[A, A throws E] = identity
  given [A, E <: Throwable] => Conversion[E, A throws E] = identity
  given [A, E <: Throwable] => Conversion[Either[E, A], A throws E] = identity
  given [A, E <: Throwable] => Conversion[Try[A], A throws E] = identity

  // The ELIMINATING conversion that used to stand here —
  // `Conversion[A throws E, Either[E | Unsafe, A]] = _.wrap` — is
  // gone (throws-into). Its target is `Either`, which is not ours to
  // mark `into`, so it was the one thing still demanding the language
  // import from every caller; and it was redundant, its body being
  // literally `.wrap`, which is public, is what `??` calls, and is
  // used explicitly 38 times in this repository. Eliminating is an
  // ACT here, not a coercion: `a.wrap` or `a.??`.

  /**
   * THE ONE-GLYPH ELIMINATORS, and they live HERE for the same reason
   * `map` and `flatMap` do, one paragraph up: at package level they
   * capture foreign receivers through the Conversion givens above.
   *
   * WHAT MOVING THEM BUYS, and it is better than the import this
   * comment first claimed. `object throws` is the COMPANION of the
   * opaque type, so these extensions are in the implicit scope of
   * `A throws E` itself — a genuine receiver finds them with nothing
   * imported, exactly as before. A receiver whose actual type is
   * `Int` does not, because the implicit scope is the RECEIVER's, not
   * the conversion target's. Measured: zero call sites in this
   * repository had to change, and `42.?` stopped compiling.
   *
   * For `map` that capture was a contest it could lose. For this one
   * it was SILENT. `throws` is `into`, so every value in the language
   * is an `A throws E`, `42.?` type-checked, and the glyph answered
   * it unchanged. It cost an hour: a `direct` block written with `.?`
   * — because specs/direct-macro.md's Interface still documented that
   * as the block's mark — compiled, ran and answered correctly
   * through auto-coloring while the glyph did nothing at all
   * (specs/unwrap-glyph.md carries the incident and the plan; the
   * marks are `.reflect`, `.!?` and prefix `!p`).
   *
   * A TYPE-LEVEL GUARD WAS TRIED FIRST AND REFUTED, which is why this
   * is a move and not a side condition. `NotGiven[E =:= Nothing]`
   * reads plausibly — a value that cannot throw has nothing to unwrap
   * — but `throws` is COVARIANT in `E`, so the typer solves `E` to
   * its upper bound. The refusal message for a GENUINE receiver said
   * it outright: `okay.?[A, okay.Unsafe](y)` for a `String throws
   * Safe`. `E` is `Unsafe` for a converted receiver too, so no
   * condition on `E` can tell the two apart.
   *
   * The SCOPE can, and it costs nobody anything: absorbing a value
   * stays a coercion (that is what `into` is for), while eliminating
   * one is reached through the type that was actually written.
   */
  extension [A, E <: Unsafe](a: A throws E)
    /** the value, or the error thrown */
    inline def ? : A = a.unwrap

    /**
     * The value, mending an error by f.
     *
     * It moves WITH the no-arg form, and the reason was measured
     * rather than assumed: with only the no-arg one gone, `x.?`
     * stopped being a no-op and started eta-expanding to THIS —
     * `(Throwable => Int) => Int`, a function nobody asked for,
     * reported by munit as "can't compare these two types". One
     * silent shape replaced by another is not a fix. Both glyphs
     * live behind the import; `handle` is the same thing with a name
     * and stays outside it.
     */
    inline def ?(f: E | Unsafe => A): A = a.handle(f)

  extension [A, E <: Unsafe](a: A throws E)
    // The type ARGUMENTS of Either and Try are erased, so these tests
    // are by class only — and complete anyway, because the union `A
    // throws E` is built from exactly these four shapes: an Either in
    // it can only be an Either[E, A]. `@unchecked` marks precisely
    // that, at the test it applies to, rather than silencing the file.
    def flatMap[B](f: A => B throws E): B throws E = a match
      case e: (Either[E, A] @unchecked) => e.fold(e => e, f)
      case e: (Try[A] @unchecked) => e.fold(t => Failure(t), f)
      case e: (E @unchecked) => e
      case x: (A @unchecked) => f(x)
    inline def map[B](f: A => B): B throws E = flatMap(a => f(a))
}

/** evaluate, catching a thrown E as is and any other Throwable as a Failure */
inline def unsafe[A, E <: Unsafe : Typeable](a: => A throws E): A throws E =
  try a catch {
    case e: E => e
    case e => Failure(e)
  }

extension [A, E <: Unsafe](a: A throws E)
  /** normalize to Either */
  inline def ?? : Either[E | Unsafe, A] = wrap
  def wrap: Either[E | Unsafe, A] = a match {
    // by class, and complete by the union's construction — see flatMap
    case e: (Either[E, A] @unchecked) => e
    case e: (Try[A] @unchecked) => e.toEither
    case e: (E @unchecked) => Left(e)
    case x: (A @unchecked) => Right(x)
  }

  inline def handle(f: E | Unsafe => A): A = wrap match {
    case Left(e) => f(e)
    case Right(x) => x
  }

  @scala.throws[Unsafe]("unwrap unsafe")
  def unwrap: A = a match {
    // by class, and complete by the union's construction — see flatMap
    case e: (Either[E, A] @unchecked) => e.fold(throw _, identity)
    case e: (Try[A] @unchecked) => e.get
    case e: (E @unchecked) => throw e
    case x: (A @unchecked) => x
  }

/** by class only: the payload `e: E` is erased in the type, so a row
 * may hold ONE Throws — see typeableKByClass */

/**
 * The seam direct-try stands on: how a monad CATCHES a JVM throw
 * from the computation's own code. Strict monads (Option, Either,
 * List) run at construction, so a try around the value covers
 * everything; a Free row runs LATER under its handlers, so the
 * instance guards the continuations through the tree — a throw from
 * a pure segment between effects lands in the handler, while a
 * throw from inside an effect's HANDLER stays that handler's
 * business (stated, not hidden). A context function (`E ?=> X`,
 * direct-try-ctx) is lazy a THIRD way — a closure over its
 * environment, not run until applied — so its instance defers the
 * try to APPLICATION time (`ctxFn` below), the honest counterpart to
 * the Free row's per-step guard. The instances are NAMED, not a
 * catch-all: a lazy monad given the STRICT instance would try the
 * CONSTRUCTION and never the run, and the catch would silently never
 * fire — so an F without an instance is a compile error that says
 * so, and a strict monad of your own declares itself in one line:
 * `given CanTry[M] = CanTry.strict`.
 */
@implicitNotFound("no CanTry[${F}]: `try` in a direct block needs to know how ${F} catches a throw.\nStrict monads (Option, Either, List, Vector, Try), Free rows, and context functions (E ?=> X)\nhave instances; for a strict monad of your own declare `given CanTry[${F}] = CanTry.strict` — a\nCont-shaped LAZY monad has no honest instance: its body runs after the try, catch in the run instead.")
trait CanTry[F[_]]:
  def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A]

object CanTry:
  import okay.!.*
  /** strict monads: the whole computation happens at construction */
  def strict[F[_]]: CanTry[F] = new:
    def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A] =
      try fa catch case e: Throwable => h(e)

  given option: CanTry[Option] = strict
  given either: [E] => CanTry[[X] =>> Either[E, X]] = strict
  given list: CanTry[List] = strict
  given vector: CanTry[Vector] = strict
  given tries: CanTry[Try] = strict
  /** direct-try-ctx: a context function is lazy in its environment
   * (a closure, not run until applied), so the try must defer to
   * APPLICATION time, not construction — the honest counterpart to
   * `rows`' per-step guard. Also sidesteps the dotty 3.7.4 erasure
   * crash ("bad adapt for M\$proxy2.pure(a)") the STRICT shape hit
   * when tried here during the 2026-09-02 audit — a different
   * generated-code shape, not a version bump */
  given ctxFn: [E] => CanTry[[X] =>> E ?=> X] = new:
    def tryIn[A](fa: => (E ?=> A))(h: Throwable => (E ?=> A)): E ?=> A =
      (e: E) ?=> (try fa(using e) catch case ex: Throwable => h(ex)(using e))
  /** Free rows: guard construction AND every continuation step */
  given rows: [Fx[+_]] => CanTry[[X] =>> X ! Fx] = new:
    def tryIn[A](fa: => A ! Fx)(h: Throwable => A ! Fx): A ! Fx =
      def step(p: () => A ! Fx): A ! Fx =
        (try Right(p().resume) catch case e: Throwable => Left(e)) match
          case Left(e) => h(e)
          // the stack's convention: resume answers one of three shapes
          case Right(head) => (head: @unchecked) match
            case Return(a) => Free.Return(a)
            case Inject(op) => Free.Inject(op)
            case Bind(Inject(op), k) => Free.Bind(Free.Inject(op), x => step(() => k(x)))
      step(() => fa)

/**
 * A step a program may DECLINE — which is what a refutable pattern on
 * the left of `<-` asks for, and what an `if` guard asks for too.
 *
 *   for case Some(old) <- find(id)
 *       _              <- save(id, to)
 *   yield old
 *
 * Scala desugars both into `withFilter`, so neither is about patterns
 * or about booleans: the question each one asks is whether this
 * program may stop early. A plain `A ! F` may not — nothing in `Free`
 * declines to answer — and it must not pretend to, because a silently
 * skipped step is a bug that reads like a feature. So the witness
 * below is the whole design, and everything else follows from which
 * effect supplies it.
 *
 * TWO EFFECTS CAN FAIL, AND THEY MEAN DIFFERENT THINGS.
 *
 *   Choose   the BRANCH dies and the search goes on — `guard`, under
 *            the syntax people reach for first
 *   Abort    the PROGRAM stops and `runOption` answers None — a lookup
 *            with nothing to look up, which has no other branch to
 *            continue into
 *
 * A row carrying `Choose` gets the search meaning: where a program
 * searches, `guard` already means prune, and one syntax must not mean
 * two things in one row.
 *
 * WHY MEMBERSHIP AND NOT `MonadPlus`. The obvious evidence is
 * `MonadPlus[[X] =>> X ! F]`, and it was shipped that way first. It
 * does not work: the instance for `Choose + F` never matches a
 * concrete row, because unifying `[A] =>> Choose[A] | F[A]` against
 * `[A] =>> Choose[A] | (Writer % String)[A]` is a higher-order
 * unification the compiler declines — it finds the given and reports
 * "does not match". Measured, not assumed: the first version bound
 * patterns in the bare `Choose` row and nowhere else, which is the
 * row nobody writes.
 *
 * What DOES resolve is membership by shape, the witness `.at` already
 * uses. So the evidence for "this row may drop a step" is that the row
 * CONTAINS an effect that can fail, and nothing larger.
 */
@implicitNotFound("""this row cannot drop a step, so a refutable pattern (`case Some(x) <- p`) and an `if` guard have no meaning in it: ${F}
Both desugar to withFilter, which needs somewhere for the dropped step to GO.
Put a failing effect in the row and it works: Abort (the program stops, runOption answers None) or Choose (the branch dies, the search goes on).
Or keep the row as it is and write the branch yourself: `old.fold(pure(()))(...)` says the same thing and hides nothing""")
trait CanFail[F[+_]]:
  def fail[A]: A ! F

trait CanFailLow:
  /** stop: the rest of the program does not run */
  given viaAbort: [F[+_]] => In[Abort, F] => CanFail[F] = new:
    def fail[A]: A ! F = abort[A].at[F]

object CanFail extends CanFailLow:
  /** prune: this branch dies, the search continues — and it wins over
   * Abort where a row carries both, because in a searching row `guard`
   * already means prune */
  given viaChoose: [F[+_]] => In[Choose, F] => CanFail[F] = new:
    def fail[A]: A ! F = effect[Choose, A](Choose(Seq.empty)).at[F]

extension [A, F[+_]](p: A ! F)
  /** the desugaring target of a pattern bind and of an `if` guard */
  def withFilter(q: A => Boolean)(using C: CanFail[F]): A ! F =
    p.flatMap(a => if q(a) then pure[F, A](a) else C.fail[A])

/**
 * The same demand as an `if` guard, outside a for-comprehension: hold
 * or stop.
 *
 *     _ <- ensure[R](balance >= amount)
 *
 * `guard` (Monad.scala) is the MonadPlus one and covers any carrier
 * with an algebra — LazyList, a bare Choose row. This one covers what
 * that cannot: a ROW, identified by membership, which is how every
 * real row is known. Same three meanings as the pattern: prune where
 * the row searches, stop where it can abort, a compile error where it
 * can do neither.
 */
inline def ensure[F[+_]](p: Boolean)(using C: CanFail[F]): Unit ! F =
  if p then pure[F, Unit](()) else C.fail[Unit]

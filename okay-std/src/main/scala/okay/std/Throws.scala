package okay.std

import okay.{!, %, Effect, Handler, Within, effect, pure}
import okay.freer.Replayable
import okay.core.Split


import scala.reflect.*
import scala.util.*
import scala.annotation.implicitNotFound

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
 * PARAMETERISED, and the operations carry no runtime trace of E. On the
 * kernel's rows that does not matter: an operation goes to its handler
 * by its PATH, so `Throws % A + Throws % B` are two places, each raise
 * going to its own handler — no misroute to forbid (okay-freer's
 * `Distinct` and `TestRowIdentity` were the union's).
 */
case class Throws[E, +A](e: E) extends Final, Replayable.Safe derives Effect

object Throws:
  /** the handler as a value: `p.handle(Throws.either)` answers `Either[E, A]` — a raise ENDS the body, its
   * `Left` in the body's place, the rest dropped (the kernel's `Aborting`) */
  def either[E]: Handler[Throws % E, [A] =>> Either[E, A]] =
    Handler.aborting[Throws % E, [A] =>> Either[E, A]]([A] => (a: A) => Right(a))(
      [A, X] => (t: Throws[E, X]) => Left(t.e))

/** perform the failure */
inline def raise[E, A](e: E): A ! Throws % E = effect[Throws % E, A](Throws[E, A](e))

/** handle Throws by aborting into Either, wherever Throws is among the program's effects: the rest of them
 * forwarded */
def runEither[A, Es[_], E](a: A ! Es)(using s: Split[Es, Throws % E]): Either[E, A] ! s.Rest =
  a.handle(Throws.either[E])

/** handle Throws into the throws union (an Either already is one) */
def runThrows[A, Es[_], E <: Unsafe](a: A ! Es)(using s: Split[Es, Throws % E]): (A throws E) ! s.Rest =
  runEither[A, Es, E](a)(using s).map(e => e)

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

/** handle Abort into Option, wherever it is among the program's effects: the rest of them forwarded */
def runOption[A, Es[_]](a: A ! Es)(using s: Split[Es, Abort]): Option[A] ! s.Rest =
  runEither[A, Es, Unit](a)(using s).map(_.toOption)

/**
 * AN EITHER INTO THE EFFECT: the `Right` is the answer, the `Left` is
 * raised. What a library hands back as `Either[E, A]` joins a program
 * without the `fold(raise, pure)` every call site used to spell out
 * (specs/core-gaps.md).
 *
 *     parse(s).orRaise   :  Int ! Throws % String
 */
extension [E, A](e: Either[E, A])
  inline def orRaise: A ! Throws % E = e match
    case Right(a) => pure[A, Throws % E](a)
    case Left(err) => raise[E, A](err)

/** handle Throws by actually throwing: the JVM is the handler — the raise ends the body, thrown where the handler
 * is reached (the kernel's `Aborting`) */
def runUnsafe[A, Es[_], E <: Unsafe](a: A ! Es)(using s: Split[Es, Throws % E]): A ! s.Rest =
  a.handle(Handler.aborting[Throws % E, [A] =>> A]([A] => (a: A) => a)([A, X] => (t: Throws[E, X]) => throw t.e))

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
 * instance. The receiver is any program; `Throws % E` is found among
 * its effects wherever it is (`Split`, which names `E` too — so the
 * evidence comes FIRST, before the handler or the alternative is
 * typed), and the rest of them come back in place (`Within`).
 */
extension [A, Es[_]](p: A ! Es)
  /** answer the failure, seeing the error */
  def recover[E](using s: Split[Es, Throws % E], w: Within[s.Rest, Es])(h: E => A ! Es): A ! Es =
    runEither[A, Es, E](p)(using s).at[Es].flatMap(_.fold(h, pure[A, Es]))

  /** answer the failure, ignoring the error */
  def orElse[E](using s: Split[Es, Throws % E], w: Within[s.Rest, Es])(q: => A ! Es): A ! Es =
    recover[E](using s, w)(_ => q)

/** reflect a direct-style computation into the effect */
inline def catching[A, E <: Unsafe : Typeable](a: => A throws E): A ! Throws % (E | Unsafe) =
  unsafe(a).wrap match
    case Left(e) => raise[E | Unsafe, A](e)
    case Right(x) => pure[A, Throws % (E | Unsafe)](x)

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
   * marks are `.reflect`, `.?` and prefix `!p`).
   *
   * A TYPE-LEVEL GUARD WAS TRIED FIRST AND REFUTED, which is why this
   * is a move and not a side condition. `NotGiven[E =:= Nothing]`
   * reads plausibly — a value that cannot throw has nothing to unwrap
   * — but `throws` is COVARIANT in `E`, so the typer solves `E` to
   * its upper bound. The refusal message for a GENUINE receiver said
   * it outright: `okay.?[A, okay.freer.Unsafe](y)` for a `String throws
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

  // in the companion, not at the package level: a top-level `handle` here took the name from every
  // program's `p.handle(h)` (level 1, handle-handler-values)
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
}

/** evaluate, catching a thrown E as is and any other Throwable as a Failure */
inline def unsafe[A, E <: Unsafe : Typeable](a: => A throws E): A throws E =
  try a catch {
    case e: E => e
    case e => Failure(e)
  }

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
 * program may stop early. A plain `A ! Es` may not — nothing in a
 * program declines to answer — and it must not pretend to, because a silently
 * skipped step is a bug that reads like a feature. So the witness
 * below is the whole design, and everything else follows from which
 * effect supplies it.
 *
 * THREE EFFECTS CAN FAIL, AND THEY MEAN DIFFERENT THINGS.
 *
 *   Choose   the BRANCH dies and the search goes on — `guard`, under
 *            the syntax people reach for first
 *   Maybe    nothing is THERE and `Maybe.run` answers None — the
 *            refuted `case Some(x) <-` said exactly that
 *   Abort    the PROGRAM stops and `runOption` answers None — a lookup
 *            with nothing to look up, which has no other branch to
 *            continue into
 *
 * A row carrying `Choose` gets the search meaning: where a program
 * searches, `guard` already means prune, and one syntax must not mean
 * two things in one row. Between the two that stop, Maybe wins over
 * Abort (specs/core-gaps.md).
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
 * What DOES resolve is membership, the witness `.at` already uses
 * (`Within`: the effect found among the program's, by its `Req`). So
 * the evidence for "these effects may drop a step" is that they
 * CONTAIN an effect that can fail, and nothing larger.
 */
@implicitNotFound("""this row cannot drop a step, so a refutable pattern (`case Some(x) <- p`) and an `if` guard have no meaning in it: ${Es}
Both desugar to withFilter, which needs somewhere for the dropped step to GO.
Put a failing effect in the row and it works: Maybe (nothing is there, Maybe.run answers None), Abort (the program stops, runOption answers None) or Choose (the branch dies, the search goes on).
Or keep the row as it is and write the branch yourself: `old.fold(pure(()))(...)` says the same thing and hides nothing""")
trait CanFail[Es[_]]:
  def fail[A]: A ! Es

trait CanFailLow:
  /** stop: the rest of the program does not run */
  given viaAbort: [Es[_]] => okay.Evidence.All[Abort, Es] => Within[Abort, Es] => CanFail[Es] = new:
    def fail[A]: A ! Es = abort[A].at[Es]

trait CanFailMid extends CanFailLow:
  /** nothing is there: a refuted `case Some(x) <-` IS an absence, so
   * where a row carries both Maybe and Abort, Maybe answers it
   * (specs/core-gaps.md) */
  given viaMaybe: [Es[_]] => okay.Evidence.All[Maybe, Es] => Within[Maybe, Es] => CanFail[Es] = new:
    def fail[A]: A ! Es = Maybe.none[A].at[Es]

object CanFail extends CanFailMid:
  /** prune: this branch dies, the search continues — and it wins over
   * Abort where a row carries both, because in a searching row `guard`
   * already means prune */
  given viaChoose: [Es[_]] => okay.Evidence.All[Choose, Es] => Within[Choose, Es] => CanFail[Es] = new:
    def fail[A]: A ! Es = effect[Choose, A](Choose(Seq.empty)).at[Es]

extension [A, Es[_]](p: A ! Es)
  /** the desugaring target of a pattern bind and of an `if` guard */
  def withFilter(q: A => Boolean)(using C: CanFail[Es]): A ! Es =
    p.flatMap(a => if q(a) then pure[A, Es](a) else C.fail[A])

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
inline def ensure[Es[_]](p: Boolean)(using C: CanFail[Es]): Unit ! Es =
  if p then pure[Unit, Es](()) else C.fail[Unit]

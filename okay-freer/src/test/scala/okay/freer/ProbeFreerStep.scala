package okay.freer

import okay.*

import scala.annotation.tailrec

/**
 * ONE INDEXED BASE FOR `Free` AND `Cont`, PROBED (freer-base-step-
 * extractor, 2026-09-29) — AND LANDED the same day: `okay.freer.Freer` is
 * this enum with `A` last and `Lift` a class projection (specs/
 * freer-base.md, "The dual placement, LANDED", says what the compiler
 * added). Kept compiling, like ProbeRowCrash, so the next Scala says by
 * turning red whether the shapes below still type; its own `Freer`
 * shadows the library's inside this object on purpose.
 *
 * specs/freer-base.md stage 1 wanted `Free[F, A]` to be an indexed
 * `Freer[Unary[F], A, Unit, Unit]` and was REFUTED: matching `Bind`
 * makes the intermediate index existential, the 106 match sites want
 * `k: X => A ! F`, and the pinning extractor tried then — the type
 * variable only in `unapply`'s RESULT — infers it as `Nothing`. The
 * facade that landed instead (`Cont = Free[Shift, A]`, indexes on the
 * outside) pays two casts in Cont's runner (`Shift.at`, `pinned`).
 *
 * THIS PROBE IS THE DUAL, and it compiles: the tree is indexed, Cont's
 * runner is typed by the GADT again (no cast at all — `Return` gives
 * `S = R`, the leaf is `(A => S) => R` precisely), and the erased side
 * pays ONE cast, in `Step` — an extractor whose pattern-bound type
 * variables sit in its PARAMETER type (the compiler inserts the type
 * test that binds them; that is what the stage-1 signature lacked)
 * and whose result is the node itself (a Product: no Option, no
 * tuple; bytecode `aload_1; areturn`). `resume` — the rotation — is
 * written once, index-polymorphic, cast-free. A third instantiation
 * (`Typed`, a protocol index on a plain effect) is the same enum at
 * another signature: no second facade, which is where duplication
 * grows today (`Prog`, its own `transition`).
 *
 * WHAT THE COMPILER SAID (bare dotc 3.9.0, `-Wall`-equivalent, on the
 * standalone copy of this file with a main): no error, no warning;
 * `runFree` 42, a relaying handler 42, a 100 000-deep foldLeft chain
 * 100000 (rotated, stack whole), `k(1) + k(10)` under reset 11
 * (multi-shot), an answer-type-modifying chain "seed5!". Refused, as
 * they must be: a bind whose answer types do not meet (E007), a run
 * with a continuation of the wrong answer type (E007), `Step` on a
 * `Cont[Int, Int, Int]` (E030 unreachable — proved, not trusted),
 * `Step` on a `Freer[G, A, Unit, Unit]` with G abstract (E092, the
 * type test cannot be checked — a warning, which this repository's
 * gate makes red). `Step` on the DIAGONAL at an abstract R,
 * `Cont[A, R, R]`, is the one shape the compiler lets through: R is a
 * method type parameter and the GADT may bind it to Unit, so the test
 * reads as checkable. `Step` therefore belongs beside `Free` in
 * `object !`, applied to scrutinees typed `Free[F, A]` — the standing
 * of today's `(x.resume: @unchecked)`, no worse and no better.
 *
 * NOT MEASURED: nodes are the same objects as today's (`Op(g)` is
 * `Inject(e)` byte for byte, the leaf is the raw function), so no
 * number is expected to move; the lane, if taken, changes 106 match
 * sites mechanically (`Bind(Inject(e), k)` -> `Step(Op(e), k)`) and
 * deletes the two casts and the `Shift[+X] = (X => Nothing) => Any`
 * spelling with its paragraph. backlog.d/okay-core/
 * freer-base-step-extractor.md has the plan.
 */
object ProbeFreerStep:

  /** the one base: S and R are the answer types of (A => S) => R; a
   * signature that ignores them (Lift) leaves them phantom */
  enum Freer[G[_, _, _], A, S, R]:
    case Return[G[_, _, _], A, R](a: A) extends Freer[G, A, R, R]
    case Op[G[_, _, _], A, S, R](g: G[A, S, R]) extends Freer[G, A, S, R]
    case Bind[G[_, _, _], A, B, S, T, R](a: Freer[G, A, T, R], f: A => Freer[G, B, S, T]) extends Freer[G, B, S, R]
    case Delay[G[_, _, _], A, S, R](t: () => Freer[G, A, S, R]) extends Freer[G, A, S, R]

    def flatMap[B, S2](f: A => Freer[G, B, S2, S]): Freer[G, B, S2, R] = Bind(this, f)

    /** THE rotation, once, index-polymorphic: no cast anywhere */
    @tailrec final def resume: Freer[G, A, S, R] = this match
      case Bind(Bind(a, f), g) => Bind(a, x => f(x).flatMap(g)).resume
      case Bind(Return(a), f) => f(a).resume
      case Delay(t) => t().resume
      case Bind(Delay(t), g) => Bind(t(), g).resume
      case a => a

  // ---- Free: the base at a signature that ignores the indexes ----

  type Unary[F[+_]] = [X, S, R] =>> F[X]
  type Free[F[+_], A] = Freer[Unary[F], A, Unit, Unit]

  object Free:
    def pure[F[+_], A](a: A): Free[F, A] = Freer.Return(a)
    def inject[F[+_], A](e: F[A]): Free[F, A] = Freer.Op(e)

  /**
   * THE ONE CAST of the erased side: a Lift tree is built with every
   * index Unit (the factories above are the only doors), so the
   * intermediate index a Bind forgot is Unit too. The type variables
   * sit in the PARAMETER (a type test binds them), the result is the
   * node itself (a Product: no Option, no tuple, no allocation).
   */
  object Step:
    def unapply[F[+_], X, A, T](b: Freer.Bind[Unary[F], X, A, Unit, T, Unit]): Freer.Bind[Unary[F], X, A, Unit, Unit, Unit] =
      b.asInstanceOf[Freer.Bind[Unary[F], X, A, Unit, Unit, Unit]]

  trait Interp[F[+_]]:
    def handle[X](e: F[X]): X

  /** Effects.scala's `runFree`, in shape */
  @tailrec def runFree[F[+_], A](m: Free[F, A])(using H: Interp[F]): A =
    (m.resume: @unchecked) match
      case Freer.Return(a) => a
      case Freer.Op(e) => H.handle(e)
      case Step(Freer.Op(e), k) => runFree(k(H.handle(e)))

  /** a handler that RELAYS: builds a new tree from k — the shape that
   * needs k typed as X => Free[F, A], not X => Freer[…, t] */
  def relay[F[+_], A](m: Free[F, A]): Free[F, A] =
    (m.resume: @unchecked) match
      case Freer.Return(a) => Free.pure(a)
      case Freer.Op(e) => Free.inject(e)
      case Step(Freer.Op(e), k) => Free.inject(e).flatMap(x => relay(k(x)))

  // ---- Cont: the same base at the precise leaf ----

  type Shift = [X, S, R] =>> (X => S) => R
  type Cont[A, S, R] = Freer[Shift, A, S, R]

  object Cont:
    def pure[A, R](a: A): Cont[A, R, R] = Freer.Return(a)
    def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Freer.Op(f)
    def reset[A, R](c: Cont[A, A, R]): R = run(c)(identity)

    /** the continuation a shift's body receives: direct style's own
     * frame (the library's `Reentry`), out of `run` so @tailrec checks
     * the loop */
    private def reenter[X, B, S, T](g: X => Cont[B, S, T], k: B => S): X => T = x => run(g(x))(k)

    /** the old Cont's runner: the GADT types every line — no `Shift.at`, no `pinned` */
    @tailrec def run[A, S, R](c: Cont[A, S, R])(k: A => S): R = c match
      case Freer.Return(a) => k(a)
      case Freer.Op(f) => f(k)
      case Freer.Bind(Freer.Op(f), g) => f(reenter(g, k))
      case Freer.Bind(Freer.Bind(a, f), g) => run(Freer.Bind(a, x => f(x).flatMap(g)))(k)
      case Freer.Bind(Freer.Return(a), f) => run(f(a))(k)
      case Freer.Delay(t) => run(t())(k)
      case Freer.Bind(Freer.Delay(t), g) => run(Freer.Bind(t(), g))(k)

  // ---- a third instantiation: a protocol index on a plain effect ----

  /** `Prog` with no second facade: Lift with the indexes KEPT */
  type Typed[F[+_]] = [X, S, R] =>> (F[X], S => R)
  type Prog[F[+_], A, S, R] = Freer[Typed[F], A, S, R]

  // ---- the programs the standalone copy ran (values in the comment) ----

  enum Ask[+A]:
    case Get extends Ask[Int]

  given Interp[Ask] with
    def handle[X](e: Ask[X]): X = e match
      case Ask.Get => 21

  def answers: (Int, Int, Int, Int, String) =
    val p: Free[Ask, Int] = Free.inject(Ask.Get).flatMap(a => Free.inject(Ask.Get).flatMap(b => Free.pure(a + b)))
    val deep = (1 to 100000).foldLeft(Free.pure[Ask, Int](0))((m, _) => m.flatMap(x => Free.pure(x + 1)))
    val c: Cont[Int, Int, Int] = Cont.shift[Int, Int, Int](k => k(1) + k(10))
    val atm: Cont[Unit, String => String, String] =
      Cont.shift[Int, String => String, String](k => k(5)("seed")).flatMap(n =>
        Cont.shift[Unit, String => String, String => String](k => s => k(())(s + n)))
    (runFree(p), runFree(relay(p)), runFree(deep), Cont.reset(c), Cont.run(atm)(_ => s => s + "!"))

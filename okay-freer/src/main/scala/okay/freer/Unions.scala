package okay.freer

import okay.{Answers, Control, TypeableK, ==>}

/**
 * THE CLASSIC'S UNION ROWS, what a handler needs to tell one effect's operations from the rest's: a class test
 * (`TypeableK`) and the split on it — the classic's claim, the union's excluded middle (split-without-either).
 * In the classic since stage 47: the core's interface is over nominal rows, where an operation is found by
 * its path and no class is tested.
 */

/** the empty signature is trivially splittable: nothing inhabits it, so the test never matches — which lets
 * row-generic code (Logic, the effectful streams) instantiate at F = Pure */
given TypeableK[Pure] = new:
  def test(x: Any): Boolean = false

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
 * measured no faster than Cps and is gone (core-cleanup) */
type Interpr[F[_], C[_, _, _], S] = F ==> C[*, S, S]

/**
 * A handler of the operations F, with the answers S, is an interpretation
 * of F in the continuation paramonad: the natural transformation
 *
 * F ==> ([X] =>> X /> S)
 *
 * That is, handlers are continuations.
 */
infix type !>[F[_], S] = Interpr[F, okay.cont.Carrier, S]

/** `A /> R`: the machine as a control carrier at its diagonal, an ordinary monad — the bare name is the machine's
 * (cont-classic-rename); the CPS paramonad's is `A />> R`, over `Cps` */
infix type />[A, R] = okay.cont.Carrier[A, R, R]

/** A comonadic handler interprets each operation by its own value */
/** A comonadic (per-operation) Answers at every answer type. */
inline def handler[F[_] : Answers as H, S]: F !> S =
  [X] => e => okay.cont.Cont.Return(H.handle(e))

/** the same, at any Control carrier */
inline def interpr[C[_, _, _] : Control as C, F[_] : Answers as H, S]: Interpr[F, C, S] =
  [X] => e => C.pure(H.handle(e))

/** named, with a PUBLIC `C`, for the same binary-compatibility reason
 * as `DiagonalMonad`: an inline method reaching a privately captured
 * given makes the compiler synthesize an accessor with an unstable
 * name, and a downstream JAR compiled against it breaks when this
 * library is recompiled. */
/** Pure has no operations left to handle */
given Answers[Pure] with
  inline def handle[A](a: Pure[A]): A = a

/**
 * Handlers compose along the union: split the operation by the F
 * test and delegate. This is what lets a multi-effect row be run by
 * `runWith` with one handler per effect, assembled by the compiler —
 * an agent's `Model + (Tool + (Context + Async))` needs no bespoke
 * interpreter, only its four handlers in scope.
 */

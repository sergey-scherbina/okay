package okay

import okay.freer.Freer

import scala.annotation.implicitNotFound
import scala.quoted.*

/**
 * THE ROW OF INDEXED SIGNATURES (specs/indexed-effects.md, stage 3).
 *
 * `A ! F` is a program over a row of UNARY signatures, `F + G`, every
 * index `Unit`. A signature that carries the tree's two indexes —
 * `PState.Op[S, R, X]`, a typestate whose state moves from `R` to `S`
 * — is three-ary, and a row of those is this file: `F +~ G` is the
 * union at three parameters, exactly as `+` is at one, and `split`'s
 * by-class test is `splitI`'s. Nothing in the tree changes: `Freer[F
 * +~ G, S, R, A]` is the same nodes, `Inject` for every operation —
 * one that moves the index and one that does not alike.
 *
 * THE UNARY MEMBER, and why it is a match type. An ordinary effect —
 * `State % Int` beside a typestate — has no index of its own; the row
 * probe (TestFreerPara) first lifted it with a wrapper per operation
 * (`At.Op`), then with a `Diag` node in `Freer` (freer-diag-leaf),
 * gone since freer-no-diag (2026-10-04). `Unary[F]` is
 * `[S, R, X] =>> S match { case R => F[X] }`: it REDUCES to `F[X]` only
 * when the two indexes are the same type — the diagonal — and off it
 * is `Nothing` where they are provably different (the second case;
 * without it dotty WARNS at every door whose indexes are disjoint,
 * E184 "matches none of the cases") and a stuck match type where they
 * are abstract, which no value of `F[X]` conforms to either. So
 * `Indexed.unary` accepts a `State` operation and `Indexed.effect` at
 * `(Int => Z, String => Z)` refuses it, both by the compiler
 * (TestFreerPara pins the refusal).
 *
 * ONE SYSTEM FOR TWO ARITIES (freer-no-diag, 2026-10-04): the tree
 * knows one kind of signature, `G[_, _, +_]`; `+~` is its one sum; a
 * unary effect enters it by one bridge, `Unary`; and `+` — the unary
 * sum — agrees with them: `Unary[F + G]` and `Unary[F] +~ Unary[G]`
 * reduce to the same type, `F[X] | G[X]` on the diagonal and nothing
 * off it. A row of both arities is `F +~ Unary[G]`. Since the DOOR is
 * typed, every operation of a unary member in the tree was built on
 * the diagonal, and a handler that tells one apart (`splitI`) gets the
 * equality its continuation needs from `Indexed.onDiagonal` — the one
 * claim of the design, the reflexive singleton, no node and no
 * allocation (it was the `Diag` node's job). An indexed signature's
 * own diagonal operations need not even that: `TxOp.Update[S] extends
 * TxOp[S, S, Long]`, and matching the constructor gives the equality.
 *
 * Not here, on purpose: an inductive membership witness over `+~`
 * (`Row.In`'s shape crashes dotty on an abstract row —
 * row-membership-crash — and nothing needs one: a handler names the
 * member it answers and takes the rest by exclusion), and a `direct`
 * block over an indexed program (the macro reads `Free`'s doors by
 * symbol; `A ! F` stays the 95% case and is untouched).
 */

/** the union of two indexed signatures — `+` at three parameters */
infix type +~[F[_, _, +_], G[_, _, +_]] = [S, R, X] =>> F[S, R, X] | G[S, R, X]

/** a unary effect as a member of an indexed row: `F[X]` on the
 * diagonal, and nothing off it — see the header */
type Unary[F[+_]] = Diagonal[F]#L

/**
 * `Unary`'s body, a projection on a class rather than a bare type lambda, for inference: two lambdas applied to a
 * row `Users + F` beta-reduce to unions, which give nothing to solve `F1` and `G` from; a projection compares by its
 * prefix (`Diagonal[Users + F]` against `Diagonal[F1 + G]`), so the row's `+` matches application to application.
 * The member reduces to `F[X]` wherever its two indexes are one type — every `A ! F`, at `Unit` — and to nothing
 * elsewhere (one-bridge: `Free`'s bridge and the indexed rows' are this one; `Lift`, which ignored the indexes,
 * is gone).
 */
/**
 * THE BRIDGE'S EXTRACTOR (unary-extractor): a node of a unary member's operation ON THE DIAGONAL, answered at the
 * operation's own type. Its field is typed by the bridge, `Unary[F][R, R, A]` — a match type, which reduces in an
 * expression but not as the scrutinee of a nested pattern, so `case Unary(Writer.Say(c))` on the plain node would
 * refine nothing; viewed at `Unary.Op[F]` the field IS `F[A]`, and a constructor pattern on it refines `A` as a GADT
 * match does. A product match: the node itself, nothing allocated. `Free.Inject` is this at `Unit, Unit`.
 */
object Unary:
  /** `F` with its two indexes ignored: what the bridge IS on the diagonal */
  type Op[F[+_]] = [S, R, X] =>> F[X]

  def unapply[F[+_], R, A](i: Freer.Inject[Unary[F], R, R, A]): Freer.Inject[Op[F], R, R, A] =
    // THE CLAIM, the bridge's own definition read back: on the diagonal `Unary[F]` reduces to `F` — the same
    // operation, the same node, read at the signature that says so
    i.asInstanceOf[Freer.Inject[Op[F], R, R, A]]

sealed trait Diagonal[F[+_]]:
  type L[S, R, +X] = S match
    case R => F[X]
    case _ => Nothing

/**
 * THE DOORS OF A PROGRAM, in the implicit scope of every `A ! F` with no import: `Unary[F]` is `Diagonal[F]#L`, a
 * member's projection, and a member's prefix anchors its implicit scope — so this companion is where
 * `p.handle`, `p.run`, the direct colouring and the tree's `Monad`/`TailRecM` are found (they were `Freer`'s
 * companion's until the monad moved below the core, freer-min stage 29; a top-level `run` was ambiguous beside
 * any other wildcard-imported `run`, and a selective `import okay.{!, …}` lost `p.handle`).
 */
object Diagonal {
  import Freer.Return
  /**
   * ONE handler taken off (handler-single-pass, specs/handler-single-pass.md): a STEPPED handler is registered
   * on the program's stack of handlers, walked once by whoever forces it; any other handler runs its own `run`
   * over the program, as it always has. The types were checked by the caller's `handle`.
   */
  def handleOne[A, E[+_], I, O[_], N[_[+_]], F[+_]](p: A ! E + F, h: Handler.Full[E, I, O, N])
                                                   (using A <:< I, Distinct[E + F], N[F]): O[A] ! F =
    h match
      case s: Handler.Stepped[?, ?, ?] => HandleFrames.handled[A, O, F](p, s)
      case _ => h.run[A, F](p)

  // level 1 (specs/shift-effect.md): in the companion, so `p.handle` and `p.run` need no import
  extension [A, G[+_]](p: A ! G)
    /** take the handler's effect off the row: `F`, the rest of the row, is what remains */
    def handle[E[+_], I, O[_], N[_[+_]], F[+_]](h: Handler.Full[E, I, O, N])
                                               (using row: (A ! G) =:= (A ! E + F), ok: A <:< I, d: Distinct[E + F], n: N[F]): O[A] ! F =
      handleOne[A, E, I, O, N, F](row(p), h)

    /** two handlers, innermost first: `p.handle(State(5), Throws.either)` is `p.handle(State(5)).handle(Throws.either)` */
    def handle[Ef1[+_], I1, O1[_], N1[_[+_]], F1[+_], Ef2[+_], I2, O2[_], N2[_[+_]], F2[+_]](
        h1: Handler.Full[Ef1, I1, O1, N1], h2: Handler.Full[Ef2, I2, O2, N2])
        (using r1: (A ! G) =:= (A ! Ef1 + F1), ok1: A <:< I1, d1: Distinct[Ef1 + F1], n1: N1[F1],
               r2: (O1[A] ! F1) =:= (O1[A] ! Ef2 + F2), ok2: O1[A] <:< I2, d2: Distinct[Ef2 + F2], n2: N2[F2]): O2[O1[A]] ! F2 =
      handleOne[O1[A], Ef2, I2, O2, N2, F2](r2(handleOne[A, Ef1, I1, O1, N1, F1](r1(p), h1)), h2)

    /** three handlers, innermost first */
    def handle[Ef1[+_], I1, O1[_], N1[_[+_]], F1[+_], Ef2[+_], I2, O2[_], N2[_[+_]], F2[+_], Ef3[+_], I3, O3[_], N3[_[+_]], F3[+_]](
        h1: Handler.Full[Ef1, I1, O1, N1], h2: Handler.Full[Ef2, I2, O2, N2], h3: Handler.Full[Ef3, I3, O3, N3])
        (using r1: (A ! G) =:= (A ! Ef1 + F1), ok1: A <:< I1, d1: Distinct[Ef1 + F1], n1: N1[F1],
               r2: (O1[A] ! F1) =:= (O1[A] ! Ef2 + F2), ok2: O1[A] <:< I2, d2: Distinct[Ef2 + F2], n2: N2[F2],
               r3: (O2[O1[A]] ! F2) =:= (O2[O1[A]] ! Ef3 + F3), ok3: O2[O1[A]] <:< I3, d3: Distinct[Ef3 + F3], n3: N3[F3])
        : O3[O2[O1[A]]] ! F3 =
      handleOne[O2[O1[A]], Ef3, I3, O3, N3, F3](
        r3(handleOne[O1[A], Ef2, I2, O2, N2, F2](r2(handleOne[A, Ef1, I1, O1, N1, F1](r1(p), h1)), h2)), h3)

  extension [A](p: A ! Pure)
    /** a program with no effect left, run to its value */
    inline def run: A = p.runWith

  /**
   * A program value as its answer INSIDE a `direct` block: `Direct.selfColor` for this monad, found here with
   * no import (a conversion's implicit scope is its source type's, and `Free` dealiases to `Freer`).
   * `DirectCtx` exists only inside a block, so outside one a program stays a program; the body never runs.
   * A program of ANOTHER row colours too: the macro, which knows both rows, checks the membership and coerces
   * or refuses by name (searched here, the row's halves would still be free variables).
   */
  given directColor[R[+_], R2[+_], A](using DirectCtx[[X] =>> Free[R, X]]): Conversion[Free[R2, A], A] =
    _ => throw new IllegalStateException(
      "Direct auto-coloring escaped macro rewriting — this call belongs inside direct { ... }")

  /** Free[F, *] is a Monad for every signature F, with no constraint on F */
  given [F[+_]]: Monad[Free[F, *]] with
    override inline def pure[A](a: A): Free[F, A] = Return(a)
    extension [A](a: Free[F, A])
      override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = a.flatMap(f)

  /** a program's loop is `!.loop`: the recursion sits in a `Bind` the
   * interpreter resumes, never on the caller's stack
   * (specs/eager-carrier-depth.md) */
  given [F[+_]]: TailRecM[Free[F, *]] with
    def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] = Free.loop(a)(f)

}

/** ∀S R X, the runtime test for F[S, R, X] — `TypeableK` at three
 * parameters; by class, since the indexes are erased and a row may
 * hold one of a signature (many-instances is `Tag`'s, as for `State`) */
@implicitNotFound("no TypeableI[${F}].\nSplitting an indexed row needs a runtime test for ${F}'s operations: `given TypeableI[YourOp] = TypeableI.derived` beside the signature (or `TypeableI.byClass(classOf[...])` for a class known only at run time).")
trait TypeableI[F[_, _, +_]]:
  def test(x: Any): Boolean

object TypeableI:
  /** the test by the operations' runtime class, READ FROM A FIELD and
   * asked of `Class.isInstance`: for a class that is a run-time value.
   * For a signature written in the source, `derived` (below) is the
   * same test as a constant-class `instanceof`, which is what the JIT
   * folds; measured on a forwarding handler (indexed-effects-measure-2,
   * `stateIndexedForward` against `stateForward`), the field-and-call
   * form read 1.045x on a lane that is nothing but dispatch — the same
   * residual `TypeableK.derived` removed on the unary side
   * (typeablek-instanceof). A member that is a MATCH TYPE (`Unary[F]`)
   * erases to no class: write its instance by hand as
   * `new TypeableI[Unary[F]] { def test(x: Any) = x.isInstanceOf[F[?]] }`. */
  def byClass[F[_, _, +_]](cls: Class[?]): TypeableI[F] = new TypeableI[F]:
    def test(x: Any): Boolean = cls.isInstance(x)

  /** `TypeableK.derived`'s twin for a three-ary signature: the
   * signature's class with every argument a wildcard, emitted as a
   * class of its own per site so the test is a constant `instanceof` */
  inline def derived[F[_, _, +_]]: TypeableI[F] = ${ okay.macros.IndexedMacros.derivedImpl[F] }

/**
 * `split` over an indexed row: the left member by its test, the right
 * by exclusion. The two casts are `split`'s, licensed by the one test
 * — a runner refines the answer type by matching the constructor
 * inside `onF`, exactly as after `split`, so no cast reaches a runner.
 */
inline def splitI[F[_, _, +_], G[_, _, +_]](using T: TypeableI[F])[S, R, X, B]
                                           (e: F[S, R, X] | G[S, R, X])
                                           (inline onF: F[S, R, X] => B)
                                           (inline onG: G[S, R, X] => B): B =
  if T.test(e) then onF(e.asInstanceOf[F[S, R, X]]) else onG(e.asInstanceOf[G[S, R, X]])

/** the doors of an indexed program, beside `!`'s */
/**
 * THE INDEXED `forwarded` (Answers.scala): a node whose operation
 * `splitI` has just proven to be `F`'s, read at `F` — the row narrowed
 * by the test that was made a line above, the same cast for the same
 * reason. A handler that forwards the node it holds allocates nothing
 * for the forwarding (indexed-effects-measure-2: rebuilding the node
 * read +24 B and 1.047x per forwarded operation on `stateIndexedForward`).
 */
inline def forwardedI[F[_, _, +_], G[_, _, +_]](using DummyImplicit)[S, R, X](n: Freer[F +~ G, S, R, X]): Freer[F, S, R, X] =
  n.asInstanceOf[Freer[F, S, R, X]]

object Indexed:
  /** a value, at any index, moving nothing */
  inline def pure[G[_, _, +_], R, A](a: A): Freer[G, R, R, A] = Freer.Return(a)

  /** an operation that MOVES the index: the signature says from where
   * to where */
  inline def effect[G[_, _, +_], S, R, X](g: G[S, R, X]): Freer[G, S, R, X] = Freer.Inject(g)

  /** an operation that moves nothing — a unary member's (`Unary[G]` reduces to `G[X]` only here), or an indexed
   * signature's own diagonal operation (`TxOp.Update[S] extends TxOp[S, S, Long]`): a plain `Inject` at `(R, R)` */
  inline def unary[G[_, _, +_], R, X](e: G[R, R, X]): Freer[G, R, R, X] = Freer.Inject(e)

  /**
   * A whole UNARY PROGRAM inside an indexed one, on the diagonal
   * (indexed-effects stage 2, for `Tx.Data`'s `async`): every operation
   * becomes the `Inject` at `(R, R)` that `unary` would build for it, through `into`
   * — which at a `Unary` member is the identity, since the match type
   * reduces to `F[X]` there. Lazy, one node per operation as the
   * interpreter reaches it, and the recursion is under `flatMap`
   * (trampolined): a program of any depth lifts in constant stack.
   */
  def lift[G[_, _, +_], F[+_], R, A](p: Free[F, A])(into: [X] => F[X] => G[R, R, X]): Freer[G, R, R, A] =
    (p.resume: @unchecked) match
      case Freer.Return(a) => Freer.Return(a)
      case Freer.Inject(e) => Freer.Inject(into(e))
      case Free.Bind(Free.Inject(e), k) => Freer.Inject(into(e)).flatMap(x => lift(k(x))(into))

  /**
   * THE ONE CLAIM OF A MIXED-ARITY ROW (freer-no-diag, the operator's ask: no `Diag` node — the combinator of
   * effects of different arity carries it): an operation of a UNARY member stands on the diagonal. Not a
   * convention but the door's type: `Unary[G][S, R, X]` is `S match { case R => G[X] }`, which REDUCES to
   * `G[X]` only where `S` and `R` are one type — `Nothing` where they differ, stuck where they are abstract — so no
   * value of `G[X]` was ever built into the row anywhere else (TestFreerPara pins the refusal). A handler that
   * has told a unary member's operation apart (`splitI`) asks this for the equality its continuation needs.
   * The answer is the reflexive singleton: no allocation.
   */
  def onDiagonal[G[+_], S, R, X](@annotation.unused e: Unary[G][S, R, X]): S =:= R =
    summon[S =:= S].asInstanceOf[S =:= R]

  /** a unary member's operation read as its effect's, at the diagonal `onDiagonal` proved */
  def atDiagonal[G[+_], S, R, X](e: Unary[G][S, R, X]): G[X] =
    onDiagonal(e).substituteCo[[s] =>> Unary[G][s, R, X]](e)

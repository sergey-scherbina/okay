package okay

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
 * +~ G, S, R, A]` is the same nodes, with `Inject` for an operation
 * that moves the index and `Diag` for one that does not.
 *
 * THE UNARY MEMBER, and why it is a match type. An ordinary effect —
 * `State % Int` beside a typestate — has no index of its own; the row
 * probe (TestFreerPara) first lifted it with a wrapper per operation
 * (`At.Op`), then with `Diag` on the node (freer-diag-leaf). What was
 * left open was the DOOR: the union `PSt[S, R, X] | State[Int, X]`
 * accepts a `State` operation through `Inject` at a moving index, a
 * program no handler can answer (its continuation would be at an
 * index the handler cannot reach), so the handler threw. `Unary[F]`
 * closes it at the type: `[S, R, X] =>> S match { case R => F[X] }`
 * REDUCES to `F[X]` only when the two indexes are the same type — the
 * diagonal, where `Diag`'s door puts it — and off it is `Nothing`
 * where the two indexes are provably different types (the second
 * case; without it dotty WARNS at every door whose indexes are
 * disjoint, E184 "matches none of the cases") and a stuck match type
 * where they are abstract, which no value of `F[X]` conforms to
 * either. So `Indexed.
 * unary` accepts a `State` operation and `Indexed.effect` at `(Int =>
 * Z, String => Z)` refuses it, both by the compiler (TestFreerPara
 * pins the refusal). What the match type cannot do is tell a HANDLER
 * that the arm is dead: at an existential middle index the member is
 * stuck, not `Nothing`, so `splitI`'s exclusion arm still has to be
 * written there. `offDiagonal` is that arm, the one throw of the
 * design, named once and documented as `Free.Bind`'s constant claim
 * is: no door builds what it catches.
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
type Unary[F[+_]] = [S, R, X] =>> S match
  case R => F[X]
  case _ => Nothing

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

  /** an operation that moves nothing, on the diagonal by its node — the
   * door of a `Unary` member, and of any diagonal operation */
  inline def unary[G[_, _, +_], R, X](e: G[R, R, X]): Freer[G, R, R, X] = Freer.diag(e)

  /**
   * A whole UNARY PROGRAM inside an indexed one, on the diagonal
   * (indexed-effects stage 2, for `Tx.Data`'s `async`): every operation
   * becomes the `Diag` node `unary` would build for it, through `into`
   * — which at a `Unary` member is the identity, since the match type
   * reduces to `F[X]` there. Lazy, one node per operation as the
   * interpreter reaches it, and the recursion is under `flatMap`
   * (trampolined): a program of any depth lifts in constant stack.
   */
  def lift[G[_, _, +_], F[+_], R, A](p: Free[F, A])(into: [X] => F[X] => G[R, R, X]): Freer[G, R, R, A] =
    (p.resume: @unchecked) match
      case Freer.Return(a) => Freer.Return(a)
      case Freer.Inject(e) => Freer.Diag(into(e))
      case Free.Bind(Free.Inject(e), k) => Freer.Diag(into(e)).flatMap(x => lift(k(x))(into))

  /**
   * The exclusion arm no door can reach: a `Unary` member's operation
   * under `Inject` at a moving index. `Indexed.effect` refuses it at
   * compile time (the match type is stuck off the diagonal), so a
   * handler that meets one holds a node built by hand around the
   * doors — the claim is constant, as `Free.Bind`'s, and this is where
   * it is said.
   */
  def offDiagonal(op: Any): Nothing =
    throw IllegalStateException(
      s"an operation of a unary member of an indexed row off the diagonal: $op — built by `Freer.Inject` where `Indexed.unary` is the door")

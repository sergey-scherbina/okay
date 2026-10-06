package okay.freer

import scala.annotation.tailrec

/**
 * `(A => S) => R` over the signature `G`: a computation of `A` that, given a continuation into `S`, answers `R`.
 * The freer monad (Kiselyov–Ishii 2015), indexed for answer-type modification (Danvy–Filinski on the node;
 * Atkey's parameterised monad as the algebra). Four nodes and nothing else (specs/freer-min.md):
 *
 *  - `Return`, the value: the inner answer is the outer one (the diagonal), over the empty row;
 *  - `Inject`, a DIAGONAL operation: the equation `S = R` sits on the node, so a matched `Bind` recovers its
 *    middle index from the node and dispatch needs no cast. A unary signature enters here, as `Diag[F]`;
 *  - `Perform`, an operation that MOVES the index: a continuation's (`Control`), a type-changing state's, a
 *    protocol's;
 *  - `Bind`, sequencing as data: the answer types meet at `T`.
 *
 * `G` is COVARIANT: a program over a row is a program over any wider row, and `flatMap` joins the two sides'
 * rows — a row is BUILT by `pure`/`inject`/`perform`/`flatMap`, never declared. `Delay` is not a node: a bind
 * off the unit (`delay`, `defer`), forced where `resume` forces any `Bind(Return(a), k)`.
 *
 * `A` comes LAST: a unary constructor inferred from a program value abstracts over the last parameter.
 */
enum Freer[+G[_, _, +_], S, R, +A]:
  case Return[R, A](a: A) extends Freer[Pure, R, R, A]
  case Inject[G[_, _, +_], T, A](op: G[T, T, A]) extends Freer[G, T, T, A]
  case Perform[G[_, _, +_], S, R, A](op: G[S, R, A]) extends Freer[G, S, R, A]
  case Bind[G[_, _, +_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]

  /** sequencing is a data node; the row of the result is the join of the rows of the two sides */
  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] = Bind(this, f)

  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))

  /**
   * THE rotation: a head form — `Return`, `Inject`, `Perform`, or a `Bind` whose left side is one of the last
   * two — in constant stack, for every signature, with no cast. Sound by associativity. A left side that is a
   * `Return` under a `Bind` (what `defer` builds) is forced here, with no closure composed over it.
   */
  @tailrec final def resume: Freer[G, S, R, A] = this match
    case Bind(Bind(m, f), g) => m match
      case Return(a) => Bind(f(a), g).resume
      case _ => Bind(m, x => Bind(f(x), g)).resume
    case Bind(Return(a), f) => f(a).resume
    case p => p

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [S, R, A] =>> Nothing

/** the row join: a union, pointwise */
infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]

/** a unary signature as a row: its operations stand at the diagonal, and the indexes are not its business */
type Diag[F[+_]] = [S, R, A] =>> F[A]

/** a value as a tree, at any index */
def pure[A, R](a: A): Freer[Pure, R, R, A] = Freer.Return(a)

/** a unary operation as a tree, at any index */
def inject[F[+_], T, A](op: F[A]): Freer[Diag[F], T, T, A] = Freer.Inject[Diag[F], T, A](op)

/** an index-moving operation as a tree */
def perform[G[_, _, +_], S, R, A](op: G[S, R, A]): Freer[G, S, R, A] = Freer.Perform(op)

/** a deferred program: built when an interpreter's loop reaches it, so functions returning a `Freer` call each
 * other in tail position with no JVM frame per call. Not a node: a bind off the unit */
def delay[G[_, _, +_], S, R, A](t: => Freer[G, S, R, A]): Freer[G, S, R, A] = Freer.Bind(Freer.Return(()), _ => t)

/** `delay` with something after it: `resume` forces the left side with no closure composed over it */
def defer[G[_, _, +_], S, T, R, A, B](t: => Freer[G, T, R, A])(f: A => Freer[G, S, T, B]): Freer[G, S, R, B] =
  Freer.Bind(delay(t), f)

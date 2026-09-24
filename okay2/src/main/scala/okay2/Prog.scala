package okay2

/**
 * AN INDEXED PROGRAM — the Scala 3 core's Prog.scala (specs/freer-base.md,
 * stage 2): the same `Free` tree behind an opaque facade with two phantom
 * indexes, `S` (what holds before the program runs) and `T` (what holds
 * after). A `Prog[R, A, S, T]` is a claim ABOUT the program, not a node
 * in it: nothing is allocated, nothing is matched, and `free` gives the
 * tree back byte for byte. What the indexes buy is SEQUENCING checked by
 * the compiler — `flatMap` joins a program that ends at `T` only to one
 * that starts at `T`, so a protocol written as smart constructors
 * (`begin: Idle -> Open`, `commit: Open -> Idle`) cannot be called out of
 * order, doubled, or left half done. That is typestate, at zero cost.
 *
 * Atkey's parameterised monad (JFP 2009) is the shape.
 *
 * Three doors, and their names are their discipline:
 *  - `diag(p)`: any ordinary program, at any index, moving nothing;
 *  - `transition(p)`: THE claim that `p` moves `S` to `T` — reviewed as
 *    the `asInstanceOf` of this design and kept inside a module's private
 *    smart constructors, since nothing checks it;
 *  - `free`: the unlift, on the DIAGONAL only. A program that promises a
 *    move it has not closed has no `free`.
 *
 * THE CAVEAT, as in Scala 3: the index says what a program does if it
 * runs to the end. An abort inside a block promising a transition drops
 * the transition; the type is not a run-time guarantee (TestProg).
 *
 * Scala 2 spelling: an abstract type in a module, `Rep`, which is Scala
 * 2's opaque type — as `Cont` and `Eager` are here. The row comes FIRST,
 * as in `Free[R, A]`; Scala 3 writes `Prog[F, A, S, R]` with the same
 * order. Not ported: `Delim.Stacked` over `Prog`, which needs a dependent
 * function type for its body (specs/okay2.md, stage 7).
 */
sealed abstract class ProgModule {
  type Rep[R, A, S, T]

  /** any program, at any index, moving nothing: the diagonal */
  def diag[S, R, A](p: Free[R, A]): Rep[R, A, S, S]
  def pure[R, A, S](a: A): Rep[R, A, S, S]
  /** THE claim: `p` moves the index from `S` to `T` */
  def transition[S, T, R, A](p: Free[R, A]): Rep[R, A, S, T]

  /** sequencing composes the indexes end to end */
  def flatMap[R, A, B, S, T, U](m: Rep[R, A, S, T])(f: A => Rep[R, B, T, U]): Rep[R, B, S, U]
  def map[R, A, B, S, T](m: Rep[R, A, S, T])(f: A => B): Rep[R, B, S, T]
  /** the tree back, unchanged — only for a program that ends where it began */
  def free[R, A, S](m: Rep[R, A, S, S]): Free[R, A]
}

private[okay2] object ProgImpl extends ProgModule {
  type Rep[R, A, S, T] = Free[R, A]

  def diag[S, R, A](p: Free[R, A]): Free[R, A] = p
  def pure[R, A, S](a: A): Free[R, A] = Free.Return(a)
  def transition[S, T, R, A](p: Free[R, A]): Free[R, A] = p
  def flatMap[R, A, B, S, T, U](m: Free[R, A])(f: A => Free[R, B]): Free[R, B] = Free.Bind[R, A, B](m, f)
  def map[R, A, B, S, T](m: Free[R, A])(f: A => B): Free[R, B] = Free.Bind[R, A, B](m, (a: A) => Free.Return(f(a)))
  def free[R, A, S](m: Free[R, A]): Free[R, A] = m
}

package okay

/**
 * AN INDEXED PROGRAM (specs/freer-base.md, stage 2): the same `Free`
 * tree behind an opaque facade with two phantom indexes — `S`, what
 * holds before the program runs, and `R`, what holds after. A
 * `Prog[F, A, S, R]` is a claim about the program, not a node in it:
 * nothing is allocated, nothing is matched, and `free` gives the tree
 * back byte for byte. What the indexes buy is SEQUENCING checked by
 * the compiler — `flatMap` joins a program that ends at `R` only to
 * one that starts at `R`, so a protocol written as smart constructors
 * (`begin: Idle -> Open`, `commit: Open -> Idle`) cannot be called out
 * of order, doubled, or left half done.
 *
 * Atkey's parameterised monad (JFP 2009) is the shape; `PState` in
 * State.scala is its first instance here, threading the index through
 * `Cont`'s answer type. This is the second, threading it through
 * NOTHING: the tree does not know it is indexed, which is why it costs
 * nothing and why the existential leak that refuted an indexed ENUM
 * (stage 1, the same spec) cannot reach it — a facade is never
 * matched.
 *
 * Three doors, and their names are their discipline:
 *
 *  - `diag(p)`: any ordinary program, at any index, moving nothing.
 *    An inline identity — the diagonal is a conversion, `A ! F` stays
 *    what it is.
 *  - `transition(p)`: THE claim that `p` moves `S` to `R`. Public,
 *    because a module typing its own protocol must make its own
 *    moves; reviewed as the `asInstanceOf` of this design, and kept
 *    inside that module's private smart constructors (okay-sql's `Tx`
 *    is the pattern).
 *  - `free`: the unlift, on the DIAGONAL only. A program that promises
 *    a move it has not closed (`begin` without `commit`) has no
 *    `free`; a module's runner takes the closed shape and unlifts
 *    inside.
 *
 * `Delim.Stacked` is the first consumer: the prompt stack in the
 * index, so a `shift` to a prompt that is not installed — `NoPrompt`
 * at run time today — is a compile error.
 *
 * `Prog[F, A, S, R]` is an alias for the opaque `Prog.Rep`, and the
 * reason is in the companion.
 */
type Prog[F[+_], A, S, R] = Prog.Rep[F, A, S, R]

object Prog:

  /** the facade's representation. INSIDE the companion, not at the
   * top level: a top-level opaque type is transparent to its whole
   * PACKAGE (cont-facade-over-free, 2026-09-15 — `Cont` learned it
   * first), and the package's program-carrier `map` then captures a
   * for-comprehension over the facade. Here the alias is opaque
   * everywhere outside this object, `Delim.Stacked` included. */
  opaque type Rep[F[+_], A, S, R] = Free[F, A]

  /** any program, at any index, moving nothing: the diagonal */
  inline def diag[S, F[+_], A](p: A ! F): Prog[F, A, S, S] = p

  inline def pure[F[+_], A, S](a: A): Prog[F, A, S, S] = Free.Return(a)

  inline def effect[F[+_], A, S](e: F[A]): Prog[F, A, S, S] = Free.Inject(e)

  /** THE claim: `p` moves the index from `S` to `R`. The one place a
   * protocol's author says what a step does; nothing checks it, which
   * is why it is named as loudly as a cast and kept private where it
   * is used. */
  inline def transition[S, R, F[+_], A](p: A ! F): Prog[F, A, S, R] = p

  extension [F[+_], A, S, R](m: Prog[F, A, S, R])
    /** sequencing composes the indexes end to end: `S -> R` then
     * `R -> T` is `S -> T`, and a continuation starting anywhere but
     * `R` does not typecheck */
    inline def flatMap[B, T](f: A => Prog[F, B, R, T]): Prog[F, B, S, T] = Free.Bind(m, f)
    inline def map[B](f: A => B): Prog[F, B, S, R] = Free.Bind(m, (a: A) => Free.Return(f(a)))

  extension [F[+_], A, S](m: Prog[F, A, S, S])
    /** the tree back, unchanged — for a program that ends where it
     * began. A move left open has no way out. */
    inline def free: A ! F = m

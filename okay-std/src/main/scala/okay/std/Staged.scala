package okay.std

import okay.{Control, Func, Monad}
import okay.{given Control[Func]}

/**
 * specs/direct-staged.md: a program over a row with its handler known at the call site — STAGING on the kernel's
 * words (was okay-freer's `Handled`).
 *
 * A `Handled[F, R, A]` is the root's `Func` at the diagonal, `(A => R) => R`: the program as a function of its
 * continuation, at answer type R, built and composed by `Control[Func]` — the control interface's closure instance —
 * with the row and the answer carried in the type so that the `direct` macro can read them off a block's F. Its
 * operations are not dispatched: a `Stager` object's `stage` is an INLINE MATCH over the row's constructors, applied
 * by the macro to the operation term as the block wrote it, and the compiler picks the arm — the handler applied to
 * the program's known structure at build time, so nothing is left to find at run time: no path, no `split`, no node
 * to interpret (the kernel's `Cont` would be one, and the specialisation's whole point is that there is none).
 * Measured before it was designed (staged-block-lanes, then `blockFuncInlineMatchR`): on a static 10-op block inside
 * a 100-iteration loop, 8.15 µs / 84 568 B against a fused Free fixture's 11.7 / 123 712 and nested runners'
 * 15.0 / 154 448 — the arm chosen at compile time is the whole of the win.
 *
 * `Func`'s contract is this type's contract: fast, fused, NOT stack-safe on a left-nested chain. A block that loops
 * ten thousand operations is fine; one that must loop a million is a program (`A ! Es`) under its handlers.
 */
object Handled:
  opaque type Handled[F[_], R, A] = Func[A, R, R]

  /** the root's `Control[Func]`, found HERE and at its own type, so its inline words resolve statically wherever a
   * `Handled` word is inlined — at a user's site, which has no import of it */
  private transparent inline def C = summon[Control[Func]]

  /** an operation's meaning: what it does with its continuation — `Control[Func]`'s `shift` */
  inline def shift[F[_], R, A](inline f: (A => R) => R): Handled[F, R, A] = C.shift(f)

  inline def pure[F[_], R, A](a: A): Handled[F, R, A] = C.pure(a)

  /** apply the program to its final continuation: `Control[Func]`'s `/` */
  inline def run[F[_], R, A](p: Handled[F, R, A])(k: A => R): R = C./(p)(k)

  /** `Control[Func]` at the diagonal: `flatMap` is one lambda per bind, and the JIT inlines the monomorphic call —
   * measured at the ceiling without compile-time inlining of the bind */
  given [F[_], R]: Monad[Handled[F, R, *]] with
    override inline def pure[A](a: A): Handled[F, R, A] = C.pure(a)
    override inline def fmap[A, B](m: Handled[F, R, A], f: A => B): Handled[F, R, B] = C.map(m)(f)
    extension [A](m: Handled[F, R, A])
      override inline def flatMap[B](f: A => Handled[F, R, B]): Handled[F, R, B] = C.flatMap(m)(f)

type Handled[F[_], R, A] = Handled.Handled[F, R, A]

/**
 * A row's staged interpreter: ONE object (or class instance) per row and answer layout, whose `stage` is
 *
 *   inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
 *     inline op match
 *       case State.Get()   => Handled.shift(k => st => k(st._1)(st))
 *       …
 *
 * A staged row is a UNION of its effects' constructors (`[X] =>> State[S, X] | Writer[W, X]`), not the kernel's
 * `F + G`: an operation as written belongs to it by subtyping, which is what the inline parameter and the macro's
 * membership test need. The trait carries the types and nothing else, on purpose: an abstract inline member is not
 * a thing, and `stage` must resolve on the object's OWN type for the inliner to see it — which is why the `direct`
 * macro takes the object as an `inline` parameter rather than summoning a `Stager[F, R]` that would widen to this
 * trait.
 */
trait Stager[F[_], R]

object Stager:
  type Acc[S, W] = (S, Vector[W])
  /** the answer type of a stateful, logging block: a function of the accumulator, the state threaded as a value —
   * the layout `((S, Vector[W]), A)` the fused fixture answers in */
  type Answer[S, W, A] = Acc[S, W] => (Acc[S, W], A)

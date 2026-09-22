package okay

/**
 * specs/direct-staged.md: a program over a row with its handler
 * known at the call site.
 *
 * A `Handled[Row, R, A]` is `Func` — the program as a function of its
 * continuation, at answer type R — with the row and the answer carried
 * in the type so that the `direct` macro can read them off a block's
 * F. Its operations are not dispatched: a `Stager` object's `stage` is
 * an INLINE MATCH over the row's constructors, applied by the macro to
 * the operation term as the block wrote it, and the compiler picks the
 * arm. Measured before it was designed (staged-block-lanes, then this
 * lane's `blockFuncInlineMatchR`): on a static 10-op block inside a
 * 100-iteration loop, 8.15 µs / 84 568 B against the fused Free
 * fixture's 11.7 / 123 712 and the shipping nested runners' 15.0 /
 * 154 448 — the arm chosen at compile time is the whole of the win;
 * the same block with `split` running at run time is 1.0x.
 *
 * `Func`'s contract is this type's contract: fast, fused, NOT
 * stack-safe on a left-nested chain. A block that loops ten thousand
 * operations is fine; one that must loop a million runs as a Free
 * block under `Cont`.
 */
object Handled:
  opaque type Handled[Row[+_], R, A] = (A => R) => R

  /** an operation's meaning: what it does with its continuation */
  inline def shift[Row[+_], R, A](inline f: (A => R) => R): Handled[Row, R, A] = f

  inline def pure[Row[+_], R, A](a: A): Handled[Row, R, A] = k => k(a)

  /** apply the program to its final continuation */
  inline def run[Row[+_], R, A](p: Handled[Row, R, A])(k: A => R): R = p(k)

  /** closure composition, as `Control[Func]`: `flatMap` is one lambda
   * per bind, and the JIT inlines the monomorphic call — measured at
   * the ceiling without compile-time inlining of the bind */
  given [Row[+_], R]: Monad[Handled[Row, R, *]] with
    override inline def pure[A](a: A): Handled[Row, R, A] = k => k(a)
    override inline def fmap[A, B](m: Handled[Row, R, A], f: A => B): Handled[Row, R, B] =
      k => m(x => k(f(x)))
    extension [A](m: Handled[Row, R, A])
      override inline def flatMap[B](f: A => Handled[Row, R, B]): Handled[Row, R, B] =
        k => m(f(_)(k))

type Handled[Row[+_], R, A] = Handled.Handled[Row, R, A]

/**
 * A row's staged interpreter: ONE object (or class instance) per row
 * and answer layout, whose `stage` is
 *
 *   inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
 *     inline op match
 *       case State.Get()   => Handled.shift(k => st => k(st._1)(st))
 *       …
 *
 * The trait carries the types and nothing else, on purpose: an
 * abstract inline member is not a thing, and `stage` must resolve on
 * the object's OWN type for the inliner to see it — which is why the
 * `direct` macro takes the object as an `inline` parameter rather
 * than summoning a `Stager[Row, R]` that would widen to this trait.
 */
trait Stager[Row[+_], R]

object Stager:
  type Acc[S, W] = (S, Vector[W])
  /** the answer type of a stateful, logging block: a function of the
   * accumulator, as `PState` threads its state — the layout
   * `((S, Vector[W]), A)` the fused fixture answers in */
  type Answer[S, W, A] = Acc[S, W] => (Acc[S, W], A)

  /** the canonical stage: `State % S + Writer % W`, the row every
   * fusion number in this repository was taken on. A user's row is an
   * object of this shape — the arms are the handler's, one per
   * constructor, with the accumulator threaded as a value (never a
   * cell: a captured continuation must be re-runnable). */
  final class StateWriter[S, W, A] extends Stager[State % S + Writer % W, Answer[S, W, A]]:
    type Row = State % S + Writer % W
    type R = Answer[S, W, A]

    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case State.Get() => Handled.shift[Row, R, X](k => st => k(st._1)(st))
        case State.Set(s2) => Handled.shift[Row, R, X](k => st => k(s2)((s2, st._2)))
        case Writer.Say(v) => Handled.shift[Row, R, X](k => st => k(())((st._1, st._2 :+ v)))

    /** run a block from an initial state: the state and the log in
     * the row's layout, then the answer */
    def run(s: S)(p: Handled[Row, R, A]): ((S, Vector[W]), A) =
      Handled.run(p)(a => acc => (acc, a))((s, Vector.empty))

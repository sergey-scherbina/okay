package okay

/**
 * specs/direct-staged.md: a program over a row with its handler
 * known at the call site.
 *
 * A `Handled[F, R, A]` is `Func` — the program as a function of its
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
  opaque type Handled[F[+_], R, A] = (A => R) => R

  /** an operation's meaning: what it does with its continuation */
  inline def shift[F[+_], R, A](inline f: (A => R) => R): Handled[F, R, A] = f

  inline def pure[F[+_], R, A](a: A): Handled[F, R, A] = k => k(a)

  /** apply the program to its final continuation */
  inline def run[F[+_], R, A](p: Handled[F, R, A])(k: A => R): R = p(k)

  /** closure composition, as `Control[Func]`: `flatMap` is one lambda
   * per bind, and the JIT inlines the monomorphic call — measured at
   * the ceiling without compile-time inlining of the bind */
  given [F[+_], R]: Monad[Handled[F, R, *]] with
    override inline def pure[A](a: A): Handled[F, R, A] = k => k(a)
    override inline def fmap[A, B](m: Handled[F, R, A], f: A => B): Handled[F, R, B] =
      k => m(x => k(f(x)))
    extension [A](m: Handled[F, R, A])
      override inline def flatMap[B](f: A => Handled[F, R, B]): Handled[F, R, B] =
        k => m(f(_)(k))

type Handled[F[+_], R, A] = Handled.Handled[F, R, A]

/**
 * A row's staged interpreter: ONE object (or class instance) per row
 * and answer layout, whose `stage` is
 *
 *   inline def stage[X](inline op: F[X]): Handled[F, R, X] =
 *     inline op match
 *       case State.Get()   => Handled.shift(k => st => k(st._1)(st))
 *       …
 *
 * The trait carries the types and nothing else, on purpose: an
 * abstract inline member is not a thing, and `stage` must resolve on
 * the object's OWN type for the inliner to see it — which is why the
 * `direct` macro takes the object as an `inline` parameter rather
 * than summoning a `Stager[F, R]` that would widen to this trait.
 */
trait Stager[F[+_], R]

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

  /** specs/direct-stagers.md: every effect a staged block can hold,
   * in ONE layout — the environment a plain argument (it never
   * changes), the state and the log threaded as a value, the error as
   * the answer's Left. A subrow instantiates the slots it does not
   * use with `Unit` (a state or environment never read) and `Nothing`
   * (a log or error nothing constructs): the arms for those members
   * are in the match and never chosen. Reader + State + Writer +
   * Throws is the whole of what a direct block is written over when
   * it is not Async — Async is a suspension, not an arm. */
  final class All[E, S, W, Err, A] extends Stager[Reader % E + State % S + Writer % W + Throws % Err, All.Answer[E, S, W, Err, A]]:
    type Row = Reader % E + State % S + Writer % W + Throws % Err
    type R = All.Answer[E, S, W, Err, A]

    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Reader.Ask() => Handled.shift[Row, R, X](k => env => acc => k(env)(env)(acc))
        case State.Get() => Handled.shift[Row, R, X](k => env => acc => k(acc._1)(env)(acc))
        case State.Set(s2) => Handled.shift[Row, R, X](k => env => acc => k(s2)(env)((s2, acc._2)))
        case Writer.Say(v) => Handled.shift[Row, R, X](k => env => acc => k(())(env)((acc._1, acc._2 :+ v)))
        // the early end: the continuation is dropped, the answer is the error
        case Throws(e) => Handled.shift[Row, R, X](_ => _ => acc => (acc, Left(e)))

    /** run a block in an environment from an initial state: the state
     * and the log at the end — or at the raise — then the answer or
     * the error; `run` IS the block's catch */
    def run(env: E, s: S)(p: Handled[Row, R, A]): ((S, Vector[W]), Either[Err, A]) =
      Handled.run(p)(a => _ => acc => (acc, Right(a)))(env)((s, Vector.empty))

  object All:
    type Answer[E, S, W, Err, A] = E => Acc[S, W] => (Acc[S, W], Either[Err, A])

  // ---- the singles: the same arms with the tuple removed (a
  // one-effect block pays for one effect; the benchmark says how much
  // the tuple was — specs/direct-stagers.md, Results)

  final class Reading[E, A] extends Stager[Reader % E, E => A]:
    type Row = Reader % E
    type R = E => A
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Reader.Ask() => Handled.shift[Row, R, X](k => env => k(env)(env))
    def run(env: E)(p: Handled[Row, R, A]): A = Handled.run(p)(a => _ => a)(env)

  final class Stateful[S, A] extends Stager[State % S, S => (S, A)]:
    type Row = State % S
    type R = S => (S, A)
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case State.Get() => Handled.shift[Row, R, X](k => s => k(s)(s))
        case State.Set(s2) => Handled.shift[Row, R, X](k => _ => k(s2)(s2))
    def run(s: S)(p: Handled[Row, R, A]): (S, A) = Handled.run(p)(a => s => (s, a))(s)

  final class Logging[W, A] extends Stager[Writer % W, Vector[W] => (Vector[W], A)]:
    type Row = Writer % W
    type R = Vector[W] => (Vector[W], A)
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Writer.Say(v) => Handled.shift[Row, R, X](k => log => k(())(log :+ v))
    def run(p: Handled[Row, R, A]): (Vector[W], A) = Handled.run(p)(a => log => (log, a))(Vector.empty)

  final class Failing[Err, A] extends Stager[Throws % Err, Either[Err, A]]:
    type Row = Throws % Err
    type R = Either[Err, A]
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Throws(e) => Handled.shift[Row, R, X](_ => Left(e))
    def run(p: Handled[Row, R, A]): Either[Err, A] = Handled.run(p)(Right(_))

package okay

import okay.one.Cont

/**
 * DANVY–FILINSKI'S `(A => S) => R` ON THE KERNEL: `Cps[A, S, R]` is okay-core's one monad under one `$` over no
 * effects, its answer moved from `S` to `R` — the root's `Carrier`, `Cont` itself, no structure of its own. The words
 * okay-freer's `Cps` had: `Cps.Pure`, `Cps.defer`/`delay` (a thunk the machine forces: a tail call with no host
 * frame), `Cps.shift`, `c / k`, `Cps.reset`; `A />> R` the diagonal, an ordinary monad. The machine is the kernel's,
 * on the heap: a deep recursion through `defer` takes no host stack. A `shift`'s `k` is STRICT — the rest run to its
 * value, a run of its own (the kernel's own `k` is a program: `Atm.shift0`).
 */
type Cps[A, S, R] = Cps.Rep[A, S, R]
/** the diagonal: an ordinary monad */
infix type />>[A, R] = Cps[A, R, R]
/** what `reset` can delimit */
infix type ^[A, R] = Cps[A, A, R]

object Cps:
  /** the kernel's `Cont` itself, opaque only so that its own words (`flatMap` with the answer flowing as
   * Danvy–Filinski's typing has it, `/`) are found with it, no import — nothing at run time */
  opaque type Rep[A, S, R] = Carrier[A, S, R]
  /** the kernel's program behind it */
  def of[A, S, R](c: Carrier[A, S, R]): Cps[A, S, R] = c
  extension [A, S, R](c: Cps[A, S, R])
    /** the kernel's program */
    def cont: Carrier[A, S, R] = c
    def flatMap[B, S2](f: A => Cps[B, S2, S]): Cps[B, S2, R] = Cont.Bind(c, f)
    def map[B](f: A => B): Cps[B, S, R] = Cont.Map(c, f)
    /** `c` applied to its continuation: its answer */
    infix def /(k: A => S): R =
      import okay.one.Cont.run
      (c: Carrier[A, S, R]).run(a => Cont.Return[okay.one.row.Pure, Nothing, S](k(a))).run
  /** a value, at any answer */
  def Pure[A, R](a: A): Cps[A, R, R] = Cont.Return(a)
  /** a bind whose left side is a thunk the machine forces: tail calls with no host frame */
  def defer[A, B, S, T, R](thunk: () => Cps[A, T, R])(f: A => Cps[B, S, T]): Cps[B, S, R] =
    Cont.Bind(Cont.Defer(thunk), f)
  /** `defer` with nothing after it */
  def delay[A, S, R](thunk: () => Cps[A, S, R]): Cps[A, S, R] = Cont.Defer(thunk)
  /** flatMap in prefix form */
  def bind[A, B, S, S2, R](c: Cps[A, S, R])(f: A => Cps[B, S2, S]): Cps[B, S2, R] = Cont.Bind(c, f)
  /** map in prefix form */
  def mapped[A, B, S, R](c: Cps[A, S, R])(f: A => B): Cps[B, S, R] = Cont.Map(c, f)
  /** `shift`: `f` given `k`, the rest up to the reset, STRICT (run to its value) */
  def shift[A, S, R](f: (A => S) => R): Cps[A, S, R] = summon[Control[Carrier]].shift(f)
  /** the program applied to its continuation `k`: its answer */
  def run[A, S, R](c: Cps[A, S, R])(k: A => S): R = c / k
  /** delimit and run: `c / identity` */
  def reset[A, R](c: Cps[A, A, R]): R = run(c)(identity)

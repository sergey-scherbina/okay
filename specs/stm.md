# STM — one transaction language, a family of handlers

## Overview
Software transactional memory for the stack, in the stack's own
shape: a transaction is a PROGRAM in its own effect row (`A ! Tx`),
the cell is one type (`TRef[A]`), and WHERE a transaction runs —
and by which strategy — is a typeclass door, `Stm[F]`. The
operator's brief (2026-09-02): a family of STMs behind a typeclass,
each implementation optimized for its case, and the Channel on the
same machinery without losing throughput.

## Interface
```scala
final class TRef[A](init: A)            // the cell: value + version, an AtomicReference
  def get: A                            // a plain read, outside any transaction
  def modify[B](f: A => (A, B)): B      // the single-cell transaction: ONE CAS

enum Tx[A]:                             // the transaction language
  case Read[A](r: TRef[A])              extends Tx[A]
  case Write[A](r: TRef[A], a: A)       extends Tx[Unit]
  case Modify[A, B](r: TRef[A], f: A => (A, B)) extends Tx[B]
  case Retry()                          extends Tx[Nothing]   // block until something read changes

def read/write/modify/retry/check       // the operations as programs

trait Stm[F[_]]:                        // the door
  def atomically[A](tx: A ! Tx): A ! F

object Stm:
  val tl2: Stm[Async]                   // JVM/Native: versions, CAS-owned commit, structural fast paths
  val direct: Stm[Async]                // JS: one thread, no logs — a transaction is atomic by construction
  val sim: Stm[Sim.Op]                  // deterministic: the Sim scheduler interleaves at every operation
```
The row `Tx` has no `Async`, no `Run`: I/O inside a transaction is a
compile error, which is the one STM property most libraries can only
document.

## Behavior
- [x] a transaction sees a CONSISTENT snapshot: every read re-validates
      the versions of everything read before it, so the values a body
      computes on are the state as of its last read; a changed
      version aborts and restarts the attempt
- [x] commit is atomic and never parks: the write-set's slots are
      OWNED by CAS one at a time, the read-set validated, the new
      versions installed, ownership released — a transaction that
      meets an owned slot does not wait, it aborts and retries
      (obstruction-free: no thread ever holds anything a scheduler
      could park it on)
- [x] `retry` parks the TRANSACTION, not a thread: waiters are
      registered on the read-set's refs, one-shot; the first commit
      that changes one of them re-attempts the transaction on the
      committing thread — the Channel's hand-off, generalized
- [x] structural fast paths, chosen by the handler from the program's
      SHAPE, not by the caller: a transaction that IS one `Modify`
      runs as `TRef.modify` — one CAS, no log, no versions beyond the
      cell's own; one `Read` is a plain read. Everything else goes
      through the log
- [x] the Channel's state lives in a `TRef[State]` and its
      transitions go through `TRef.modify` — the same single-CAS path
      the handler takes for a one-op transaction, so a channel IS a
      one-cell STM structure and the full transaction language works
      on its cell; measured, no loss (Results)
- [x] the Sim handler runs transactions under the deterministic
      scheduler: every Read/Write/Modify is a scheduling point
      (`Sim.yieldNow`), commits validate versions like TL2, and a
      `retry` sleeps one virtual millisecond and re-attempts — the
      same transaction code, every interleaving reproducible by seed
- [x] the direct handler (JS): one thread and a row without
      suspension make a transaction atomic by construction; it runs
      Read/Write/Modify straight against the cells, no log; `retry`
      registers the same one-shot waiters

## Out of scope
- opacity by global clock (TL2 proper): per-ref versions with
  incremental validation give a consistent snapshot without the
  global counter — a global clock would put one contended atomic on
  the Channel's fast path, which is the throughput the brief
  protects. Revisit if a workload shows read-heavy transactions
  aborting too often.
- `orElse` (composable alternatives on retry) — the language can
  grow it as `OrElse(a, b)`; not needed by the first consumers.
- nested `atomically` — refused by the row: a transaction cannot
  contain an `Async`, so it cannot contain another `atomically`.

## Design
- **One cell.** No `Cell` for CAS and `TRef` for transactions: the
  strategy is the handler's choice from the program's shape, so code
  written against `TRef` moves between handlers unchanged.
- **The program is data.** A freer program can be inspected before
  it is run; `Stm.tl2` looks at the tree's root: `Effect(Modify)` is
  the one-CAS path, `Effect(Read)` the plain read, anything with a
  `Bind` the general path. No annotation from the caller.
- **Callbacks after the commit.** As in the Channel: the transaction's
  ANSWER may be an action (`() => Unit`) that the caller runs after
  `atomically` returns; the handler itself runs nothing but the
  transaction — on retry, a pure function is re-run, never a side
  effect.
- **Waiters live on cells.** A `TRef` carries a one-shot waiter list;
  `modify` and commits wake it only when the value changed. The
  common case (no waiters) is one volatile read on the fast path.

## Decisions
- **Per-ref versions, incremental validation, no global clock** —
  the Channel's fast path stays one CAS (the brief). A body between
  reads holds the state as of its last validation, so it never
  computes on a torn view; the cost is O(k²) version checks for k
  reads, and k is small everywhere this is used.
- **Obstruction-free commit, not lock-free MCAS** — an owned slot is
  a CAS-installed marker released within the same commit; a
  contender aborts, it never waits. This is the honest reading of
  "no locks": no thread is ever blocked, though a transaction can be
  delayed by retrying.
- **`Sim` gets a handler, not new ops** — one `Yield` op suffices
  for the scheduler to interleave at every transactional step.
- **Stamped values, no wrapper on the fast path** — the first cut
  wrapped every value in a `Slot(value, version, owner)` and cost
  the Channel 10% (A/B, three rounds). A value may carry its own
  version (`TRef.Stamped`, an abstract CLASS so the type test is a
  primary-supers check, not an interface scan — that alone was half
  the gap): the cell installs it bare and stamps it; `Slot` wraps
  only other values; the `Owned` marker exists only during a
  commit. A Stamped value belongs to one cell and one install — an
  immutable case class rebuilt by `copy` on every transition, as the
  Channel's State is, satisfies that by construction.
- **`Slot` IS a `Stamped`** (stm-slot-stamped, 2026-09-02, the
  operator's suggestion). The cell's content is two kinds, not
  three: a Stamped — the value itself, or a Slot wrapping any other
  value, which is a Stamped too with `value` overridden — or the
  Owned marker. `Stamped.value` defaults to `this`, so `valueOf` and
  `versionOf` are a field read behind one Owned check and `modify`
  has one path for every value type. The caveat that follows:
  `wrap` stamps whatever Stamped it is given, so it must only ever
  see what a transaction or modify PRODUCED — never an existing
  Slot; the code has no such path and the comment says so. A/B
  (three rounds): equal within noise on both channel paths.
  `Slot[+A]` is generic for the reader (stm-slot-generic); `Stamped`
  stays unparameterized because a typed `value = this` would need an
  F-bound on every user value for nothing but that field.
- **The cell holds ONE type** (stm-one-content, 2026-09-02, the
  operator: no AnyRef, no casts between the kinds). `Owned` extends
  `Stamped` and mirrors its content's stamp and value, so the
  reference is `AtomicReference[Stamped]`, `valueOf`/`versionOf`
  are gone (a field read), and `Owned` is matched only where
  ownership MEANS something: the fast path spins on it, a
  transactional read aborts on it, a commit's ownership CAS fails on
  it. The one cast left in the cell is `value: Any` to `A` in
  `TRef.get`/`modify` — the price of a bare value being its own
  content; the handlers' remaining casts are the freer interpreter's
  erasure over `Tx[Any]`, the same shape every handler in the stack
  has. A/B: equal within noise.

## Results
Landed (stm, 2026-09-02): see CHANGELOG. Channel benchmark
(src/jmh ChannelBenchmark, alternating A/B rounds, medians, busy
host; rows in src/jmh/history.tsv): the buffer path (offer, then a
receive that finds the element, ×1000) 32.1 µs on master's
AtomicReference[State] vs 29.6 µs on TRef.modify — 8% FASTER
(one type test and no unwrapping where the old code pattern-matched
a tuple); the program path (send and receive as Async programs
under runAsync, ×1000) 65.4 vs 65.8 µs — equal within noise. The
brief ("without losing performance") holds. Tests: transfers under
eight threads keep the sum and a reader never sees a torn pair;
retry parks the transaction and only the commit that changes what
it read wakes it; a thousand parked transactions hold no thread;
the cross suite runs the same programs through tl2 and direct; the
Sim suite runs them under sixty seeds and checks the scheduler did
interleave.

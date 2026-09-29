# Merge mechanisms and wait strategies

A `merge` of two sources is decided by three values, each a `given`
a caller may swap with one line and every call site compiles
unchanged:

| given | what it decides | the choices | default |
|---|---|---|---|
| `Merge` | HOW the sides are joined | `Merge.Ready`, `Merge.Shared` | `Ready` |
| `Wait` | HOW a consumer waits for the next element before it blocks | `Register`, `Spin(n)`, `Ladder(s, y, p)`, `Cycle(s, y, c)` | `Ladder(100, 50, 4)` |
| `Pause` | WHAT one rung of a wait does on this platform | the platform's, or your own | `PlatformPause` |

`Wait` and `Pause` are not the merge's alone: the blocking runner
(`Async.run`, `runWith`, `toLazyList`) asks the same `Wait` before it
parks on any `Await` that can be polled, a drained channel included.
The design and the measurements are in specs/ready-merge.md; this page
is the API.

## The default, and what it does

```scala
val merged = Source.of(List(1, 2, 3)) merge Source.of(List(10, 20))   // Merge.Ready, Wait.Ladder(100, 50, 4)
val all = merged.runCollect.runWith
```

Under `Merge.Ready` each side is buffered onto a fiber of its own,
into a ring nobody else writes, and the consumer walks a ring of the
SIDES: a side with something ready is drained in a batch, a side with
nothing is skipped, and the only thing that crosses threads is one
index per wake-up, never an element. `merge` promises no order between
its sides, only within each.

When every side is empty the ring is dry, and the consumer does not
register a callback at once: it asks the given `Wait`, which polls the
sides on its own schedule and answers whether something came. Only
when the wait gives up does the consumer register with each side and
park. Every rung of the wait is the consumer's own cost. The producer
pays only for the last one, a real wake-up, and that is now rare.

## `Merge`: the mechanism

```scala
trait Merge:
  def elements[A](l: Source[A], r: Source[A], capacity: Int)
                 (using Scheduler, CanBlock, Timer, Wait, Pause): Source[A]
  def chunks[A](l: Source[A], r: Source[A], slots: Int, size: Int, within: Option[Long])
               (using Scheduler, CanBlock, Timer, Wait, Pause): Source[Chunk[A]]
  def flushing[A](l: Flushing[A], r: Flushing[A], slots: Int, size: Int, within: Option[Long])
                 (using Scheduler, Timer, Wait, Pause): Source[Chunk[A]]
```

The three joins are the three shapes `Source` offers, and every
public spelling dispatches to the given:

| call | join | notes |
|---|---|---|
| `a merge b` | `elements` | `capacity` elements a side, 64 by default |
| `a.merge(b, chunked = true)` | `chunks` | chunks of `Source.ChunkSize`, `capacity / ChunkSize` slots a side; `flushAfter = Some(ms)` flushes a partial chunk on time |
| `a mergeFlushing b` | `flushing` | sources that mark their own boundaries with `Flush.now`; always chunked |
| `a either b` | `elements` | the same join, tagged `Left`/`Right` |

Two mechanisms implement them:

- `Merge.Ready` — the default. A channel per side, joined by readiness
  on the consumer's own thread of control (`ReadyMerge`), waiting by
  the given `Wait` before it registers. Measured on the elementwise
  join 0.73-0.93x of the shared queue at every capacity (64, 256,
  1024) with 12-14% fewer bytes; on the chunked join 1.03x, at parity;
  on the flushing join 1.05x under Loom over five alternating rounds,
  and 0.88x on the `adaptive` default, where the ring is the faster
  road (merge-flush-on-ring-gap, 2026-09-29). Those rows were taken
  under Loom; on the `adaptive` default a chunked merge is 1.5-1.9x
  slower on both roads, an open item (backlog
  `adaptive-chunked-merge-cost`).
- `Merge.Shared` — one queue both producers feed (`Channel.merge`,
  `Channel.mergeChunked`, `Channel.mergeFlushing`). The road before
  the ring, kept as a door by choice. Its consumer never catches up,
  so it batches by construction and is never bimodal; what it pays is
  every producer contending for one head.

```scala
given Merge = Merge.Shared
val joined = Source.of(List(1, 2, 3)).merge(Source.of(List(10, 20)), chunked = true)
```

`Source.mergeReady(a, b, ...)` and `a mergeReady b` take no `Merge`
at all: they ARE the ring, with no fiber and no channel, for sources
that are already ready or that bring their own asynchrony. A side that
computes for a millisecond holds the others for that millisecond; give
such a side a core with `Channel.buffer(n)(s).drained` and the merge
reads it as one more source that is sometimes not ready.

```scala
infix def mergeReady[B](t: Source[B])(using Wait, Pause): Source[A | B] =
```

## `Wait`: the strategy

```scala
trait Wait:
  def until(ready: () => Boolean)(using p: Pause): Boolean
```

`until` polls `ready` on the strategy's own schedule and answers
whether the condition came. `false` means it gave up: the caller
registers a callback and parks, and the strategy has already called
`Pause.block()` to say so. Four strategies are in the companion:

```scala
object Register extends Wait:
final case class Spin(polls: Int) extends Wait:
final case class Ladder(spins: Int, yields: Int, sleeps: Int) extends Wait:
final case class Cycle(spins: Int, yields: Int, cycles: Int) extends Wait:
given Wait = Ladder(100, 50, 4)
```

| strategy | what it does | when |
|---|---|---|
| `Register` | never waits: block at once | JS's shape, and the road before the hybrid; a consumer that must not burn a core |
| `Spin(polls)` | `polls` looks, a `spin` between each, then block | producers a few hundred nanoseconds away and a core to spare |
| `Ladder(spins, yields, sleeps)` | `spins` looks spinning, then `yields` looks each after a yield, then `sleeps` looks each after a nano-sleep, then block | the default; took the chunked ring road's tail away |
| `Cycle(spins, yields, cycles)` | (`spins` looks, `yields` yield-looks, one nano-sleep) × `cycles`, one last look, then block | a choice, not the default: it re-spins after every sleep, and measured a tail because that brings the consumer back to the producer's cache line too soon |

Measured on a Mac (JDK 26): `Thread.yield` 125 ns; `parkNanos(1)`
10-12 us on a platform thread and 10 us on a virtual one, the timer's
floor, so the argument below ~10 us does not matter;
`parkNanos(100 000)` 150-158 us. One nano-sleep is the window in which
two producers make ~50 chunks, so a consumer that slept wakes into a
batch. That is why the ladder beats a spin: on the chunked ring road,
10 forks each, `Ladder(100, 50, 4)` read 200.4 ± 3.8 us with no fork
above 225 where the spin-only road had 2-4 forks of 10 at 215-247 and
`Cycle(100, 50, 4)` read 208.0 ± 7.8 with 3 of 10 at 220-247.

```scala
given Wait = Wait.Register
```

```scala
given Wait = Wait.Spin(1000)
```

```scala
given Wait = Wait.Cycle(100, 50, 4)
```

A strategy of your own is one method. This one yields ten times and
gives up:

```scala
given Wait = new Wait:
  def until(ready: () => Boolean)(using p: Pause): Boolean =
    var i = 0
    var got = false
    while !got && i < 10 do { got = ready(); if !got then p.yieldNow(); i += 1 }
    if !got then p.block()
    got
```

## `Pause`: the rungs

```scala
trait Pause:
  def threads: Boolean
  def spin(): Unit
  def yieldNow(): Unit
  def nano(): Unit
  def block(): Unit
```

`threads` is the platform's fact a strategy needs: whether producers
run on threads of their own, so that a poll can find what the last
one missed at all. On JS it is `false`, and every strategy in the
companion blocks at once, so a program that merges on the JVM and on
JS carries the same `given` and behaves right on both. `spin` is
`Thread.onSpinWait`, `yieldNow` is `Thread.yield`, `nano` is
`LockSupport.parkNanos(1)`, and `block` is the wait giving up: the
caller is about to register and park.

A test or a debugger substitutes its own, to count the rungs or to
refuse to sleep. This one counts, and shows exactly what the default
ladder does when nothing ever comes:

```scala
final class CountingPause extends Pause:
  var spins, yields, nanos, blocks = 0
  def threads = true
  def spin(): Unit = spins += 1
  def yieldNow(): Unit = yields += 1
  def nano(): Unit = nanos += 1
  def block(): Unit = blocks += 1
```

```scala
val pause = CountingPause()
val came = Wait.Ladder(100, 50, 4).until(() => false)(using pause)
// came == false; pause.spins == 100, pause.yields == 50, pause.nanos == 4, pause.blocks == 1
```

## Who asks the wait

| consumer | how it waits |
|---|---|
| `ReadyMerge` (every `Merge.Ready` join, `mergeReady`) | on a dry ring, the given `Wait` over the sides' polls; registers only when it gave up |
| the blocking runner: `Async.run`, `runWith`, `toLazyList` | an `Await` that carries a poll is asked by the given `Wait` on the runner's own thread, which no producer needs; parks only when the wait gave up. So `Merge.Shared`'s consumer and a plain `channel.drained` wait the same way |
| the callback drive: `runAsync`, fibers | polls ONCE, takes an answer in place, otherwise registers. After its first callback it runs on whoever woke it, a producer's thread as often as not, and a wait there would stall the very producer it waits for |

An `Await` without a poll (a timer, a channel that does not offer
one) parks as it always did, whatever the given `Wait`.

## Choosing

- Leave the defaults. `Merge.Ready` with `Ladder(100, 50, 4)` is the
  measured best on the elementwise and chunked joins, and it is what
  the numbers above describe.
- `Merge.Shared` when the flushing join is your hot path and 12% in
  one round matters more than the tail, or to hold the old road as a
  control beside the new one in a benchmark of your own.
- `Wait.Register` where the consumer's core is not yours to burn:
  inside a request handler, on a loaded box, or when a profile shows
  the spin rung and not the work.
- `Wait.Spin(n)` when producers answer within a few hundred
  nanoseconds and you have measured that the yield and sleep rungs
  never fire.
- Your own `Pause` in a test that must not sleep, or to count the
  rungs a wait climbed and assert on them.

The bound to keep in mind when tuning: a strategy that spins for about
as long as a block costs before blocking is within a factor of two of
the best possible, and no fixed choice that cannot see the future does
better. So the ladder's four sleeps of ~10 us against a block that
costs a producer's wake-up is not a magic number, it is that bound
written down.

## Literature

- LMAX Disruptor, `WaitStrategy`: `BusySpinWaitStrategy`,
  `YieldingWaitStrategy`, `SleepingWaitStrategy`,
  `BlockingWaitStrategy`. `Wait` is that family made a value the caller
  chooses, and `Ladder` is `SleepingWaitStrategy`'s spin-yield-sleep
  order as three counts.
- Anna Karlin, Mark Manasse, Lyle McGeoch and Susan Owicki,
  "Competitive randomized algorithms for non-uniform problems",
  Algorithmica 1994: the spin-then-block bound above.
- Rust `futures::stream::select_all`, with `Poll::Pending` and `Waker`:
  the readiness merge is the same shape, and `Async.Await(register,
  poll)` is `Pending` plus a `Waker` that can also be asked.
- Oleg Kiselyov, "Iteratees" (2012): the merge is written in the pull
  form, so that the merged source is consumed by any iteratee
  unchanged (docs/theory).

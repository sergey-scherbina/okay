# Channels and queues

## What this is, and who needs it

A **channel** is a place one part of a program puts values and another
part takes them out. That is all it is. Everything else on this page
is consequences of two questions:

1. **What must it promise?** If I put something in and was told yes,
   will it come out? When the producer says "that's all", does the
   consumer see the last elements or lose them?
2. **What should it run on?** An array? A growable one? Several
   independent ones? The answer changes only the timing — never the
   behaviour.

The two are independent, and keeping them apart is the whole design.
`Channel[A](capacity)` answers both for you, sensibly, and most code
should never look further:

```scala
val c = Channel[Int](1024)
c.send(42)          // an effect: suspends if the buffer is full
c.receive           // an effect: suspends while empty and open
c.close()           // ends the stream -- what is buffered still drains
```

Read on if you need something the default does not give: a producer
that must never wait, a channel usable inside an STM transaction, or
throughput with many producers at the cost of their mutual order.

## Why any of this is hard

Putting values in an array and taking them out is easy. Three things
make a channel harder than that, and every design decision below comes
from one of them.

**Concurrency without locks.** Several producers and consumers touch
the same structure at once. A lock would make it simple and slow; the
whole point of these structures is that every operation either
completes or answers "full"/"empty" to its caller, and never waits on
another thread making progress.

**Termination.** A queue has no end. A channel does — someone
eventually says "no more", and every consumer has to learn about it
*after* receiving everything that was already accepted, not before.
This is the part that is genuinely difficult, and §2 is about why.

**Backpressure.** If the producer is faster than the consumer,
something must give: block the producer, drop elements, or grow
without bound. That choice belongs to the caller, which is why
capacity is not a detail.

---

## 1. The contract — what it promises

### The strong one — `Queues.strong`

Two promises, and the second is stronger than a queue usually gives:

- **acceptance is final.** If `send` answered `true`, that element
  *will* be delivered.
- **close drains.** `close()` ends the stream only after everything
  already buffered has been handed over.

Compare `zio.Queue.shutdown`, which interrupts pending offers and
takes and promises no drain at all; or `java.util.concurrent`'s
queues, which have no notion of "closed" and leave termination to the
caller — usually as a poison pill invented per project.

The promise is not free, and the interesting part is *where* it is
paid. Measured two ways on the same load (`ChannelGuaranteeBenchmark`,
N=4000, cap=1024):

| how drain-on-close is bought | us/op |
|---|---|
| as an invariant of the mechanism (`StmChannel`) | 166.1 |
| as a mark travelling in the stream | 115.9 |
| not bought at all (`AbruptChannel`) | 113.1 |

Bought as a mark it costs 2.4%; baked into the mechanism it costs 47%.
That is why the default channel carries termination as an *element*:
it takes a position through the same atomic as everything else, so the
buffer's own ordering settles it and there is no second structure to
reconcile. Four earlier drafts tried to reconcile a "closed" flag with
a separate buffer, and each needed an in-flight counter that leaked a
new race.

#### Why drain-on-close is the hard promise

It sounds trivial: keep a `closed` flag, and let the consumer stop
when the flag is set and the buffer is empty. That design is wrong,
and it is wrong in a way that shows up once in a few thousand runs.

Here is the race, written out. A producer wants to send:

```
producer                          consumer / closer
--------                          -----------------
1. read `closed`  -> false
                                  2. close(): set `closed` = true
                                  3. buffer is empty and closed
                                     -> end the stream
4. write the element into
   the buffer, return TRUE
```

The producer was told **true**, so by the contract that element must
be delivered — and the stream has already ended. It is stranded. Swap
the order and you get the mirror: the producer is told **false**,
resends elsewhere, and the element is delivered twice.

The usual patch is an "in-flight" counter: increment before the check,
decrement after the write, and end only when `closed && inFlight == 0
&& empty`. Four drafts of a ring-backed channel in this repository
went that way. Each one moved the window rather than closing it — the
counter itself has to be read at some instant, and the check-then-act
reappears one level up. They failed the gate roughly one run in three,
which is exactly the frequency that wastes a week.

**What works is to stop having two structures.** Termination becomes
an ELEMENT. Closing pushes a *mark* through the same buffer, claiming
a position with the same atomic as any element, so the buffer's own
ordering decides what is before and after it. There is no flag to
reconcile with a buffer, because there is no flag.

One detail completes it. A sender decides *what to publish* only
**after** it has won its position — if the close landed first, it
publishes a *void* the receiver steps over, and answers `false`. So a
sender ordered after the mark always sees the close (its own atomic is
the fence) and never claims acceptance; a sender ordered before it is
delivered. The window has nowhere left to open.

```
positions:  ..7....  8......  9......  10.....
contents:   elem     elem     MARK     void
            ^ delivered       ^ end    ^ stepped over, sender told false
```

This is why `Buffer` exposes `pushDeciding` rather than a plain
`push`: the decision has to happen inside the claim, or the guarantee
is not there.

### The weak one — `Queues.weak`

`close()` ends it at once and whatever is buffered is abandoned. There
is no termination to detect, so there is nothing to get wrong.

Right for a feed whose remainder is stale the moment the consumer
stops: a UI's event stream, a cancelled request's frames, a shutdown
path. **Wrong** anywhere `send`'s `true` is read as a promise, and
wrong under `Source.merge`, whose consumers rely on draining what the
sources already produced.

### The composable one — `Queues.composable`

The strong contract **plus** STM: the whole channel is one cell, so it
works inside `Tx.orElse`, `retry`, and transactions spanning several
cells. Nothing else here does.

```scala
// take from whichever is ready, atomically
val either = Tx.orElse(left.receiveTx, right.receiveTx)
```

It is also the slowest, and for a structural reason: a transaction
that loses its CAS must have left nothing behind, so the buffer has to
be **persistent**. Persistence is what composability costs.

### Rendezvous — `Queues.rendezvous`

No buffer: a sender waits for a receiver. Its own mechanism, because
the stamp scheme a ring uses cannot express a capacity below two —
`stamp - p = 1 - capacity` is zero exactly at one, and a slot would
read as writable while still full. ZIO draws the same line, handing
capacity 1 to a separate `OneElementConcurrentQueue`.

---

## 2. The mechanism

Every mechanism below satisfies the same laws. They differ in what
they cost and in one guarantee, named where it applies.

### `Ring` — a bounded array

Vyukov's bounded MPMC queue: an array of slots, each carrying a
sequence stamp that says whose turn it is.

- a slot is writable at push position `p` when `stamp == p`
- a push publishes `stamp = p + 1`, which is what a pop at `p` waits for
- a pop publishes `stamp = p + capacity`, which the next lap's push waits for

Worked through, with capacity 4. Slots start stamped with their own
index, and positions keep counting up forever — the slot is
`position & 3`:

```
start        stamp: [0, 1, 2, 3]   head=0  tail=0
push at 0    slot 0 is writable because stamp[0] == 0
             write, then stamp[0] = 1        -> [1, 1, 2, 3]
pop at 0     wants stamp[0] == 0 + 1: yes
             read, then stamp[0] = 0 + 4 = 4 -> [4, 1, 2, 3]
push at 4    slot 0 again (4 & 3 == 0); writable because stamp[0] == 4
```

So one number per slot answers both questions — "may I write here?"
and "is there something to read here?" — and it answers them *without*
looking at the other end's counter. That is what makes "advance the
position, then write the slot" safe: a consumer never reads the tail,
it reads the stamp of the slot it wants. A producer that has claimed
position 9 but not yet written it simply leaves stamp[1] at 9, and the
consumer waiting for 10 sees "not ready" rather than garbage.

The same property has a consequence worth knowing, because it bit us:
"the buffer is not empty" (`tail > head`) and "there is something to
take" (the stamp says published) are **different questions**. A
consumer that checks the first before parking will spin instead of
waiting — and the CPU it burns comes out of the very producer whose
publish it is waiting for. `Buffer` therefore has both `isEmpty` and
`hasReady`, and the one to ask before waiting is `hasReady`. Capacity is rounded up to a power of two so indexing is
a mask, which is why ZIO ships `RingBufferPow2` separately from its
arbitrary-capacity ring.

**Use it when** the buffer should be bounded — that is, when a fast
producer *should* be made to wait. Backpressure is a feature.

### `Segments` — an unbounded array

A linked list of fixed arrays behind the same pair of position
counters. A claim is one `getAndIncrement` (an unbounded buffer can
never refuse), and a batched take still claims a run with one CAS,
crossing segment boundaries.

Segments are **never reused and never freed by hand**. A thread
reaches one by holding a reference, and a segment nobody holds is
garbage — so the hazard a segmented queue usually carries, freeing a
segment while another thread still walks it, has nowhere to live. The
collector does the reclamation.

The stamp is simpler than a ring's for the same reason: every position
is used exactly once, so a slot needs one bit of state rather than a
lap number.

**Use it when** the producer must never wait. `Channel.apply()` with
no capacity gives you this.

### `MultiFifo` — relaxed, for many producers

`parts` independent buffers. A producer is bound to one for the life
of its thread; a consumer takes from whichever has something.

**What it gives up:** the order *between* producers. An element pushed
later into a quiet part can come out before one pushed earlier into a
busy part.

**What it keeps, and why it can still be a channel's buffer:** a
producer stays on *its* part, so one producer's own elements keep
their order among themselves. Nothing is lost or duplicated. The
channel laws state exactly this — "one producer's elements arrive in
the order it sent them" — and it survives.

A relaxed buffer says so: `buffer.parts > 1`. It is stated in the
interface rather than left implicit, because a relaxed buffer that
quietly passes a global-FIFO test is a test that is not testing.

Termination needs care here and the interface carries it. With `k`
parts, a mark placed in one is reached while other parts still hold
elements that were already accepted — the stream would end early. So
`close` **seals every part** and the channel counts the marks back;
only the last one ends the stream. That is what `Buffer.seal` is for.

**Use it when** producers are many and their mutual order means
nothing: metrics from many workers, log lines, independent requests.

**Both bounded and growable parts work, and they answer different
questions.** `Total=8000`, us/op, a chunked consumer on every side
because that is what `ZStream.fromQueue` uses:

| producers | one ring | relaxed, bounded | one growable | relaxed, growable | zio.Queue |
|---|---|---|---|---|---|
| 1 | 205.9 | 306.6 | **157.7** | 249.7 | 427.7 |
| 4 | 1242.0 | 366.9 | 677.0 | **196.0** | 1290.2 |
| 16 | 2073.6 | 362.2 | 680.2 | **129.7** | 2540.8 |

Two things to take from it.

**No single configuration wins everywhere, which is why there is a
menu.** At one producer relaxation costs — 249.7 against a plain
growable buffer's 157.7 — because there is no contention to relax and
a part still has to be chosen. From four producers up the relaxed
growable form pulls away and keeps improving: 196.0, then 129.7.
Everything unpartitioned gets slower as producers are added.

**Read the granularity before reading the numbers.** An earlier
version of this table put our ELEMENTWISE consumer beside zio's
chunked one and showed zio ahead at a single producer. It was the
fifth appearance of that mismatch in this repository, and the first
introduced rather than found. With the same question asked on both
sides, the fastest okay lane leads at every width — 2.7x, 6.6x, 19.6x
— and the elementwise lanes remain as diagnostics, because `zio.Queue`
has no per-element read of a queue to place beside them.

### `ListFifo` and `ArrayFifo` — persistent, for the composable channel

Only `Queues.composable` uses these, because only it needs a buffer
that a losing transaction can abandon without a trace.

Both are the banker's two-list queue — a front in order and a back
newest-first, so `enqueue` is one cons cell. They differ in the front:
`ListFifo` keeps a list, `ArrayFifo` an immutable chunk plus a
per-version index. The array form turns the back round into one
allocation instead of `n` cons cells, makes `dequeue` index
arithmetic, and makes `drop` within the front free. Measured 7% ahead
elementwise and level in batches; it is the default.

---

## 3. Choosing

Answer three questions in order. The first is the only one that can
change what your program *does*.

**1. What must happen to buffered elements when the producer ends?**

| you need | pick |
|---|---|
| everything already accepted must arrive | `strong` |
| the remainder is stale anyway; end at once | `weak` |
| the take must compose with other transactions | `composable` |

**2. What should happen when the consumer falls behind?**

| you need | pick |
|---|---|
| slow the producer down — backpressure | `.bounded(n)` |
| never slow the producer; memory is the limit | `.unbounded` |
| a synchronous handoff, no buffer at all | `Queues.rendezvous` |

**3. How many producers, and does their mutual order matter?**

| situation | pick |
|---|---|
| one or a few producers | leave it — a single buffer |
| many producers, mutual order is meaningful | a single buffer, and accept the contention |
| many producers, mutual order is noise | `.relaxed(parts, each)` |

If question 3 is not obviously "many", the answer is "leave it". A
relaxed buffer at one producer can only lose: a part to choose and
nothing gained.

---

## 4. Recipes

```scala
// the default: strong contract, bounded ring. Backpressure included.
val c = Channel[Int](1024)

// a producer that must never block
val log = Queues.strong[String].unbounded.build

// events where the tail is worthless once the consumer stops
val ui = Queues.weak[Event].bounded(256).build

// sixteen workers reporting metrics; their mutual order is noise
val metrics = Queues.strong[Metric].relaxed(parts = 16, each = 128).build

// a transactional take from either side
val work = Queues.composable[Job](1024).build

// a handoff with no buffer
val baton = Queues.rendezvous[Unit].build

// your own mechanism, for the strong contract. The factory is
// polymorphic because the channel stores more than your element type:
// termination rides the same buffer, so only the channel knows what
// it needs to allocate -- and the mark type stays private, which is
// what stops a caller forging an end of stream.
val mine = Queues.strong[Int].on([T] => (n: Int) => Ring[T](4096)).build
```

### Reading in batches

Everything above is faster read in chunks than element by element,
and the reason is not the channel — it is how many elements one
coordination step carries.

```scala
val total = c.drained.through(Chunks.fold(0)(_ + _)).runLast
```

Measured: elementwise 253us against chunked 56 on the same channel.
The batch is not a relaxation of anything — an element already
buffered is already late, so handing over ten costs no freshness.

---

## 5. What the numbers say

One quiet box, N=4000, cap=1024, us/op. Only lanes sharing a contract
**and** a granularity compare — a rule this repository had to learn
twice, because `ZStream.fromQueue` takes up to 4096 elements per queue
operation and an elementwise loop takes one.

| pair | okay | zio |
|---|---|---|
| unbounded, chunked | **49.1** | 383.8 |
| unbounded, elementwise | **110.5** | 439.1 |
| bounded strong, chunked | **56.2** | 125.9 |
| bounded strong, elementwise | **248.6** | 286.9 |
| weak, chunked | **54.8** | 116.1 |
| weak, elementwise | **237.6** | 304.0 |

Full tables and the methodology, including the runs that were thrown
out, are in [benchmarks.md](benchmarks.md) §14–§16.

### How these numbers were arrived at, and why that matters

The first measurement of this channel against `zio.Queue` read 206us
against 169 and looked like a straightforward "their mechanism is
faster". It was not one thing, and none of the four things it actually
was would have been guessed. They are worth listing because each is a
mistake that is easy to repeat.

**1. The units were different.** `ZStream.fromQueue` takes up to 4096
elements per queue operation; our consumer took one. We were comparing
one coordination step per element against one per batch. Measured at
the same granularity, we were ahead the whole time — 212 against 337.

*The lesson:* before comparing two systems, check that one operation
means the same thing on both sides. If it does not, either make it so
or show the missing column as missing.

**2. A batched API over an unbatched primitive.** Adding a chunked
receive produced real batches — 4000 handshakes became 299, averaging
13.4 elements — and bought 4%. Everything the batch touched got 13x
cheaper; it simply did not touch the expensive part, because the bulk
receive was a loop over the single receive and the buffer still paid
one atomic per element.

*The lesson:* an amortization argument is only as good as the
inventory of what is paid per element. Counting the batch is not the
same as counting the work.

**3. The producer could not run ahead.** Batch size is not set by the
consumer asking for more; it is set by how far the producer gets in
front. Ours could not, because `sendBlocking` ran a parking handshake
even when the buffer had room and there was nothing to wait for.
Trying `offer` first — one line — took the average batch from 35 to
444 and the chunked lane from 175us to 59.

*The lesson:* a cost paid by the producer shows up as a *consumer*
problem, because it starves the consumer of anything to batch.

**4. The guarantee was bought in the wrong place.** Drain-on-close as
an invariant of the mechanism cost 47%; as a mark travelling in the
stream, 2.4%. Same promise, same laws, an order of magnitude apart in
price.

*The lesson:* "this guarantee is expensive" is usually "this
guarantee, bought this way, is expensive".

### On measuring at all

Every number here was taken on a machine shared with other builds, and
that is not a footnote. The same untouched `zio.Queue` lane read
anywhere from 116 to 138 in a single day — a 13% spread on code nobody
edited, against effects of 2-5% that we were trying to see.

Three habits made the numbers usable, and they are the reason to trust
the table above:

- **A control lane.** Include something you did not change. If it
  moves, the run is measuring the host, not the code, and goes in the
  bin. Several did.
- **Both variants in one invocation.** Two runs an hour apart cannot
  settle a 5% difference. Two lanes in the same run can, because they
  see the same machine.
- **Ratios, not absolutes.** Compare each lane to the control within
  its own run. When the array-front buffer and the list-front buffer
  gave the identical ratio 0.925 in two separate runs whose absolute
  numbers differed by 6%, that identity was the finding.

---

## 6. Literature

**Vyukov's bounded MPMC queue** — the stamp scheme `Ring` uses.
Dmitry Vyukov, "Bounded MPMC queue", 1024cores.net. The same algorithm
underlies ZIO's `RingBuffer` (`zio/internal/RingBuffer.scala`), which
is why the two agree on rounding capacity to a power of two and on
treating capacity 1 as a different structure.

**Michael & Scott**, *Simple, Fast, and Practical Non-Blocking and
Blocking Concurrent Queue Algorithms*, PODC 1996 — the linked-list
queue whose shape `Segments` follows at the granularity of segments
rather than nodes.

**Okasaki**, *Purely Functional Data Structures*, CUP 1998 — the
banker's queue behind `ListFifo` and `ArrayFifo`, and the amortization
argument for turning the back round: O(1) amortized, and the reason
the batched path avoids doing it at all.

**Koch, Sanders & Williams**, *BlockFIFO & MultiFIFO: Scalable Relaxed
FIFO Queues*, arXiv:2507.22764 (2025) — the relaxed queues, the rank
error as the measure of how far from true FIFO a design strays, and
the observation that the gains appear at p=32..192 producers. Our
`MultiFifo` is the MultiFIFO idea with producer affinity, which is
what preserves per-producer order. BlockFIFO's held block is not built
here: our `pushMany`/`popMany` already claim a run with one CAS, and a
*held* block means positions claimed and unpublished, which truncates
a concurrent batched scan — measured, that cost more than it saved.

**Herlihy & Shavit**, *The Art of Multiprocessor Programming*, 2nd ed.
2020 — the lock-freedom vocabulary used throughout: every operation
here either completes or answers "full"/"empty" to its caller, and
never waits on another thread's progress.

**Harris, Fraser & Pratt**, *A Practical Multi-Word Compare-and-Swap*,
DISC 2002, and **Shavit & Touitou**, *Software Transactional Memory*,
PODC 1995 — the background for `StmChannel`: one cell, transitions as
pure `State => (State, action)`, and the action deferred until the CAS
has won.

---

## 7. Laws

Every mechanism answers to `TestChannelLaws`, which is parameterised
over implementations and split into two tiers.

**Core** — every channel: order per producer, no duplication, a closed
channel accepts nothing, `fail` records without closing, a bulk take
is the elementwise one batched.

**Drain** — only what claims it: acceptance is final, close does not
discard, a receiver sees the end only once the buffer is drained,
`finished` is false while anything remains, a failure arrives last.

An implementation that does not claim a drain law is *recorded* as not
claiming it, in the gate's own output. `AbruptChannel` refuses five;
that is not five failures, it is a contract stated.

Adding a mechanism means adding a row to the table at the top of the
suite, and answering for everything in the tier you claim.

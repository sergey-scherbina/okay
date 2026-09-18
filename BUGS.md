# Bugs — the core `okay` module (`src/`)

The ledger for defects whose FIX lands in the root module's own sources
(`src/main/scala-jvm`, `scala-js`, `scala-native`, `src/main/scala`).
A module with its own sources keeps its own file beside them —
`okay-http/BUGS.md` and `okay-ui/BUGS.md` are the other two today.
This is not a leftovers bin: an entry whose fix belongs to a module
belongs in that module.

Newest first. Status lives in the machine-readable header, never in
the prose.

## par-right-failure-waits — `Async.par` notices a right-side failure only after the left finishes
<!-- status: fixed
     lane: cross-platform (Async.scala, core)
     area: async
     found-by: TestPar "a failing leaf fails the spine and cancels its siblings"
       (applicative-par, 2026-09-17), then reduced to Async.par alone by
       src/test/scala-jvm/ProbeParFailFast.scala
     fixed-in: par-fail-fast (2026-09-18)
     gate: TestAsync "par sees EITHER side fail, and does not wait out the
       healthy one" — both orders, fails on the old line; and TestPar
       "a failing leaf fails the spine at once, in either order", which
       is the pin that ANNOUNCED the fix by failing when it landed
     repro: measured 2026-09-17
       Async.par(async { Thread.sleep(3000); 1 }, async[Int](throw boom)).runWith
         -> boom after 3.017 s
       Async.par(async[Int](throw boom), async { Thread.sleep(3000); 1 }).runWith
         -> boom after 0.0007 s -->

`Async.par`'s own doc comment says "a child failure fails the pair and
cancels the sibling". It does, on the LEFT. A right-side failure is
not observed until the left side completes, because the two
completions are registered in a nest rather than side by side
(Async.scala, `par`):

    fa.onComplete:
      case Right(x) => fb.onComplete:          // only reached if fa succeeded
        case Right(y) => ...
        case Left(e) => fail(fa)(e)
      case Left(e) => fail(fb)(e)

Nobody is listening to `fb` while `fa` runs. So the pair waits out the
healthy sibling of a leaf that has already failed, and that sibling is
never cancelled — the numbers above are the same failure in the two
orders, 3 seconds against 0.7 milliseconds.

The ANSWER is not wrong, only late: the pair still fails with the
right error, which is why this rode through every existing test. It is
`race` that would be wrong, and `race` does not share this code.

**THE FIX, and it needed no cell.** The shape sketched here was
"register both completions independently and join them in a cell". The
cell turned out to be unnecessary: only the FAILURE watch has to be
independent. `fb.onComplete` is now registered up front for its Left
alone, and the pairing stays nested for the success road, where it
already has both values in hand and costs nothing. Two facts were
checked before relying on them, on all three platforms: a fiber takes
several subscribers (a waiter list on the JVM's DriveTask,
`whenComplete` on a CompletableFuture, `subscribe` on Native's cell, a
Future callback on JS), and a callback registered on an already
finished fiber fires at once. `done` still keeps the first answer, so
the second side's late failure is ignored.

Measured after: the same failure in the two orders now returns in
0.003 s and 0.004 s, and the healthy sibling is cancelled rather than
run to completion.

**What it cost while it was open.** `Par` (the parallel applicative,
specs/applicative-static.md) documented fail-fast as inherited and its
test asserted only the left-side case, pinning the OTHER order with a
message telling whoever fixed this to come back. That is exactly what
happened: landing the fix failed `TestPar` with "par-right-failure-waits
is FIXED — strengthen this assertion and close the BUGS.md entry", and
this entry is closed because a test said to close it. Every
`parAll`/`parTraverse` caller was unaffected throughout: those join in
order and never claimed to cancel anything.

## growing-stale-route — a route taken before the swap names the adopted part, and overtakes its own producer
<!-- status: reopened
     reopened: 2026-09-17, TestGrowing round 36, producer 1: 29, +37, 31, 35, -37, 39
     see: the REOPENED section at the end of this entry -->
<!-- previous status: fixed
     lane: jvm
     area: queues
     found-by: TestGrowing "each producer's own order survives the swap" (reported 2026-09-14, round 40)
     fixed-in: growing-stale-route (2026-09-14)
     gate: src/test/scala-jvm/TestGrowing.scala, "a route taken before the swap
       does not send later elements into the adopted part" — one producer, no
       race, fails on the old line -->

`TestGrowing`'s per-producer order law failed again, and the reported
output says the whole of it: producer 1 came back

    25, 33, 27, 31, 35

`33` ahead of `27` and `31`, from one producer, which is exactly the
shape `merge-chunked-order` was closed on. That fix is sound and is
not the one at fault — this is a second road to the same place.

**The mechanism.** `SentinelChannel.sendAsync` took the route ONCE,
before the first attempt, and carried it through every retry. It must
carry it: a parked send resumes on the waker's thread, so asking again
there would answer with the CONSUMER's part and scatter one producer's
elements. But taken before the swap the answer is `0`, because
`Buffer.route()` is `0` for anything unpartitioned — and after the
swap `0` is the ADOPTED PART, which `popAdoptedFirst` reads before
every part opened after it, on purpose.

So the producer's later element was carried into the one part that is
read first, and overtook its earlier ones sitting in a part of its
own. The rule that fixed `merge-chunked-order` is what carried it
there. The same stale route also parked the sender on the waiter queue
of a part it was not pushing to.

**Why it shows only sometimes.** It needs part 0 to be FULL when the
earlier elements are pushed — so they are refused into a part of their
own — and to have ROOM when a later one is. 1 200 rounds on a quiet
machine did not produce it; the reporter's run did, at round 40.

**The fix, in two places, because the route has two owners.**

- `SentinelChannel.attemptSend` takes the route AFTER reading the
  buffer and only while it is on the producer's own thread
  (`!granted`). A resumed send keeps the route it parked with, which
  was taken correctly before it parked.
- `Growing.pushAt` / `pushDecidingAt` correct a stale route before the
  attempt rather than only after a refusal. Doing it only on the
  refusal path was the hole: a push that SUCCEEDS into part 0 never
  reaches the refusal path. `pushDecidingAtOnBehalf` deliberately does
  NOT correct — it runs on whichever thread freed the slot, and the
  honest answer there is the one its caller already took.

**The gate is deterministic**, with one producer thread and latches
rather than a race, and it was run both ways: it fails on the old line
with `33, 27, 31` and passes on the new one. The round-based law
passed 1 600 rounds after the fix — which is consistent with it and is
not evidence, since it passed 1 200 before.

### REOPENED 2026-09-17 — the fix narrowed the window, it did not close it

`TestGrowing`'s per-producer law failed again on a lane that touches
neither queues nor the JVM platform, three days after this entry was
marked fixed:

    round 36: producer 1 came back out of its own order
       29, +37, 31, 35, -37, 39

**It is the same shape, and it is now measurable.** Each producer
emits an arithmetic sequence, so the hoist can be counted, and the
three recorded sightings agree exactly:

    2026-09-10   49 -> 57   +8   = 4 of its own elements
    2026-09-11    5 -> 13   +8   = 4 of its own elements
    2026-09-17   29 -> 37   +8   = 4 of its own elements

Four is `Channel(4)`'s CAPACITY, which is what the mechanism above
already predicts: the hoisted element reaches the adopted part while
its predecessors sit in a part of their own, and how many predecessors
that is, is how many fit in the part that had to fill first.

**MEASURED 2026-09-17, and it is no longer a hypothesis: the window
is real and every crossing of it is in the harmful direction.**
`attemptSend` was instrumented to compare `buffer.route()` at the push
against the route it read a few lines earlier, over TestGrowing's own
scenario:

    rounds   sends      route changed in between   of those, read as 0
    2000     156 578    12                         12

Twelve out of twelve, and read-as-0 is exactly the condition this
entry was closed on: `Buffer.route()` answers 0 for an unpartitioned
ring, and by the time the push lands, 0 is the ADOPTED part that
`popAdoptedFirst` reads before every part opened after it. So the
element goes in front of its own predecessors — for ONE element,
where before the fix it was for the life of a send. About one crossing
in thirteen thousand sends, which is the right order for a law that
now breaks roughly once in a few thousand rounds. (The instrumentation
was removed before landing; these numbers are what it was for.)

### RETRACTED, the same day: the buffer already repairs this

The paragraph below proposed that the window explains the recurrence,
and the next lane was claimed to close it. **Reading `Growing` before
writing that fix showed the proposal was wrong, and the measurement
above does not mean what it was taken to mean.**

`Growing.pushDecidingAt` does not trust the route it is handed:

    private def ours(b: Buffer[A], route: Int): Int =
      if grown.get then b.route() else route

On a grown buffer the passed route is DISCARDED and the question asked
again on the producer's own thread — which is precisely the repair
`growing-stale-route` added, and its comment says so: "a push that
SUCCEEDS into part 0 never reaches the refusal path" was the hole, and
this closed it. So the twelve crossings measured above are the
PRECONDITION of the old bug, which the buffer then repairs; they are
not the bug.

The one way this could still leak is the reverse ordering — `inner`
swapped while `grown` is still false, so `ours` keeps the stale 0. It
cannot happen: `grow()` sets `grown` by CAS BEFORE it assigns `inner`,
so a reader seeing `grown == false` is seeing a buffer that has not
been replaced yet, and pushing into the ring is correct.

**WHAT THE MEASUREMENT IS STILL WORTH**, which is why it stays: it
rules the obvious candidate OUT. The window is entered about once in
thirteen thousand sends, every crossing is in the direction that
WOULD have been harmful before 2026-09-14, and none of them can reach
the part selection now. Whatever causes the 2026-09-17 recurrence, it
is not a stale route surviving into `pushDecidingAt`.

**So the cause is unknown again**, and the entry stays reopened with
the search narrowed rather than the bug explained. Candidates not yet
examined: the PARKING path (`sendersAt(route)` and the resumed
`pushDecidingAtOnBehalf`, which by design does NOT repair the route);
and the visibility of `inner`, which is a plain `var` written after an
atomic, so a reader may see `grown == true` with the old buffer — safe
for routing, but not obviously safe for everything else.

---

*What follows is the retracted proposal, kept because a lead that was
ruled out is worth more written down than deleted.*

**THE PROPOSED MECHANISM (RETRACTED).** The fix made
`SentinelChannel.attemptSend` take the route per attempt instead of
once per send:

    val route = if granted0 then route0 else buffer.route()     // line 312
    ...
    buffer.pushDecidingAt(route, a, closing, void)              // line 328

That removes the route that was stale for the LIFE of a send. It does
not make reading the route and using it one step: a swap landing
between 312 and 328 gives back exactly the old condition — `route()`
answered 0 from a buffer that was still the unpartitioned ring, and by
the push, 0 is the adopted part — for ONE element. Which is what the
evidence looks like: the same shape, and roughly one sighting in three
days where there were two in two.

**THE PROPOSED FIX (RETRACTED — the buffer already does this).** Stop
reading the route and then pushing: let the buffer decide the route AND push in
one call, so there is nothing in between.

- `AdaptiveFifo.pushDeciding(a, unless, orElse)` already routes
  internally (`mine.get.buf`), so it cannot be overtaken by a swap the
  way a route read earlier can.
- `lastRoute` (`myRoute.get`) already says which part a thread's push
  went to, which is what the parking path needs afterwards —
  `SentinelChannel` line 97 already uses it for exactly this.

So a FRESH send should push without a route and park on
`sendersAt(buffer.lastRoute)`; a RESUMED send keeps `pushDecidingAt`,
because it must carry the route it parked with (a parked send resumes
on the waker's thread, and asking there would answer with the
CONSUMER's part).

NOT DONE HERE, deliberately. This is the channel's hot path, and this
repository prices hot-path changes before it lands them — `Growing`'s
own header carries four benchmark tables. The measurement above is
what the fix needed to be justified; the fix needs a bench run of its
own, and that is the next lane rather than a rushed edit at the end of
this one.

**NOT tagged, NOT retried into green, and NOT closed.** The entry
stays reopened until the fix above lands with its numbers.

## universal-apply-blocks-named-tuples — `import okay.*` makes a named tuple's field access a type error
<!-- status: fixed
     lane: all
     area: generate
     found-by: named-tuples-stage0 (2026-09-12)
     fixed-in: named-tuple-unblock (2026-09-12)
     gate: src/test/scala/TestNamedTuples.scala, and GtfsNamed.scala
       compiling under `import okay.*` on the real feed -->

`src/main/scala/Generate.scala:25` defines

```scala
extension [A](a: A) inline def apply[R](f: A Loop R): R = loop(f)(a)
```

an `apply` on EVERY type, so that a loop can be run as `seed(body)`.
A named tuple's field access desugars to an apply by INDEX, and this
extension answers that call first: with `import okay.*` in scope,

```scala
type Trip = (route: String, service: String)
val t: Trip = (route = "a", service = "b")
t.route      // Found: (0 : Int)   Required: (route: String, service: String) Loop String
```

Reproduced standalone in four lines, with a stand-in extension of the
same shape, so it is the SHAPE and not anything else in the package:
with the extension in scope the selection fails, without it compiles.
`import okay.given` alone is fine — it is the wildcard that carries it.

WHAT IT COSTS: named tuples, a stable Scala 3 feature, cannot be used
by anyone who writes `import okay.*`, which is what every example in
our own docs writes. Found while measuring whether a named row would
suit the Wrocław job; the measurement had to import okay's names one
by one to proceed, and that workaround is in GtfsNamed.scala's header.

WHAT IT WOULD COST TO FIX, priced rather than chosen — the DSL this
extension serves (`Loop`, `take`, `loop`) has NO other use in this
repository: no test, no doc example, one mention in typepedia's type
list. So:
  - move the extension into an object users import explicitly
    (`import okay.Loops.*`), leaving `loop(f)(a)` where it is. The
    wildcard stops being greedy and nothing else changes.
  - or drop the `seed(body)` spelling entirely: `loop(body)(seed)`
    already exists and is what the extension calls.
Both are one-line changes with zero call sites to update. The choice
is the operator's, because either removes a spelling from the public
API.

FIXED 2026-09-12 (named-tuple-unblock) by a THIRD way, which removes
nothing:

```scala
extension [A](a: A)(using scala.util.NotGiven[A <:< NamedTuple.AnyNamedTuple])
  inline def apply[R](f: A Loop R): R = loop(f)(a)
```

The extension declines to apply to a named tuple, so the field access
falls through to the compiler's own selection, and `seed(body)` keeps
working for every other seed. Guarding on `Tuple` instead does NOT
work and that is the refuted step worth keeping: a named tuple is not
`<:<` a `Tuple`, so `NotGiven` succeeds and the extension captures the
selection anyway. `NamedTuple.AnyNamedTuple` is the exact upper bound.

Pinned from both sides in `src/test/scala/TestNamedTuples.scala` — the
fields are reachable under `import okay.*`, a named tuple still IS the
plain tuple at runtime and `==` to it, the `seed(body)` spelling still
resolves, and a plain tuple still indexes. And on real code:
`GtfsNamed.scala` went back to the plain wildcard import, so if the
guard ever regresses that file stops compiling and the 4 593 288-row
equality test goes with it.

## own-long-join-deadlock — 10k fibers joined inside a fiber on `Schedulers.Owned.forLongTasks` never completes
<!-- status: fixed
     lane: jvm
     area: scheduler
     gate: src/test/scala-jvm/TestSchedulerLaws.scala "work-stealing deque — nothing is lost while it grows under thieves"
     fixed-in: 3f09bd9c -->

`compare/src/jmh/scala/okay/AdversarialBenchmark.scala:124`,
`forkJoin10k_okayOwnLongInside` at `work = 100`, sat **62 minutes at
0.0% CPU** in JMH warmup iteration 1 and had to be killed by pid. It
is not slowness: the joining thread is parked and every worker is
parked, so nothing is left to wake anyone. (This paragraph first said
"it is a lost wakeup". It is not — see ANSWERED below, where a timed
park left the hang in place. The wrong reading is kept visible rather
than quietly deleted, because it is the one a reader arrives with.)

Found 2026-09-07 by `bench-refresh` at `dfde7ce3` (master + a claim
commit; no library code differs from master), JDK 21.0.7-tem, on a
quiet freshly-booted box with both scalascript launchd guards
unloaded. NOT a 143 from the guards, and not the box being busy — the
`exit 143` in that run's log is my own `kill`.

`jcmd <fork> Thread.dump_to_file -format=json`, 50 threads:

```
tid 22  ...forkJoin10k_okayOwnLongInside-jmh-worker-1
  Unsafe.park
  okay.Platform$package$$anon$6.block(Platform.scala:87)
  okay.Fiber.joinEither(Async.scala:128)
  okay.Schedulers$DriveTask.joinEither(Platform.scala:618)
  okay.Fiber.join(Async.scala:124)
  okay.AdversarialBenchmark.forkJoin10k_okayOwnLongInside(AdversarialBenchmark.scala:124)

42 threads  okay.Schedulers$Owned$Worker.run(Platform.scala:542)  — all parked
```

Repro shape: with `given Scheduler = Schedulers.owned(...).forLongTasks`,
`Async.spawn { 10 000 inner spawns, folded with joinAsync }.join()`.
The two neighbouring lanes with the SAME body and a different
scheduler policy — `forkJoin10k_okayOwnShortInside` (`forShortTasks`)
and `forkJoin10k_okayOwnInside` (the self-deciding default) — have
both measured fine for weeks, which points at the spread-at-once
policy rather than at the fold.

Platform.scala:542 is the park whose own comment names this race:
"publish `parked` and drop out of `awake` BEFORE the last look at the
queues: a submission that misses the flag has landed in a queue we are
about to see, one that sees it will unpark us." A worker that parks
between the queue check and the flag publish is the shape to look at
first — but that is a reading of the comment, not a diagnosis, and
nothing here has bisected it.

**ANSWERED, 2026-09-07 (fixed in 0815cbe8).**

Cause: `Deque.steal` cleared its slot after winning the `top` CAS. A
thief reads `top`, `bottom` and `buf` at three separate moments, so it
can be reading through an array the owner has already replaced in
`push`'s grow. Clearing writes a null into an array another thief is
still reading; that thief's CAS then SUCCEEDS while its `a.get(i)`
came back null, so the index is consumed and the task in it is never
run. One lost `DriveTask` is one fiber that never answers — hence a
`join` parked for ever with every worker legitimately idle. Canonical
Chase-Lev leaves the slot for exactly this reason.

The prediction in the claim — a lost wakeup in the park at
Platform.scala:542 — was WRONG, and the experiment that killed it is
worth keeping: replacing the worker's park with a 1 ms timed park left
the hang exactly in place, at 30% CPU instead of 0%. Workers were
waking, looking and finding nothing. Nobody had failed to signal; the
work was gone.

What it took to see it, in order, because three of these produced
nothing and that is the useful part:

| attempt | result |
|---|---|
| the shape alone, fresh scheduler per run, 200 runs | 0 |
| scheduler reused, all three pools alive (as `@Setup(Level.Trial)`), 2000 runs | 0 |
| cold JVM, 3 ops each, 250 JVM starts | 0 |
| the real JMH lane, 12 sequential runs | 0 |
| the real JMH lane, **4 in parallel** | hung on rounds 1, 4, 5, 6 |
| timed park instead of blocking park | still hangs, 30% CPU — not a lost wakeup |
| pool counters at the terminal state | `forked=40004 ran=40003`, 0 stranded, **1 steal that won its CAS on a null slot** |
| the fix alone, real blocking park | 120 forks clean |

Contention is the trigger, and only this lane's shape opens the
window: 10 000 `pushLocal` into ONE deque from capacity 256 is six
grows while thirteen thieves read through it.

Answers to the questions this entry opened with:
- Does it reproduce? Yes, but only under contention — about once in
  twenty thousand fork/join operations with four JVMs competing, which
  is why 12 sequential runs said nothing.
- Does `work = 10000` hang too? Still unknown and now moot; the defect
  was not work-size dependent.
- Is `joinAsync` inside a spawned fiber required? No. The requirement
  is a deque that GROWS while thieves read it, which that shape
  produces and the flat one does not.

Gate: the hang itself is a soak, not a gate, so the law is stated on
the deque instead — conservation, everything pushed comes out exactly
once, on the growing-under-thieves shape. It fails on the old line in
29 ms and passes on the new one.

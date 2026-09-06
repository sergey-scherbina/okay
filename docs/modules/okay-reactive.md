# okay-reactive — Reactive Streams interop

`Source[A]` ↔ `java.util.concurrent.Flow.Publisher[A]`, so this
library's streams meet Akka/Pekko, RxJava, Reactor, fs2 and ZIO on the
SPI they all speak.

```scala
import okay.*, okay.given, okay.reactive.Reactive
given Scheduler = Schedulers.loom

val p: Flow.Publisher[Long] = Reactive.publisher(Source.range(0, 1000))
val s: Source[Long]         = Reactive.source(p, capacity = 256)
val broken: Flow.Publisher[Long] = Reactive.failed(RuntimeException("nope"))
```

**No dependency.** `java.util.concurrent.Flow` has been in the JDK
since 9, so the published artifact adds nothing to your classpath. The
TCK is a test dependency only.

**JVM only.** `Flow` exists on neither Scala.js nor Native, which is
why this is a module rather than part of the core.

---

## Why it fits

The Reactive Streams protocol is DEMAND: a subscriber says
`request(n)` and the publisher may not deliver more than has been
asked for. That is what a bounded channel already is, and
`receiveMany(n)` already means "no more than n" — so the bridge turns
demand into reads rather than inventing backpressure.

Both directions carry the library's own promises across:

- **A publisher is COLD.** Every subscriber gets its own run of the
  source, because a `Source` is a program and running it twice does
  the work twice. That is the re-observation contract, not a choice
  made here.
- **A failure is the END.** `onError` reaches the reader after
  everything already delivered, which is exactly what `Channel.fail`
  promises.

---

## What the specification demands

The rules below are from Reactive Streams 1.0.4, and each is a line of
code rather than a hope. The TCK checks all of them:

| rule | what it says | how it is met |
|---|---|---|
| 1.1 | never deliver more than requested | demand is decremented BEFORE `onNext`, so a concurrent request can only raise the ceiling |
| 1.3 | signals are serial, never concurrent | one pump fiber emits; `request` only adds demand and wakes it |
| 1.6, 1.7 | nothing follows a terminal signal | one `AtomicBoolean` gates `onError`/`onComplete`, forever |
| 1.9 | `onSubscribe` comes first, even for a failing publisher | it is the first statement of `subscribe` |
| 2.5 | a second `onSubscribe` is cancelled, not used | on the `source` side, a CAS decides which subscription is real |
| 2.13 | a null element is an NPE, not a signal | checked before `onNext` |
| 3.6 | `cancel` after termination does nothing | idempotent by CAS |
| 3.9 | `request(n)` with n ≤ 0 is an `onError` | checked, with the rule number in the message |
| 3.17 | demand accumulates to 2^63−1 and saturates | `updateAndGet` clamps rather than wrapping |

**The TCK is not optional, and this module is the argument for that.**
Thirty-nine checks run against the publisher, and three things came
out of them that reading the prose did not:

**A fixture that failed at the wrong moment.** The first
"failed publisher" was `Source.of(lazyList.map(_ => throw))`, and the
exception escaped `subscribe` itself — because `Source.of` forces its
head. The TCK correctly reported the bridge as throwing, when it was
the fixture.

**A failure nobody would ever hear.** The second was a source failing
on the PULL — but the pump only pulls once something is requested, so
a subscriber that requests nothing is never told. The TCK timed out
waiting for `onError`.

**What "failed publisher" actually means.** Terminal signals are not
limited by demand: a subscriber learns a stream is broken without
having asked for anything. That is `Reactive.failed`, and it exists
because the TCK insisted, not because the design anticipated it.

---

## Demand follows CONSUMPTION

On the `Publisher → Source` side, the tempting implementation asks for
one more element inside `onNext`. It looks equivalent and it is not:
outstanding demand then stays at the full window while elements pile
up in the buffer, so a slow reader overflows it and `offer` starts
dropping — **elements lost silently**, with no exception and no hang,
which is the worst way to be wrong.

A request goes out when an element has been TAKEN. Then demand plus
buffered never exceeds the window, and a slow consumer slows the
publisher, which is what backpressure means on both sides of the
bridge.

And in BATCHES, refilling half a window at a time. Requesting per
element puts the subscriber back into the publisher on every element,
and a publisher that emits inside `request` — the spec allows exactly
that, asking only that the recursion be bounded (3.3) — then recurses
as deep as the stream is long. Our own first test publisher did, and
overflowed the stack.

---

## Testing

```
sbt okayReactive/test
```

45+ tests: the full TCK, plus checks that the bridge means what a
caller expects — order, empty streams, demand respected before any
element arrives, cancellation stopping delivery, a failure arriving
after what was already delivered, a round trip preserving elements,
and each subscriber getting its own run.

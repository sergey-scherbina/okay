# okay-actor — the mailbox you already have

An actor is a mailbox, a loop that reads it one message at a time,
private state that needs no locks BECAUSE of that, and an address
others can send to.

In this library that is a `Channel`, a fiber, a value threaded through
a loop, and the channel itself. So this module is deliberately small:
it names what composition already gives, and adds the one thing it
does not — **supervision**.

```scala
import okay.*, okay.given, okay.actor.*
given Scheduler = Schedulers.loom

val counter = Actor.spawn(0) { (n: Int, m: Int) => async(n + m) }.runWith
counter.tell(1)
counter.stop()
```

Cross-built for JVM, Scala.js and Native — because everything it
stands on already is.

---

## What is not invented here

| actor concept | what it already is |
|---|---|
| mailbox | `Channel[M]` |
| one message at a time | one consumer on that channel |
| state needs no synchronisation | a consequence of the above |
| address | the channel; `okay-cluster`'s remote channel for a distant one |
| spawn | `Scheduler.fork` |
| stop | `Channel.close()` |
| backpressure | the channel's capacity |

**The last row is a difference, not a similarity.** Akka's default
mailbox is unbounded; a bounded one drops or blocks by configuration.
Here backpressure is the ordinary case, and the caller picks the
contract at construction from the same menu as any channel:

```scala
// a mailbox that makes a flooding sender wait
Actor.spawn(0, Queues.strong[Msg].bounded(1024).build, Supervise.Stop)(behaviour)

// one that never blocks a sender, bounded only by memory
Actor.spawn(0, Queues.strong[Msg].unbounded.build, Supervise.Stop)(behaviour)

// many senders, and their mutual order does not matter
Actor.spawn(0, Queues.strong[Msg].adaptive.each(256).build, Supervise.Stop)(behaviour)
```

---

## Supervision — the part that is new

```scala
enum Supervise[+S]:
  case Stop                              // close the mailbox; the failure ends the stream
  case Resume                            // keep the state, drop the message
  case Restart(fresh: () => S)           // new state, drop the message
  case Escalate(to: Throwable => Unit)   // report it, then stop
```

Four decisions, each with the alternative it rejects:

**A message that threw is DROPPED, never retried.** Redelivery is how
a system loops for ever on one poisonous message — and the loop hides,
because every attempt looks like a fresh failure. A caller who knows a
message is retryable asks for retry where they know it.

**`Restart` resets, `Resume` keeps, and that is why both exist.** A
behaviour that threw may have left its state half-updated, and only
its author knows whether that state still means anything. Defaulting
to `Restart` would silently discard work; defaulting to `Resume` would
silently keep corruption.

**The default is `Stop`.** An actor that fails and quietly carries on
is how a system goes wrong without saying so. Supervision is opt-in
because choosing it means having thought about it.

**`Restart` calls `fresh()` again** rather than reusing the initial
value, so a restart cannot restart into the wreckage of the state it
was given the first time.

---

## Ask

```scala
enum Msg:
  case Get(reply: Reply[Int])

val n: Option[Int] = actor.ask(Msg.Get.apply, within = 5000).runWith
```

**`within` has no default, and that is the point:** an ask that can
wait for ever is a deadlock with good manners. The caller knows how
long an answer is worth waiting for; this module does not.

The reply travels in a one-shot channel, so the correlation IS the
box — no table of pending requests keyed by id, nothing to leak,
nothing to clean up, and a reply that arrives after the timeout simply
lands in a box nobody holds. A behaviour that replies twice has a bug
in the behaviour; the second answer is dropped rather than corrupting
anything.

---

## Children

```scala
val child = parent.spawnChild(0)(behaviour).runWith
parent.stop()    // stops child first, and waits for it to DRAIN
```

Stopping goes leaves-inward: a child must not outlive its parent's
mailbox, and a parent's stop completes only after its children have
closed *and drained* — because the strong contract says the accepted
messages are still coming.

There is no automatic parent restart. Restarting a subtree means
deciding what happens to the messages in flight in every mailbox under
it, and that belongs to whoever built the tree rather than to a
default here.

---

## What is deliberately absent

**Untyped messages.** `ActorRef[M]` is typed; a protocol with several
shapes is a sum type. Untyped mailboxes make supervision worse, not
better: a handler that cannot name what it failed on cannot decide
what to do about it.

**Hidden mutable state.** The state is a value threaded through the
behaviour — which is what makes `Restart` expressible as `fresh()`
rather than as re-running a constructor with side effects nobody
listed.

**An actor system, a configuration, a dispatcher.** The scheduler is
already a typeclass door.

**Location transparency, here.** `okay-cluster` already has it: an
`ActorRef` over a remote channel is remote, and nothing in this module
needs to know.

---

## Laws

`specs/actor.md` states eight, and the module lands stage by stage
with them or does not land:

1. one message at a time — behaviours never overlap
2. order per sender — inherited from the channel
3. stop drains — every accepted message is handled
4. a stopped actor answers `false`, for ever, and never throws
5. `Resume` keeps the state, `Restart` replaces it; both drop the message
6. `Escalate` reports exactly once and stops the child
7. an ask answers or times out — never both, never neither
8. children die first, and draining

```
sbt okayActorJVM/test
```

## Numbers (2026-09-06, `ActorReactiveBenchmark`, docs/benchmarks.md §17)

| what | cost |
|---|---|
| `tell`, 4000 messages as one program, then an `ask` | 295.9 us — **1.49x** the same messages through a bare `Channel.buffer(256).drained` |
| `ask` round trip | **~11 us and 4.4 KB** each (§17d; was 13.0 / 5.5 KB) — a `Reply` is a channel of two plus a timer armed per call; the timer is now a task on one scheduled executor, not a thread |
| `spawn` + `stop` | 1.04 us and 4.3 KB |

The 1.49x is the receive side: the loop reads one message at a time
(so supervision knows which one failed), and each `receiveBlocking()`
was five allocations — the mirror of the send-side handshake the feed
no longer pays. Since `actor-receive-fused` (§17c) it is two: the
`Handoff` that is its own callback, and the `Some` — bytes per
message −5% with an empty mailbox and −13% with a full one, time at
parity in both. What remains of the ratio is the loop's own
`runWith` per message and the mailbox's ring. Give a `Behavior` an
`AnyRef` state: a primitive one boxes on every step (141 of ~900
allocation samples on this lane).

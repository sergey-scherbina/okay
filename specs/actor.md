# Actors — a mailbox you already have, and the one thing you do not

## Overview

Most of the actor model is already assembled in this repository under
other names. An actor is a mailbox, a loop that reads it one message
at a time, private state that needs no locks BECAUSE of that, and an
address others can send to. Here that is: a `Channel[M]`, a fiber
draining it, a value threaded through the loop, and the channel
itself — with `okay-cluster` already making a remote channel
indistinguishable from a local one, which is location transparency.

So this spec is deliberately small. It names what composition gives
and specifies only what it does not: **supervision**, **ask**, and
**stopping a subtree**.

The operator's brief (2026-09-06): "actors, but in this library's
style — not a framework". The style is stated in `okay-cluster`:
*nothing new is invented here — that is the design.*

## What is NOT invented

| actor concept | what it already is |
|---|---|
| mailbox | `Channel[M]` — with backpressure and a choice of contract, which most actor runtimes do not offer |
| one message at a time | one consumer on that channel |
| state needs no synchronisation | a consequence of the above, not a separate mechanism |
| address | the channel; `okay-cluster`'s remote channel for a distant one |
| spawn | `Scheduler.fork` |
| stop | `Channel.close()` — and the strong contract means what was already accepted is still delivered |
| backpressure | the channel's capacity; a full mailbox slows the sender, rather than growing without bound or dropping |

That last row is worth stating as a difference rather than a
similarity. Akka's default mailbox is unbounded; a bounded one drops
or blocks by configuration. Here backpressure is the ordinary case and
the caller chooses the contract at construction, from the same menu as
any channel.

## The interface

```scala
/** what an actor does with a message: the next state, as a program */
type Behavior[S, M] = (S, M) => S ! Async

final case class ActorRef[M](private val mailbox: Channel[M]):
  def tell(m: M): Boolean ! Async        // fire and forget; false if stopped
  def ask[R](f: Deferred[R] => M, within: Long): R ! Async
  def stop(): Unit ! Async               // close the mailbox; drains first
  def stopped: Boolean

object Actor:
  def spawn[S, M](init: S)(b: Behavior[S, M])
                 (using Scheduler, CanBlock): ActorRef[M] ! Async

  def spawn[S, M](init: S, mailbox: Channel[M], supervise: Supervise[S])
                 (b: Behavior[S, M])(using Scheduler, CanBlock): ActorRef[M] ! Async

/** what to do when a behaviour throws */
enum Supervise[S]:
  case Stop                              // close the mailbox, fail the stream
  case Resume                            // keep the old state, drop the message
  case Restart(fresh: () => S)           // new state, drop the message
  case Escalate(to: Throwable => Unit)   // hand it to a parent and stop
```

## Decisions this spec makes

These are decisions, not details, and each has a defensible
alternative that is NOT taken:

**1. The message being handled when a behaviour throws is DROPPED,
never retried.** The alternative — redeliver it after a restart — is
how an actor system loops forever on one poisonous message, and the
loop is invisible because each attempt looks like a fresh failure.
A caller who wants retry can ask for it explicitly at the point where
they know the message is retryable.

**2. `Restart` resets the state, `Resume` keeps it.** The distinction
is the whole reason both exist: a behaviour that threw may have left
its state half-updated, and only the author knows whether that state
is still meaningful. Defaulting to `Restart` would silently discard
work; defaulting to `Resume` would silently keep corruption. The
default is `Stop`, which does neither.

**3. The default is `Stop`.** An actor that fails and quietly carries
on is how a system goes wrong without saying so. Supervision is opt-in
because choosing it means having thought about it.

**4. `stop()` drains.** Closing the mailbox ends it only after what
was already accepted has been handled — the channel's strong contract,
carried up. A caller who wants the abrupt behaviour builds the actor
on a weak channel, and the difference is visible at construction.

**5. `ask` needs a timeout, and has no default.** An ask that can wait
for ever is a deadlock with good manners. The caller states the bound.

**6. A stopped actor's `tell` answers `false` rather than throwing.**
A producer that outlives its actor is ordinary, not exceptional —
the same reading `Channel.send` already takes.

## Supervision and the children tree

An actor may spawn actors. Stopping a parent stops its children first,
then itself, so a subtree stops leaves-inward and no child outlives
its parent's mailbox.

`Escalate` hands the throwable to the parent's supervisor and stops
the child. There is no automatic parent restart: restarting a subtree
means deciding what happens to messages in flight in every mailbox
under it, and that decision belongs to whoever built the tree.

## What is deliberately NOT here

**No untyped `Any` messages.** `ActorRef[M]` is typed, and a protocol
that needs several shapes uses a sum type. Untyped mailboxes make
supervision worse, not better: a handler that cannot name what it
failed on cannot decide what to do about it.

**No hidden mutable state.** The state is a value threaded through the
behaviour. This is not purity for its own sake — it is what makes
`Restart` expressible as `fresh()` rather than as a re-run of a
constructor with side effects nobody listed.

**No actor system, no configuration, no dispatcher.** The scheduler is
already a typeclass door; a caller who wants a different one provides
it.

**No location transparency INSIDE this module.** `okay-cluster`
already has it: an `ActorRef` over a remote channel is remote, and
nothing here needs to know.

## Laws

1. **one at a time** — for one actor, behaviours never overlap;
   observable as: state updates are not lost under concurrent tells.
2. **order per sender** — messages from one sender arrive in the order
   sent (the channel's law, inherited).
3. **stop drains** — with the strong contract, every accepted message
   is handled before the actor ends.
4. **a stopped actor accepts nothing** — `tell` answers false, for
   ever, and never throws.
5. **Resume keeps, Restart replaces** — after a throw, the next
   message sees the old state or `fresh()` respectively, and in both
   cases the failed message is gone.
6. **Escalate stops the child** — and the parent's supervisor sees the
   throwable exactly once.
7. **ask answers or times out** — never both, never neither.
8. **children die first** — a parent's stop completes only after its
   children's mailboxes are closed and drained.

## Staging

- **Stage 0** — `spawn`, `tell`, `stop`, the behaviour loop, laws 1-4.
- **Stage 1** — `Supervise`, laws 5-6.
- **Stage 2** — `ask` with `Deferred`, law 7.
- **Stage 3** — children and subtree stop, law 8.

Each stage lands with its laws or does not land.

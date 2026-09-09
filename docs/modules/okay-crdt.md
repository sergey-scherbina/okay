# okay-crdt

> Convergent replicated data types: state that merges without a
> coordinator, and the three laws that make that true shipped as a
> check anyone can run.

Depends on: `okay` (core), for `Hlc` and `Uid`. Pure Scala —
cross-built for JVM, JS and Native; the suite runs on all three.

## Guide

**The laws are the content.** A `Crdt[A]` is one method, `merge`, and
three obligations:

| law | what it forgives |
|---|---|
| commutative — `merge(x, y) == merge(y, x)` | messages arriving out of order |
| associative — `merge(merge(x, y), z) == merge(x, merge(y, z))` | messages arriving in different groupings |
| idempotent — `merge(x, x) == x` | the same message arriving twice |

Those three are exactly what a network does to messages, which is why
they are the price of replicas that agree without anyone coordinating
them. A type that breaks one is not "mostly a CRDT" — it is a type
that silently disagrees with itself under load.

So the check ships in the library, not in its tests:

```scala
import okay.crdt.*

val bad = Crdt.violations(Seq(sampleA, sampleB, sampleC))
assert(bad.isEmpty, bad.mkString("\n"))
```

It answers a **list** of what broke rather than throwing at the first
failure, because a merge is usually wrong in one law and right in the
others, and knowing which one is the diagnosis. Every instance below
runs it.

## What is here

```scala
GCounter                 // grow-only, a count per node, merged by max
PNCounter                // up and down, as two grow-only counters
GSet[A]                  // add-only, merged by union
OrSet[A]                 // observed-remove: add wins, tags are Uids
LwwRegister[A]           // last write wins, stamped with Hlc
NodeId                   // who is speaking
```

**`GCounter` is a map, not a number,** and that is the whole lesson of
the module. One number merged by addition is commutative and
associative but *not* idempotent, so a redelivered increment counts
twice and two replicas that saw the same updates disagree. Merging a
count *per node* by `max` is idempotent, and each node writes only its
own entry.

**`PNCounter` is two grow-only counters** rather than one that can go
down: "larger" stops meaning "later" the moment a decrement exists.
The cost is honest — it never shrinks, and a key incremented and
decremented back to zero keeps both counts for ever.

**`LwwRegister` uses `Hlc`, not the wall clock.** A write that
*causally* followed another wins even if the writer's machine is an
hour behind, because the writer called `observe` on what it saw. Two
writes that never saw each other are concurrent and no clock can order
them; the node id decides — arbitrary, but the *same* arbitrary choice
on every replica, since a tie broken differently is a permanent
disagreement rather than a tie.

Its precondition is load-bearing: `(at, by)` must identify a write
uniquely, which holds when each node stamps from its own `Hlc.Clock`,
because a clock never issues one stamp twice.

**`OrSet` tags every add with a `Uid`.** "Removed" is a property of an
*addition*, not of an element — which is why `add`, `remove`, `add`
brings the element back, and why an add made concurrently with a
removal survives it: the remover tombstoned only the tags it had
observed.

## What is not here

- Delta-CRDTs. These are state-based: `merge` takes two whole values.
  Deltas are an optimisation with the same laws, additive later.
- Garbage collection of tombstones, which needs causal stability —
  knowing every replica has seen a value.
- Byzantine settings: `merge` trusts its inputs. A replica that lies
  is an authentication problem, not a convergence one.

See specs/coordination-free.md for the arc this belongs to.

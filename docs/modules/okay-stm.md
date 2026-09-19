# okay-stm

Software transactional memory: the `Tx` language, the `Stm` runtimes —
TL2 with versions and CAS-owned commit, a direct one, a simulated one
— and the platform givens that install `Stm[Async]`.

The spec is [stm.md](../../specs/stm.md).

## The cell stayed in the core

`TRef` is in `okay`, not here, and that is the shape of the whole
module. `TRef.modify` is the one-cell transaction: a CAS loop that
retries a pure function until it installs, waking watchers and never
parking. It needs no `Tx` and no runtime.

What is here is the machinery that commits SEVERAL cells together.

That division is not a preference. When the split was made, the
compiler was asked which modules actually needed the runtime:

| | modules that needed it |
|---|---|
| `TRef`, the cell | 6 (okay-http, okay-ops, okay-resilience, okay-sql, okay-stream, okay-ui) |
| the transactional runtime | 1, and only in a test |

The one is okay-stream's `TestStmChannel`, which reads a channel's
cell inside a transaction to show that it can be done. Every other use
in the repository is a single cell.

## Using it

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayStm.jvm)
```

If all you want is an atomic cell, you do not need this module:

```scala
val cell = TRef(State())          // okay, the core
val out = cell.modify(s => (s.next, s.answer))
```

Reach for `okay-stm` when two or more cells must move together, or
when you want `Tx.retry` and `Tx.orElse`.

## A note on names

Three things in this library look transactional and are not. `TMap`
and `TDict` are heterogeneous maps with TYPED keys. `Refs` is run-time
state cells. `okay.sql.Tx` is a typestate marker for database
transactions. None of them has anything to do with this module.

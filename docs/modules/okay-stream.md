# okay-stream

Streams, channels, chunked collections and the buffers under them.

Until 2026-09-18 all of this was in the core. It moved out because
the dependency graph said it could: the whole control layer — `Cont`,
`Free`, `Effects`, `Monad`, `Delim`, `State`, `Direct`, `Par`,
`Resource`, `Logic`, `Throws`, `Validated`, `Static` — never named a
channel, a source or a chunk in code. Every apparent reference was a
comment. See [specs/core-modules.md](../../specs/core-modules.md) for
the measurement and the rule it produced.

## Nothing changed in your imports

The package is still `okay`. `import okay.*` and `import okay.given`
reach across the artifact boundary in both directions, so code that
used `Channel`, `Source`, `Chunks`, `Queues` or `Pipe` needs no edit.
What changed is the build: a module that uses any of them declares
`okay-stream` rather than getting it for free from the core.

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayStream.jvm)
```

## What is here

| area | the types |
|---|---|
| channels | `Channel`, `SentinelChannel`, `AbruptChannel`, `StmChannel` |
| buffers | `Ring`, `Growing`, `Fifo`, `AdaptiveFifo`, `Segments`, `ChunkBuf`, `Buffer`, and `Queues`, the builder that chooses among them |
| sources and pipes | `Source`, `Pipe` and its `Stage`, `Pipeline`, `Lines` |
| chunked collections | `Chunks`, `Bulk`, `Tables`, `Windows` |
| parallelism over chunks | `parMap`, `retryChunks` |

## A parallel `Bulk` in one process

`localBulk` is sequential: the reference, the same on JVM, JS and
Native. On the platforms with threads, `parallelBulk(n)` (or
`BulkParallel(n, lines, bytes)`) is the same `Bulk[Chunks]` with the
three operations that parallelise for real spread over `n` fibres: a
file's splits read ahead in split order (a Parquet file's row groups),
`aggregate` folded in batches of chunks and merged in input order — so
an associative, non-commutative `Sequential` stays right — and `join`
with the right side folded in parallel and the left streamed through a
window of fibres, in the left side's order. `map`, `filter` and the
CSV reader stay the local ones: a CSV's bottleneck is its one line
iterator. A `Tables` program runs on it unchanged. JS has no such
instance — one thread — rather than a parameter that does nothing
there. For partitions, a key exchange and many processes, see
`FlowBulk` (okay-cluster).

```scala
val B = BulkParallel(4)
val p = Tables.of(Vector.range(0, 1000)).select(i => (i % 7, i)).join(Tables.of(Vector.tabulate(7)(k => (k, s"k$k"))))
  .collect.map(_.elements.toVector.sorted)
assertEquals(Tables.run(B)(p), Tables.run(local)(p))
```

## What stayed in the core, and why

Two things, both interfaces rather than machinery:

- **`Stream`** — the typeclass whose whole interface is `uncons`,
  with its `LazyList` and `List` instances. `Writer` implements it,
  so it cannot leave.
- **`Handoff`** — the rendezvous `Async.handoff()` answers.

Also `type Chunk[+A]`, which is an alias for `ArraySeq` and is what
`Producer.concat` is typed on. The chunked machinery that fills one
is here; the alias is not machinery.

## The buffer choice

Unchanged by the move, and documented where it always was:
[queues.md](../queues.md) carries the choice table and the ordering
guarantees, including which buffers keep per-producer FIFO and which
ask you to opt in.

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

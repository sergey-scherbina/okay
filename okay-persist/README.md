# okay-persist — the durable log

A named, partitioned, append-only log of records, with offsets as
resume tokens. It turned out to be the ONE persistence primitive
behind three separate things: the `Durable` journal, event-sourced UI
sessions, and resumable SSE — each of which had been about to grow its
own.

Bytes in the engine, `Schema` at the edge: the store keeps
`Record(offset, timestamp, key, value)` with key and value as bytes,
and what they mean belongs to the consumer. Traits rather than an
effect row — a log is infrastructure a handler owns.

## The pieces

| | |
|---|---|
| `Record` / `Topic` | the log: append with an ack, read from an offset, in partitions |
| `FileStore` | the JVM engine: segments on disk, opened at a root |
| `Doctor` | reading a store back when something has gone wrong with it |
| the typed view | `Schema` at the edge, so a consumer sees values rather than bytes |

## Where it is already load-bearing

| | |
|---|---|
| [`okay-agent/`](../okay-agent) | `Durable.tools(inner, journal)` — a tool call that survives a restart |
| [`okay-ui/`](../okay-ui) | a session as events, recovered by a refold |
| [`okay-http/`](../okay-http) | `McpHttp`'s resumable SSE: a dropped stream loses nothing, because the pushes went into a topic first |

## Further

| | |
|---|---|
| [`docs/modules/okay-persist.md`](../docs/modules/okay-persist.md) | the guide, and the distributed extent it was designed to |
| [`specs/persist.md`](../specs/persist.md) | the design, in stages, each one a working system |

# okay-live

> Broadcast and per-key channels over the core's own `Channel`
> (specs/live.md): `Hub[A]` (subscribe/publish to everyone) and
> `Registry[K, A]` (one channel per key, created on first use).
> Extracted 2026-09-02 from `okay-demo`, where the identical pattern
> had already been written twice by hand — `marketFeed` (a broadcast
> ping to every `/market` viewer) and `inboxes` (a per-email channel).

Depends on: `okay` (`Channel` is already cross-platform core; only
the bookkeeping around MANY channels needs `java.util.concurrent`, so
this module is JVM-only — the same tradeoff `okay-subscription`
already made) and, since `Watched`, `okay-codec` (a document is Json
addressed by the keys `JsonOptic.path` resolves against a Schema). No
dependency on any wire, page, or domain.

## Guide

**Broadcast.** `Hub[A]` — `subscribe()` mints a fresh `Channel[A]` and
remembers it; `publish(a)` offers `a` to every channel remembered SO
FAR. A subscriber added after an earlier publish never sees that
publish. A closed or abandoned subscriber's channel stays remembered
until process end — stated, not hidden; human-scale viewer counts
need no eviction.

**Per-key channels.** `Registry[K, A]` — `apply(key)` creates a
`Channel[A]` lazily on first use and answers the same one on every
later call for that key. No removal, the same honest limit as `Hub`;
a real eviction need is a BACKLOG item, not a speculative build.

**Subscribe to a lens.** `Watched[A](json)` is a document many
viewers watch through PATHS: `subscribe("customer.address")` answers
a channel that is told the address — and only the address — each
time it changes; `set("customer.address.city", v)` is a client write
through the same lens; `subscribe("")` is the whole document. The
dotted key is the wire form of a lens: it survives serialisation, a
client can send it, `JsonOptic.path` compiles it against the schema,
a key the schema does not write is refused by name, and a
`TypedZipper`'s `pathKey` is such a key. The law `TestWatched` pins:
over any history of edits, a subscriber receives exactly the distinct
consecutive values of its focus, and nothing of anyone else's.
Design and the trigger's history: specs/optics-outside.md stage 10.

**`okay-demo`'s wiring**, the two call sites this module replaced:

```scala
private val marketFeed = Hub[String]()
def marketSub(): Channel[String] = marketFeed.subscribe()
def marketChanged(kind: String): Unit = marketFeed.publish(kind)

private val inboxes = Registry[String, String]()
def inbox(email: String): Channel[String] = inboxes(email)
```

Each `Channel` is drained the way any `okay` consumer drains one — an
SSE route in the demo's case, over `Writer.of(...)`.

| | |
|---|---|
| `Hub[A]().subscribe()` | a fresh `Channel[A]`, remembered |
| `Hub[A]().publish(a)` | offered to every channel remembered so far |
| `Registry[K, A]()(key)` | the channel for `key`, created once, reused after |
| `LiveHttp.routes(watched, prefix)` | a `Watched` over HTTP: `GET <prefix>/watch?key=` is server-sent events (the value now, then each change), `POST <prefix>/set` writes (204; 409 where the place is absent; 400 where the schema has none) |
| `LiveHttp.client` / `LiveHttp.react` | the TypeScript client `live.ts` (typed `watch`/`set`, the `<okay-live>` element) and the React hook `useWatch` |

The TypeScript side, typed by the document's Scala Schema:
[a live frontend, typed by path](../typescript.md#a-live-frontend-typed-by-path).

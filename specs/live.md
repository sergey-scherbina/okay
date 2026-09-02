# okay-live: broadcast and per-key channels

## Overview

Second of the round-two demo extractions (BACKLOG.md, user ask):
found by NOTICING the same pattern already independently duplicated
TWICE in `okay-demo/ChatDemo.scala` for two different SSE features —
`marketFeed`/`marketSub`/`marketChanged` (demo-market-live: ping
every subscriber when the market changes) and `inboxes`/`inbox`
(demo-chat-async: a per-email `Channel`, created on first use, that
a `/events/<email>` stream reads). Neither mentions the marketplace;
both are the generic shape "many live viewers, one event source" —
exactly the pattern any future live-updating page (a dashboard, a
presence list, a chat room) would need again.

## The model

Two primitives, both built directly on the existing `okay.Channel`
(core, cross-platform) — this module is JVM-only itself because the
REGISTRIES holding channels use `java.util.concurrent` collections
(the same reasoning `okay-subscription` already made: `Channel`
itself stays cross-platform, the JVM-only concurrent map is the
implementation detail of tracking many of them).

- **`Hub[A]`** — broadcast. Each `subscribe()` call mints a fresh
  `Channel[A]` and remembers it; `publish(a)` offers `a` to every
  channel remembered so far. A closed/abandoned subscriber's channel
  stays remembered until process end — stated, not hidden (the same
  honest limit `marketFeed` already had; a subscriber count in the
  thousands would want eviction, human-scale viewer counts do not).
- **`Registry[K, A]`** — a channel per key, created lazily on first
  `apply(key)` and reused after. No removal — same honest limit.

## Interface

```scala
package okay.live

final class Hub[A]:
  def subscribe(): Channel[A]
  def publish(a: A): Unit

final class Registry[K, A]:
  def apply(key: K): Channel[A]
```

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `marketFeed`/`marketSub`/
  `marketChanged` become one `private val marketFeed = Hub[String]()`
  with `marketFeed.subscribe()`/`marketFeed.publish(kind)`;
  `inboxes`/`inbox` become `private val inboxes = Registry[String,
  String]()` with `inboxes(email)`. Behavior unchanged — every call
  site's own semantics (what gets published, who subscribes) stays
  in the demo; this module only holds the channel bookkeeping.

- [x] `Hub`: two subscribers both receive a published value (both
      channels get their own copy, not a fanned-out competing read)
- [x] `Hub`: a subscriber added AFTER an earlier publish does not
      see that earlier value (publish reaches only CURRENT
      subscribers, matching `marketFeed`'s existing semantics)
- [x] `Registry`: the same key returns the SAME channel on repeated
      calls; different keys get independent channels
- [x] through the real demo route: `/market`'s live feed and
      `/events/<email>` behave identically to before this move (the
      existing demo tests, unchanged in substance, still pass)

## Decisions

- **JVM-only, not cross-platform** — same reasoning as
  `okay-subscription`: the registries are `java.util.concurrent`
  collections; `Channel` itself (what they hold) is already
  cross-platform, so a JS/Native consumer loses nothing by this
  module staying JVM-only — it would build its OWN registry over
  whatever concurrency primitive its platform offers, the same way
  it already builds everything else platform-specific.
- **No eviction, no unsubscribe** — matches the two call sites this
  was extracted FROM exactly; adding lifecycle management now would
  be solving a problem neither demo feature has hit, the abstraction-
  before-a-need this codebase's own doctrine warns against. Worth a
  BACKLOG note if a real consumer needs it, not built speculatively.
- **Two small classes, not one generalized "channel tree"** — `Hub`
  and `Registry` are different enough (one holds a growing LIST, one
  holds a growing MAP) that fusing them into one abstraction would
  cost a type parameter and buy nothing either call site wants.

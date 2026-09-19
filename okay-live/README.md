# okay-live

Broadcast and per-key channels over the core's own `Channel` (specs/live.md): `Hub[A]` (subscribe/publish to everyone) and `Registry[K, A]` (one channel per key, created on first use). Extracted 2026-09-02 from `okay-demo`, where the identical pattern had already been written twice by hand — `marketFeed` (a broadcast ping to every `/market` viewer) and `inboxes` (a per-email channel).

**Depends on:** `okay` (`Channel` is already cross-platform core; only

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-live.md`](../docs/modules/okay-live.md) | what it is, and the reasoning |
| [`specs/live.md`](../specs/live.md) | the design and its decisions |

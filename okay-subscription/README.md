# okay-subscription

Gate a resource behind a paid period (specs/subscription.md): free for the subject's first calendar month, then only a period actually paid keeps it visible — unpaid is GATED, never deleted. Extracted 2026-09-02 from `okay-demo`, a pure move: the logic already took a bare `String` id and had no dependency on `MatchStore`/`ChatLog`.

**Depends on:** `okay-agent` (`ToolSpec`, for the tool contract only).

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-subscription.md`](../docs/modules/okay-subscription.md) | what it is, and the reasoning |
| [`specs/subscription.md`](../specs/subscription.md) | the design and its decisions |

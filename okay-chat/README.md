# okay-chat

A streaming LLM chat component (specs/chat.md): the model seam, Cut-guarded SSE framing, and the `/chat` route — extracted 2026-09-02 from `okay-demo`'s `ChatDemo.scala` (a pure move; the demo's page and market-flavored logic stayed where they are).

**Depends on:** `okay-llm` (the model, `Cut`), `okay-http`, `okay-conf`

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-chat.md`](../docs/modules/okay-chat.md) | what it is, and the reasoning |
| [`specs/chat.md`](../specs/chat.md) | the design and its decisions |

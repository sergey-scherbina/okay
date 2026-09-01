# Sprint

## Doing
- ui-dom-patch — the raw-DOM patch Backend over js.Dynamic: paths
  walk childNodes, the mirror tree is Ui.patch, events are one
  delegated listener through React.event; proven against a fake DOM
  under Node (specs/ui.md names it in Out of scope v1 -> now)

## Queue
(next candidates from BACKLOG.md: sql-seam, conf-impl,
 persist-wire — the seams the most filed work binds to; ui-durable
 and mcp-resumable-sse can now bind to stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.

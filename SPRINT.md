# Sprint

## Doing
- codec-defaults — decode falls back to a field's declared default
  when the wire lacks it: a macro reads the companion's
  <init>$default$N (Mirrors do not carry defaults), SProduct carries
  them, Json/Cbor fall back, ToolSpec stops requiring defaulted
  fields (specs/codecs.md files it with its reason)

## Queue
(next candidates from BACKLOG.md: sql-seam, conf-impl,
 persist-wire — the seams the most filed work binds to; ui-durable
 and mcp-resumable-sse can now bind to stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.

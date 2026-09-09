# Sprint

## Doing
- lexer-buf-without-concat — BACKLOG: ~171 B per lexed character
  remain, and `buf: String` grown per char is most of it. PRICE the
  three candidates before building one; the chunked path (no input to
  slice from) is the constraint that killed the obvious fix. Claimed
  2026-09-09 (.work/active/lexer-buf-without-concat.claim).
## Queue
(the note that stood here named eight candidates; SIX have since
 landed — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector, persist-wire, cache-memory, each with a CHANGELOG
 entry — and `mcp-resumable-sse` exists in neither board any more.
 Checked and rewritten 2026-09-09; a queue that names finished work
 sends the next agent looking for it. `ui-durable` survives only as a
 mention inside another BACKLOG entry, not as an item of its own.

 Open and named, for whoever picks next: lexer-buf-without-concat
 (~171 B per lexed character, three unpriced candidates),
 bracket-over-region (21%, §7), json-strict-is-now-the-slow-door
 (likely a wontfix — its entry states the disqualifying condition),
 scan-step-allocation (a tuple per character, an interface change,
 and it wants lexer-buf-without-concat priced first))

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.

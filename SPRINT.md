# Sprint

## Doing
- bracket-pairing — docs/benchmarks.md §7 / BACKLOG bracket-over-region:
  the two lanes do different work (the bracket lane performs a Produce
  effect and a nested runWith per step, the region lane one acquire),
  so the "21%" is an unpaired comparison. Pair them, B/op first (the
  box has three sibling builds), time only when it is quiet. Claimed
  2026-09-09 (.work/active/bracket-pairing.claim).
## Queue
(the note that stood here named eight candidates; SIX have since
 landed — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector, persist-wire, cache-memory, each with a CHANGELOG
 entry — and `mcp-resumable-sse` exists in neither board any more.
 Checked and rewritten 2026-09-09; a queue that names finished work
 sends the next agent looking for it. `ui-durable` survives only as a
 mention inside another BACKLOG entry, not as an item of its own.

 Open and named, for whoever picks next: json-strict-is-now-the-slow-door
 (likely a wontfix — its entry states the disqualifying condition),
 scan-step-allocation (a tuple per character, an interface change).
 lexer-buf-without-concat is GONE from this list: it landed 64d7af6a
 as a refutation (the array candidate measured 11-12% WORSE), which
 also prices what scan-step-allocation was waiting for.
 bracket-over-region is claimed as bracket-pairing above)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.

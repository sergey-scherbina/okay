# Typestate at the protocol seams: PState earns its keep

## Overview

`PState` (State.scala) is the type-changing state paramonad: a
computation that moves the state's TYPE from S to S2, with flatMap
composing the transitions — so a PROTOCOL's step order becomes a
fact the compiler checks. The core has carried it as theory with a
measured price (a stack frame per operation, ~1.7x the plain State
handler) and no consumer. This spec names the consumers — the
places where this stack currently guards step order with a runtime
check someone could forget — and draws the line where typestate is
NOT worth its ceremony. Born of the user's question (2026-09-01):
"where are PState and Delim useful?"

The class of bug it removes is real here, today:

- `Scram` (okay-pg) holds a mutable `serverSignature`; calling
  `verifyServerFinal` before `clientFinal` would NPE — the order
  lives in a comment.
- `PgSql`'s connection phases (startup → auth → ready →
  parse/bind/execute/sync) are sequenced by discipline inside one
  file; a misordered helper call is a protocol violation the
  backend punishes at runtime.
- `JdbcSql`/`PgSql` `begin`/`commit`/`rollback` guard
  double-begin with a runtime throw ("nested transaction").
- The future `RaftStore` role protocol (Follower → Candidate →
  Leader, each with its own legal actions) — noted already in
  specs/consensus.md.

## The landscape moved while this spec was drafted

The room ran ahead (same day, operator-steered): sql-typestate
LANDED (`Typed.region`, a `Db[S]` phantom over Tx.No/Tx.Yes —
nested begin is a compile error, proven by compileErrors),
stage-phased LANDED (a type-changing accumulator in core),
ui-pwizard LANDED (the growing-state wizard). So this spec's job
shrank to what it should have been: the CRITERIA, the pointers,
and the one remaining consumer.

The criteria, agreed in the room:

- typestate pays where phases are ONE-WAY through an ABSTRACT
  boundary; cycling automata gain nothing; lexically-visible
  accumulation is already covered by for-bindings.
- the CHEAPEST adequate mechanism wins: two states — a phantom
  type (sql-typestate's move); a short linear handshake — PHASE
  OBJECTS (each step returns the next phase's object, so an
  out-of-order call does not EXIST as a method); a type-CHANGING
  accumulator — PState proper (stage-phased's move). PState's
  per-op frame prices it out of per-row work either way.

## The rule: internals yes, public seams no

Decided up front (and taken to the room, 2026-09-01): typestate
goes INSIDE driver and protocol implementations, and does NOT
change public traits.

- `Sql`, `Docs`, `Topic`, `Cache` stay exactly as they are —
  consumers bound to them never rebind (the stage-0 promise), and
  a typestate-parameterised public trait would tax every caller
  with type plumbing for protocols the REGIONS already
  encapsulate (`Typed.transact` exists precisely so callers never
  touch begin/commit).
- Inside a driver, the phases become types:

```scala
// the shape, for Scram (the smallest and clearest first cut):
//   Initial --clientFirst--> AwaitFirst --clientFinal--> AwaitFinal
//   --verifyServerFinal--> Done
// each arrow is a PState step: Cont[Out, Next => R, Cur => R]
// calling verifyServerFinal on anything but AwaitFinal does not
// compile; the mutable serverSignature field disappears into the
// state it always was
```

- The cost note travels with the rule: PState's per-operation
  frame is irrelevant at handshake and transaction rates; it would
  NOT be acceptable per row or per record, which is another reason
  the row-processing seams stay untyped by state.

## Behavior

- [ ] Scram rebuilt as PHASE OBJECTS (the room's counter-proposal,
      accepted: cheaper than PState for a three-step line): each
      step answers the next phase's object, a misordered call does
      not compile because the method does not exist there
      (compileErrors-pinned); the live SCRAM battery against
      Postgres unchanged and green; the mutable serverSignature
      field dissolves into the phase it always was
      (filed as pg-scram-typestate — the room may claim it)
- [x] the transact protocol typed — LANDED as sql-typestate
      (d128885, another lane): Typed.region with the Db[S]
      phantom; nested begin is a compile error naming Tx.Yes
- [ ] the public seams stay: no signature of Sql/Docs/Topic/Cache
      changes (structural: their sources untouched by any
      typestate claim)
- [x] a doc example in the theory points at shipped consumers —
      the textbook's ch.3 now cites sql-typestate and
      stage-phased (their lanes' landings)

## Out of scope

- typestate on public traits — rejected above, it is the decision
- PgSql's full extended-protocol phase graph — MEASURED after Scram
  proved the pattern (pg-wire-typestate, 2026-09-02) and declined;
  the measurement is in Decisions below
- RaftStore's roles — that claim's own work (specs/consensus.md
  already carries the note)

## Decisions

- **Internals, not seams** — regions and traits already
  encapsulate protocols for CALLERS; typestate protects the
  IMPLEMENTOR, where the order actually lives. Rejected: typestate
  in public signatures (a tax on every consumer for a protocol
  they cannot even violate).
- **The cheapest adequate mechanism wins** — phantom types for
  two states, phase objects for short linear handshakes, PState
  for type-changing accumulation; the room's criterion, adopted.
  Rejected: PState everywhere (ceremony where a phantom or an
  object suffices, and a per-op frame where rows flow).
- **Compile errors are the test** — munit's compileErrors makes
  "does not compile" an assertion, so the property the feature
  exists for is itself tested. Rejected: trusting the types
  silently.

- **PgSql's phase graph stays untyped — measured, not assumed**
  (pg-wire-typestate, 2026-09-02). The graph inside PgSql.scala,
  phase by phase against the criteria above: startup → auth →
  ready is ALREADY one-way by construction — `PgSql`'s constructor
  is private and the object exists only when `connectOver` reads
  ReadyForQuery, and the auth line inside it is Scram's phase
  objects; ready ↔ in-transaction is a CYCLE on the public `Sql`
  seam (begin/commit/rollback), which the rule above forbids typing
  and which `Typed.region`'s `Db[S]` already types for callers —
  the `inTx` runtime guard is what the trait's contract demands;
  the portal (`openPortal` → `readChunk`* → `finishPortal`) and
  COPY (`awaitCopy` → rows → `collectReady`) sequences are local
  defs inside ONE method each, whose only caller is a for-binding
  three lines below — the "lexically-visible, covered by
  for-bindings" case; and the one cross-cutting order rule someone
  COULD forget — every public entry must pass through `settled`
  (cancel's pending rollback) — was checked entry by entry (all
  eight do), and is a structural property a Portal or Session type
  would not express anyway. A `Portal(oids)` phase object was the
  cheapest candidate; its whole gain is one threaded parameter
  becoming a field, at the price of a class where a local def
  reads in place. PState does not fit at all: no phase changes the
  state's TYPE. Rejected: typing the graph for symmetry with Scram
  — the pattern pays where the order crosses an abstract boundary,
  and here every remaining order is either already a type, a
  public cycle, or a single method. The box closes as a decision;
  the entry point that would reopen it is a second consumer of the
  driver's internals (a pool or a pipeline that interleaves
  portals), which would make the portal phase an abstract boundary.

## Results

(after implementation — Scram on PState with the live battery
green, the compile-error assertions, the internals-typed transact)
pg-wire-typestate (2026-09-02): the pg graph measured and left
untyped — see the Decision; PgSql.scala unchanged, which is the
result. The spec's "cheapest adequate mechanism" criterion now has
its zero case on record: sometimes the adequate mechanism is the
for-binding already there.

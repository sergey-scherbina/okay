# okay-federation — aggregation across parties, where the records stay home

## Overview

The dataflow engine (specs/dataflow.md) has one property that was
measured before it was understood as a product: **what crosses the
wire is accumulators, not records.** On the Wrocław job 122 679
accumulators reach the coordinator where the job produces 1 734 893
panes over 1 255 298 events. Stage 7 weighed it — 5.44 bytes per
event — and stage 4's Claim 2 is the reason: a keyed stage finishes by
`Aggregator.merge`, so a partition hands over a summary of what it
saw and never what it saw.

Federation is that property applied across an ownership boundary. N
parties, each with its own log (okay-persist), each running its own
partition of a job on its own machine, handing a coordinator nobody
owns exclusively an accumulator per key. The records do not leave. The
aggregate does.

This is not privacy as a feature bolted on. It is the consequence of
an architecture that already exists, and this spec's job is to say
precisely what it does and does not give.

## The claims

1. **A job over N logs owned by N parties computes what the same job
   over their union computes, and no party's records cross to any
   other.** Falsifiable: the answer is asserted equal to the
   single-log run, and the bytes that cross are counted and shown to
   be accumulators (stage 7's instrument).

2. **What leaves a party is exactly what the aggregator's `merge`
   needs, and nothing the job did not ask for.** Falsifiable: the
   partial's Schema (`Wire.wire`) IS the list; a test decodes what
   crossed and finds only accumulators keyed by the job's keys.

3. **An aggregate can still be a record.** A count of one, a max over
   one element, a key that identifies a person — these are records
   with a different name, and this spec says so rather than claiming
   otherwise. What the engine offers is that the LEAK IS THE SCHEMA:
   it can be read, reasoned about and bounded before a job runs,
   because nothing else crosses.

## What exists

- the coordinator as a seam: `Lease` and `Checkpoint` (stages 8, 10)
  — any process may coordinate, and two may not at once
- `okay.codec.Compat`: schema compatibility between parties who do not
  share a build
- `okay-crdt`: counters, registers, sets that merge with no
  coordinator at all — the shape a federated aggregate takes when the
  parties cannot agree on who coordinates
- `okay-persist`: each party's log, with `Election` and `Offsets`
- `okay-security`: what a party may be asked, and by whom — not yet
  connected to any of the above

## What is the work, and it is not the engine

- **The trust boundary.** Who may submit a job to a party's worker,
  what a worker will compute, and what it refuses. Today a worker
  runs any job its build knows by name. Federation needs a worker
  that runs a job its OWNER allowed, for a coordinator its owner
  recognises.
- **Schema across organisations.** `Compat` says whether two Schemas
  agree; federation needs the answer BEFORE the job, at submission,
  with the refusal naming the field.
- **The bound on the leak.** Claim 3, made mechanical: a job's partial
  Schema examined for keys that identify, counts that could be one.
  Not a solved problem; a named one.
- **Nobody's coordinator.** Today the coordinator is a process one
  party runs. Two parties who trust the arithmetic and not each
  other want the merge to happen where neither can read the other's
  partial — which is a different mechanism (a CRDT merge on both
  sides, or a third party) and is out of scope until Claims 1-2 are
  demonstrated.

## Stages

- **0 — this spec.**
- **1 — two parties, one machine.** Two okay-persist logs in two
  processes, a job over both, the answer equal to the union's, and
  the bytes that crossed shown to be accumulators. Everything on one
  machine — which is the honest first demonstration, since federation
  is about ownership and not distance. LANDED (TestFederation; see
  Results).
- **2 — the refusal.** A worker that runs only jobs its owner
  allowed; a submission from an unrecognised coordinator refused with
  a reason. LANDED (`federation-refusal`, 2026-09-18) as
  `Cluster.guarded`. `okay-security` is NOT connected and is not
  needed to be: this is authorisation over an identity the transport
  establishes, and which `Capability` a party accepts as that identity
  is stage 3's question, at the door, with `Compat` beside it.
- **3 — schema at the door.** `Compat` at submission: a job whose
  partial Schema a party cannot decode is refused before it runs.
  LANDED (`federation-schema-door`, 2026-09-19). See the Design
  section: a `Schema` cannot itself travel (it carries closures), so
  a `Compat`-comparable SHAPE does.
- **4 — the leak, read.** A tool that prints what a job's partial
  Schema lets out, per key, and flags a count that can be one. LANDED
  (`federation-leak-read`, 2026-09-18) as `okay.cluster.Leak`.
- **5 — the network.** With stage 12 of dataflow: parties on
  machines. BLOCKED on the same machines.

### Stage 3: a Schema cannot travel, so its shape does

`Compat.compare` needs two LIVE `Schema` values, and a `Schema` is not
itself Schema-derivable: `SProduct`'s `make`/`parts` and every field's
own thunk are functions, and no codec serialises a function. Two
processes that do not share a build cannot exchange a `Schema[A]` —
only a description of its shape.

`okay.codec.Digest` is that description: a small, `derives Schema`
mirror of exactly what `Compat.walk` reads to compare two schemas —
names, nesting, field presence, whether a default exists — and nothing
it does not (never a value, never a function). Reading `Compat.walk`
proves this is enough: it inspects `.name`, `.fields`/`.cases`
(thunked SCHEMAS, recursed into — never called for a VALUE) and
`.defaults` (`.isDefined`, a presence check, never the thunk itself).
Nothing it does is a value-level operation, so a data-only mirror of
the same shape carries everything the comparison needs.

**The reconstruction is one isolated, defended cast.** `Digest.of`
builds a `Digest` from a live `Schema[?]`; `Digest.compare(local,
remote)` needs to hand the EXISTING `Compat.compare` two live
schemas, so it turns the remote `Digest` back into a `Schema[Any]`
SHELL whose `make`/`parts`/`caseOf` throw if ever called. That throw
never fires — proven by the same reading of `Compat.walk` above — and
if a future change to `Compat` ever called one, the throw says so
loudly rather than corrupting a comparison silently. `Compat.scala`
itself is unchanged: this is new code beside it, not a rewrite of it.

**A self-referential type does not loop.** A recursive schema (a tree)
needs `Compat.walk`'s own recursion guard (a repeated `(name, name)`
pair returns `Vector.empty` before touching `.fields`), and `Digest.of`
needs the SAME guard while BUILDING, for the same reason a live
schema's thunk would otherwise be re-entered forever. A repeated name
is truncated to an empty product/sum. This is safe, not merely
convenient: `Compat.walk`'s guard fires on the NAME PAIR *before* it
ever inspects a repeated occurrence's fields, so the truncated stub is
provably never consulted — the outer, first occurrence always carries
the real fields.

**The direction that matters.** A party ENCODES its own partial with
its own live schema; the coordinator DECODES it with its own. The
question a party must answer before writing a byte is "will the
reader on the other end, with ITS schema, be able to decode what I
write with MINE?" — exactly `Compat.compare(mine, theirs).backward`
(the party's schema plays "old"/the producer, the coordinator's
digest plays "new"/the reader, in `Compat`'s own naming). No other
direction exists in this protocol: a party never decodes anything with
`sink.wire` — only `job.params`, a separate schema this box does not
touch.

**Where the check runs, and where it does not.** `Req.Extent`,
`Req.Run` and `Req.Open` each NAME a job (the same three
`Cluster.guarded` already gates by coordinator identity), so each
carries the coordinator's digest of what it expects. `Req.Advance` and
`Req.Close` name a SESSION, admitted at `Open`, and carry none — the
check already happened. An EMPTY digest (the default every existing
caller gets for free) skips the check entirely: a coordinator that has
not been rebuilt with this box keeps working exactly as before, the
same "hasn't asked yet" shape as `Checkpoint.none`/`Lease.solitary`.

## Behavior

Stage 1:
- [x] two `MemoryStore` logs in two OS processes, one job, the
      union's answer — `TestFederation`, two `WorkerMain` processes
      each started as `-Dokay.party=N`, the run's value, drop count
      and merged count equal to the fan's over the whole feed
- [x] the bytes that crossed decode as accumulators under
      `Wire.wire` and as nothing else — every partial decodes AND
      re-encodes to the same bytes, so no byte is outside the Schema;
      582 bytes crossed for 341 923 held (0.17%)
- [x] a party's log is never read by the other's process (asserted
      by the store, which counts its readers) — in-process, each
      party's counting store hands out exactly its own slice (twice:
      once to the windowed pre-pass, once to the run) and none of
      another's; with party B dead, A is asked for B's partition and
      REFUSES (`Cluster.Refused` → `Resp.Failed`, not retried), the
      run fails naming B, and B's store counts zero reads
- [x] the same over `Cluster.stream` resuming at a position — a party
      that resumes must resume from ITS log (`federation-refusal`,
      2026-09-18). It was NOT a three-line test, and the reason is
      worth the correction: the sink here is WINDOWED, so before
      dataflow's box 2b it could not seek at all, and the party's
      FLOW was a `Flow.of`, which skips by READING — so the first
      version of the test seeked correctly and re-read all 10 000
      records anyway, because a source that cannot seek costs the
      whole log whatever the session asks for. A party's log is a
      topic and an offset is a number: `Flow.seekable` closes it, and
      the test now asserts each party reads MORE than nothing and LESS
      than its whole log, with the horizon marks in the journal



Stage 2:
- [x] a worker with an allow-list refuses a job not on it, by name,
      with the list — `Cluster.guarded(jobs, coordinators)(caller)`
      wraps any `Serve` and answers `Resp.Failed` before the request
      reaches the job
- [x] a coordinator without a recognised identity is refused before
      the pre-pass, and the checks are in THIS ORDER for a reason: an
      unrecognised caller learns only that it is not recognised, never
      which jobs the party allows, which would be a directory of its
      business handed to whoever knocked. The test asserts the
      absence, and asserts that the party's log counted zero reads
- [x] the caller is a property of the CONNECTION, not of `Req`.
      Putting an identity in the message would put it where the
      sender controls it; a socket authenticates once and every
      request on it comes from the party that authenticated. So
      `guarded` wraps a `Serve` and `Req` is unchanged — this is the
      AUTHORISATION half, and authentication stays `okay-security`'s
      (a `Capability` narrows without its issuer, which is the shape a
      delegated submission wants)
- [x] `Advance` and `Close` name a SESSION rather than a job, so the
      job check happened when it was opened — the coordinator check
      still runs on every request, and a stranger cannot advance a
      session somebody else opened

Stage 3:
- [x] `Cluster.schemaChecked(base)` wraps a `Serve` and refuses, before
      `job.extentAt`/`partialAt`/`openAt` ever runs, a request whose
      coordinator-supplied `okay.codec.Digest` disagrees (backward-
      incompatibly) with what the party's own build would produce for
      that job. OPT IN, like `guarded`, and composes with it — the
      SAME three request kinds (`Extent`/`Run`/`Open`) that name a
      job carry the digest; `Advance`/`Close` name a session and
      carry none
- [x] `Cluster.run`/`Cluster.stream` compute the digest ONCE per run
      (from `sink.wire`) and attach it to every request, unconditionally
      — a party that never opts into checking it pays nothing and the
      digest simply goes unread; an EMPTY digest (every existing
      caller's default) skips the check entirely, so a coordinator
      built before this box changes nothing for anybody
- [x] the refusal comes BEFORE any read — the same rule stages 1 and 2
      both already established — and names the job and what changed,
      via `Compat.Report.render`
- [x] a job not found is left to the ordinary "no job named" answer
      downstream, never duplicated by this check

Stage 4:
- [x] `Leak.of(wire)` walks a `Wire#wire` Schema and reports every
      field that crosses, per key where there is one (a `Wire.keyed`
      or `Wire.windowed` partial's key-then-value convention,
      `Wire.pair`/`Wire.triple`) and per PARTITION where there is not
      (a `Wire.fold` partial's whole accumulator, which asks the same
      question at that grain)
- [x] a KEY whose shape has no bound on its cardinality (`String`,
      `Bytes`) is flagged UNBOUNDED — an email address and a
      two-letter country code have the identical shape, so this is a
      thing to read, not a verdict
- [x] a VALUE that is a bare scalar with nothing beside it to say how
      many records made it is flagged UNCOUNTED — Claim 3's "a count
      of one, a max over one element" made mechanical: a group of one
      is then indistinguishable from that record's own field. Proven
      against this repository's OWN shapes, not invented ones:
      `TestJobs.value` (a bare `Long` per key) is uncounted;
      `PartyJob`'s own partial — the job `TestFederation` actually
      federates over — is uncounted for the identical reason
- [x] a COMPOUND value (a user's own accumulator, whatever it derives)
      is named and left there — reported as one field, never
      fragmented into its own leaves. `Feeds.Sum(n, total, x)` is
      reported as one `Sum`, not three independently-flagged `Long`s;
      whether its fields answer "how many" is a question about what
      they MEAN, which is not a Schema's business

## Results

### Stage 1 — two parties, one machine

What the stage found that the spec had not said:

- **A refusal must be a distinguished answer on BOTH roads.** Over a
  socket, `Served.handle` already answered any throwable as
  `Resp.Failed`, which `ask` does not carry to another worker. In
  process, `Cluster.local` let the throwable propagate, and `ask`
  read it as a death: party A's refusal to compute B's share buried A
  and asked B — then reported "no workers left". The union job was
  correct on the socket road and wrong in process, which is the
  worst kind of split, because tests without sockets are the fast
  ones. `Cluster.Refused` is the fix: a throwable that MEANS "no",
  answered as `Resp.Failed` by `local` too. A crash still propagates
  in process and is retried as a death — only the refusal is set
  apart. Refuted in place: with the change stashed, the two refusal
  tests fail with "no workers left".
- **The refusal comes before any read.** A party's store is a
  ONE-partition topic — there is nothing else in it — and the party
  checks the partition number against its own identity before it
  opens the log. A store that answered "no records" for a foreign
  partition would let a misplaced partition compute an EMPTY share
  in silence, which is precisely the failure federation exists to
  prevent. Emptiness is not a refusal.
- **Identity is process state, not a parameter.** Every worker gets
  the same encoded parameters, so "which party am I" cannot travel
  in them; `-Dokay.party=N` at start, held in a `DynamicVariable` so
  that one JVM can be several parties for a test (`Party.as(n)`
  wraps `Cluster.local`) and the same code runs either way.
- **A windowed job reads a party's log twice.** The pre-pass
  (`Req.Extent`) reads the partition for its bounds before the run
  reads it for its panes. The counting store made that visible; a
  party paying for reads should know it, and a fold or keyed sink
  (no pre-pass) reads once.
- **What crossed:** 582 bytes for 20 000 records (341 923 bytes) —
  0.17%, and every one of them re-encodes from the decoded panes.
  This is a count, not a bound on what the panes REVEAL; the bound
  is stage 4's.

## What this is not

Not differential privacy, not secure multi-party computation, not a
guarantee that an aggregate reveals nothing. It is the narrower and
checkable thing: the records stay, the aggregate goes, and what the
aggregate contains is written down as a Schema before anything runs.

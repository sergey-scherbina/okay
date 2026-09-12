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
  a reason. `okay-security` connected.
- **3 — schema at the door.** `Compat` at submission: a job whose
  partial Schema a party cannot decode is refused before it runs.
- **4 — the leak, read.** A tool that prints what a job's partial
  Schema lets out, per key, and flags a count that can be one.
- **5 — the network.** With stage 12 of dataflow: parties on
  machines. BLOCKED on the same machines.

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
- [ ] the same over `Cluster.stream` with a seekable sink resuming at
      a position — a party that resumes must resume from ITS log; box
      2 of dataflow stage 11 makes this a three-line test, not built

Stage 2:
- [ ] a worker with an allow-list refuses a job not on it, by name,
      with the list
- [ ] a coordinator without a recognised identity is refused before
      the pre-pass

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

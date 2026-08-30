# P7 — okay-cluster: the own distributed runtime

## Overview
Besides the P4 bridges (Spark/Flink/Kafka do the heavy lifting on
their clusters), okay gets its OWN small distributed runtime (the
user's decision: both bridges and our own, in a separate module).
Everything below stands on parts specified elsewhere; this spec is the
assembly plan, detailed at implementation time.

## Design (assembly of existing parts)
- A node runs fibers (Scheduler) and channels; a REMOTE channel is a
  Channel whose other end is on another node — same interface, the
  transport underneath.
- Transport: okay-codec (CBOR for the wire) + the cross-platform
  Async (Await-based I/O) — which is also how the one-source
  client/server policy is fulfilled: a JS client and a JVM server run
  the same code and speak the same codec.
- Actors: an actor is a Stage (awaits messages, tells messages) with
  a mailbox Channel — no new abstraction, the coroutine machinery
  distributed.
- Distribution of WORK rides P1/P2: an Aggregator's
  (zero, seqOp, combOp) is the merge contract between nodes; chunks
  are the shipping unit; per-chunk retry is the fault model, with
  replayable-source capabilities deciding what can be re-asked.

## Behavior (assembly-level)
- [ ] two local processes exchange chunks over a remote channel and a
      merged fold agrees with the local run
- [ ] a killed worker's chunks are recomputed on another (replayable
      source), the aggregate stays correct
- [ ] a JS client drives a JVM server with the same shared-source
      program (the policy's acceptance test)

## Out of scope (initially)
- membership/discovery beyond static configuration; persistence;
  exactly-once guarantees (at-least-once + idempotent merges first)

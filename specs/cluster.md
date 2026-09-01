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
  The transport plumbing was MEASURED on 2026-09-01
  (`ClusterTransportBenchmark`, per round of 100 chunks of 64 Longs
  over localhost) and the honest outcome is that it stays as it is.
  Four lanes: the shipped BufferedReader/PrintWriter transport 37.9ms;
  blocking sockets with byte reads and a SINGLE flush 24.4; NIO 24.7;
  the JSON codec alone, no wire, 25.9. A byte rewrite of Remote with
  the flush-per-send that streaming requires measured 38.4 — equal to
  the shipped code — so the 1.55x in the middle lane was the FLUSH
  POLICY (one packet per chunk against one big write), not the text
  machinery, and the rewrite was reverted rather than kept: equal
  performance, more code. Two findings stand. Loom parking is free —
  NIO against a parked blocking read is a wash. And the codec IS the
  transport within noise (25.9 of 37.9), which makes the CBOR dialect
  this spec already plans the lever, not the socket API. The
  deconfounding lane ran on 2026-09-01: `blockingBytesFlushed`
  measured 50.8 ±9.5 — slower than the shipped transport's
  38.2 ±0.5 — so bytes with a flush per line beat nothing, which is
  consistent with the rewrite's wash and closes the byte-plumbing
  question for good. The NIO half stays open for a NEW reason: its
  lane read 59.0 ±48.6 with three fork failures on the sum
  assertion — the NIO path sporadically loses data around close
  under per-line sends. A correctness lead now, not a performance
  one (BACKLOG: nio-close-race).
  (Found on the way, by a torn frame: the "total" JSON parser threw
  NumberFormatException on five number-shaped damages — "-", "[1,2,-",
  "-e5", "1e". Fixed in okay-codec, pinned by TotalityProbe.)
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
- [x] two ends of a wire exchange chunks over a remote channel and
      the merged fold (variance!) agrees with the local run; a damaged
      frame is dropped and the stream lives
- [x] a killed worker's chunks are recomputed on another (replayable
      source), the aggregate stays correct — Cluster.distribute:
      workers behind the one seam `Chunk[A] => Acc` (in-process or a
      wire away; a dead worker THROWS, that is the whole protocol),
      round-robin over the living, the failed worker leaves the
      rotation and its chunk — still in hand, the source is a value —
      goes to a survivor; partials merge by combOp. Tested with a
      dying in-process worker (variance exact, every element counted
      once) AND a socket worker whose server drops the connection
      mid-stream.
- [x] a JS client drives a JVM server with the same shared-source
      program (the policy's acceptance test) — DONE, exactly by the
      plan: (1) okay-lex/parse/codec are crossProjects now (pure
      Scala; their suites run on JS too), (2) okay-cluster is a
      crossProject — jvm holds Remote/Cluster, js the Node client
      (js.Dynamic over `net`, CommonJS module), shared the ONE
      `Acceptance` object (source, statistic, frames, expected) both
      ends compile, (3) TestAcceptance starts a JVM fold-server,
      spawns `node <fastLinkJS output>` (the build wires the link
      dependency and passes the path as a system property), the
      client streams the shared frames, awaits the answer through
      runAsync and exits 0 on agreement — asserted, plus every frame
      counted on the server. Gotcha: Scala.js main(args) does NOT get
      process.argv — read it explicitly.

## Out of scope (initially)
- membership/discovery beyond static configuration; persistence;
  exactly-once guarantees (at-least-once + idempotent merges first)

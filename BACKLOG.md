# Backlog

## Flakes observed (record → fix loop when they recur)
- [ ] persist-election-replicated-flake — okay.persist
      .TestElectionReplicated errored at suite level on one platform
      under the full matrix (2026-09-01, Errors 1 with 0 failures;
      JVM tests of the same suite green in the same run); second
      platform run printed the header with no tests. Suspect
      platform init under load. Settle by: run the suite alone on
      JS/Native; if it reproduces, isolate.
- [ ] netty-ws-matrix-flake — okay.netty.TestBackends "every WebSocket
      client talks to every WebSocket server" failed once under the
      full sbt test matrix (jetty StaticException: Closed,
      2026-09-01, log: one failure in 12); twice green in isolation
      right after. Suspect load/port timing, not code. Settle by:
      run the suite in a loop under parallel matrix load; if it
      reproduces, isolate per the isolate skill.

## Correctness and the core (specs/sim.md, specs/typestate.md)
- [ ] wire-typestate — PState at the protocol seams
      (specs/typestate.md; user ask 2026-09-01): SCRAM's step
      order and PgSql's connection phases as types (out-of-order =
      compile error), the Sql transact protocol for driver authors

## Cross-cutting — the 2026-09-01 audit (specs landed e3b5a74; slugs are implementation)
- [ ] queue-shape — DECIDED (specs/data.md, Queues): no Queue seam;
      ingress/egress bridges to topics with message-id dedup —
      implement the two bridges; engine adapters as named

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-security (specs/security.md — staged, like persist)


## okay-codec

## okay-py (specs/py.md — Python as a handler; model = specs/r.md by reference)
- [ ] py-arrow — frames via pyarrow (twin of r-arrow; nearer —
      pyarrow is first-class)

## okay-r (specs/r.md — R as a handler)
- [ ] r-subprocess — stage 0: the module, REval/RValue/RFrame,
      Rscript engine (CBOR/JSON wire, clean env), verify(packages),
      condition-as-data, dead-process-throws; Durable-replay test
- [ ] r-rserve — stage 1: the served engine (Java client behind a
      trait; own QAP1 over Async later if named); two-engine
      acceptance
- [ ] r-arrow — frames as Arrow files/streams once the JSON-frame
      road hurts

## The data landscape (specs/data.md — umbrella; vendor = seam impl)
- [ ] lake-delta — Delta specifics: DuckDB delta extension via the
      JDBC seam (read), Delta Kernel interop (read/write, no
      Spark), spark-bridge writes already available (specs/data.md)

## okay-cache (specs/cache.md)

## okay-jdbc (specs/jdbc.md — the foreign database)
- [ ] sql-r2dbc — the JVM reactive-driver hatch behind Sql (LOW:
      driver availability, not performance — virtual threads
      already cover JDBC-behind-Async)

## okay-conf (specs/conf.md)

## okay-persist (specs/persist.md — staged design; stage 0 landed)
- [ ] persist-raft — RaftStore: consensus as one more control-log
      engine under the unchanged Election machinery (specs/
      consensus.md own-Raft notes; typestate per specs/typestate.md)

## okay-http (sibling's area — coordinate before taking)
- [ ] flaky-port-roulette — the full-matrix port/readiness family,
      one ledger: TestMcpHttp 503 (2026-09-01), TestResumable first
      subscribe, TestHttp first GET 404, and TestWire reading
      literal "HTTP" bytes at its handshake (a foreign server
      answered on the expected port) — all green alone, all under
      parallel suites in one sbt JVM; suspect ephemeral-port reuse
      between a closing listener and a dialing client
- [ ] ui-cmd-flaky — okay.ui.TestCmd fails 1-2 of 3 with a VARYING
      subset run to run (timing in the command loop: "interim screen
      shows", "a press launches", "a throwing command forfeits") —
      REPRODUCED ON PRISTINE MASTER (worktree probe at c20d41a,
      2026-09-01), so it landed flaky; owner: the ui-cmd lane
- [ ] demo-chat-live-budget — TestChatDemo's LIVE tests run a local
      model with munit's default 30s budget; under a full parallel
      matrix the box is compiling everywhere and the call ran 55s —
      timeout, not skip. Owner call: raise the budget matrix-proof
      (the TestRepoAgent precedent: 120s) or gate live tests out of
      the parallel run (flagged 2026-09-01; sqlite driver race fixed
      in ctx-functions' landing)
- [ ] http-flaky-mcphttp — TestMcpHttp "one Serving, three wires"
      answered 503 once in a full-matrix run (2026-09-01); green
      alone and on suite rerun — likely a port/readiness race
      (second sighting, same family: okay-jetty TestResumable
      failed its first subscribe once in a full-matrix run
      2026-09-01, green twice alone — port/readiness race shape)
- [ ] http-streaming-responses — incremental bodies on the NIO and
      Netty backends (Jetty has it); unblocks MCP push there
- [ ] http-post-body-audit — Netty/NIO: do POST bodies reach routes?
      (Jetty's did not — found by mcp-push, fixed there)

## Elsewhere
- [ ] ctx-wiring — handlers-awaiting-environment: factories
      returning `Http ?=> Secrets ?=> Handler[Model]`-shaped values;
      okay-demo adopts first (gate possibly OPEN since
      demo-chat — offered to that lane, room n244)
      (specs/context-functions.md)
- [ ] ctx-reader-bridge — `(A ?=> B) <-> B ! Reader % A`, one
      Conversion each way; GATED: no consumer named
      (specs/context-functions.md)
- [ ] llm-streaming-cut — the OPEN P9 item given its mechanism:
      Cut.guarded installs a named prompt over a streamed
      generation, a validator ABORTS to it (Delim; the doctrine's
      PRIMARY case — cross-boundary exit has no handler
      equivalent); ADDITIVE as an API: the unguarded path stays
      (specs/llm-agentic.md, Streaming validation)
- [ ] logic-named-cut — GATED on a search consumer
      (specs/backtracking.md)
- [ ] r-restarts — GATED twice: on r-subprocess and on a restart
      consumer; the one resumable-capture case (specs/r.md)
- [ ] rag-langchain4j — their EmbeddingStore as a Retrieve handler
      (the other half of the interop sentence; when a consumer
      names a store)

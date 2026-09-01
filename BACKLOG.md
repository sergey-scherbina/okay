# Backlog

## Correctness and the core (specs/sim.md, specs/typestate.md)
- [ ] sim-harness — deterministic concurrency simulation on Delim
      (specs/sim.md; user ask 2026-09-01): every fiber under its
      own Prompt, a seeded scheduler at send/receive points,
      interleavings replayable byte for byte. The argument: three
      real races on 2026-09-01 (runCmd lost answers, DriverManager
      per-classloader, port roulette) were all found by FLAKES.
      First clients: runCmd, Replicated/Election, cache
      single-flight, Channel
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
- [ ] kafka-eos — producer idempotence/transactions on the okay-kafka
      sink, or the at-least-once contract asserted where EOS is off

## okay-cache (specs/cache.md)

## okay-jdbc (specs/jdbc.md — the foreign database)
- [ ] pg-scram-typestate — the SCRAM handshake's order by type;
      FORM decided in specs/typestate.md (the wire lane's doctrine
      home): phase objects vs PState — internals only either way,
      the public API unchanged (user ask, 2026-09-01)

- [ ] sql-pg-copy — COPY through the wire: the bulk-load posture
      (pairs jdbc-bulk-load); the wire already frames it
- [ ] sql-pg-node — the cross-platform transport leg: the same
      protocol from Node (the non-JVM openness acceptance)
- [ ] sql-r2dbc — the JVM reactive-driver hatch behind Sql (LOW:
      driver availability, not performance — virtual threads
      already cover JDBC-behind-Async)

## okay-conf (specs/conf.md)

## okay-persist (specs/persist.md — staged design; stage 0 landed)
- [ ] persist-wire-node — the non-JVM leg: the same documented
      frames from Node (pairs sql-pg-node — one cross-platform
      socket story serves both)
- [ ] persist-wire-repl — replication's calls (replicate-pull,
      promote, produce) join the wire's message enum under the
      handshake version: replicas go remote, machinery unchanged
- [ ] persist-offload — cold segments to the object store via the
      Blob seam (pairs blob-seam; the backup copy helper rides too)
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
- [ ] llm-streaming-cut — the OPEN P9 item given its mechanism:
      Cut.guarded installs a named prompt over a streamed
      generation, a validator ABORTS to it (Delim; the doctrine's
      PRIMARY case — cross-boundary exit has no handler
      equivalent); ADDITIVE as an API: the unguarded path stays
      (specs/llm-agentic.md, Streaming validation)
- [ ] stage-phased3 — one more arity, because the consumer exists:
      the http message shape; same typestate guarantees, PState
      transitions; transduce stays (specs/stage-pipeline.md)
- [ ] http-message-phases — the Nio parser's request-line ->
      headers -> body as phased3, the phase enum gone structurally,
      corpus-agreement asserted; internal refactor, module API
      unmoved (specs/http.md)
- [ ] nav-pop-to-screen — pop to a NAMED screen across untouched
      intermediates, the Scope pattern one level up; the ui lane's
      to take (specs/ui.md)
- [ ] logic-named-cut — GATED on a search consumer
      (specs/backtracking.md)
- [ ] r-restarts — GATED twice: on r-subprocess and on a restart
      consumer; the one resumable-capture case (specs/r.md)
- [ ] obs-durable-overlay — the journal/trace identity join: a
      journaled operation's span carries the entry's identity so a
      replay lays over its original spans (needs a Durable consumer;
      specs/obs.md box stays open until then)
- [ ] rag-langchain4j — their EmbeddingStore as a Retrieve handler
      (the other half of the interop sentence; when a consumer
      names a store)

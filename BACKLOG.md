# Backlog

## Direct style — the roads named by the 2026-09-01 survey (docs/direct-style.md)
- [ ] condition-restart-caps — lexical restarts as capabilities
      (the ctx-prompts pattern applied to Condition.within): a
      given Restart installed by the frame makes invoking a
      nonexistent restart a COMPILE error for in-scope code; the
      dynamic policy menu stays the floor. Named by the 2026-09-01
      conditions-x-direct analysis.
- [ ] condition-typed-signal — type the condition/answer pair
      (signal[A](c: Any) casts today); HowMany.signal.? : Int with
      no annotation. A signature redesign, needs its own spec.
- [ ] demo-direct-showcase — migrate the survey's named spots as the
      worked example: ChatDemo seed/go (the effect-per-element
      pattern), Mcp Client send/receive (protocol sequencing);
      existing tests stay green, both spellings shown in the module
      doc.
- [ ] direct-effect-provide — coloring as POLICY: Effect[G] markers
      are ordinary givens, so provide/providing can install them
      per scope — auto-coloring enabled for one environment, off
      elsewhere. Small: a test + a paragraph in the spec; the
      mechanism already exists.
- [ ] direct-try — try around marks reified into the Throws error
      channel (the v2 road named in specs/direct-macro.md Out of
      scope; Throws.scala is the seam).

## Flakes observed (record → fix loop when they recur)
- [ ] persist-election-replicated-flake — okay.persist
      .TestElectionReplicated errored at suite level on one platform
      under the full matrix (2026-09-01, Errors 1 with 0 failures;
      JVM tests of the same suite green in the same run); second
      platform run printed the header with no tests. Suspect
      platform init under load. TRIAGED 2026-09-01: ran alone on
      JS (3/3) and Native (3/3) — did NOT reproduce. The suite is
      pure and deterministic (MemoryStore + Election + Replicated, a
      manual clock, no threads/IO), so the suite-level error was an
      environmental runner crash under parallel matrix load — the
      same family as the Native-SIGKILL-under-load incidents, not a
      code defect. Leave filed; re-triage only if it recurs with a
      NON-environmental signature.
- [ ] netty-ws-matrix-flake — okay.netty.TestBackends "every WebSocket
      client talks to every WebSocket server" failed once under the
      full sbt test matrix (jetty StaticException: Closed,
      2026-09-01, log: one failure in 12); twice green in isolation
      right after. Suspect load/port timing, not code. Settle by:
      run the suite in a loop under parallel matrix load; if it
      reproduces, isolate per the isolate skill.
      RECURRED 2026-09-01 (queue-shape's full-matrix run: okayNetty
      Test failed under parallel load; okay-persist was the only
      changed module, unrelated) — okayNetty ran GREEN in isolation
      immediately after (all 12, incl. "every WebSocket client talks
      to every WebSocket server"). Third sighting, same load/port
      signature; still environmental. Owner (okay-http/netty lane):
      the fix is the isolate-under-load loop, not a per-run retry.

## Correctness and the core (specs/sim.md, specs/typestate.md)
- [ ] wire-typestate — PState at the protocol seams
      (specs/typestate.md; user ask 2026-09-01): SCRAM's step
      order and PgSql's connection phases as types (out-of-order =
      compile error), the Sql transact protocol for driver authors

## Cross-cutting — the 2026-09-01 audit (specs landed e3b5a74; slugs are implementation)

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

## okay-pg (specs/sql.md — the wire driver)
- [ ] pg-composite-decode — the pg driver decodes COMPOSITE / ROW()
      and ARRAY types instead of handing back raw text (user ask
      2026-09-01, MUST). Today valueOf falls through to
      SqlValue.Text for any non-scalar OID, so ROW(1,'ann')/record
      (oid 2249)/named composites/arrays arrive as "(1,ann,25)" /
      "{a,b}" unparsed. Deliverable: parse the pg text format for
      composites (parens, comma-sep, double-quote escaping of
      members with special chars, NULL as empty) and arrays (braces,
      element typing), surfaced as a structured SqlValue (a Row/Array
      case) with the member OIDs typed via the existing valueOf; a
      live TestPg case over the docker Postgres (ROW, a named
      composite type, an int[]/text[] array, nested, NULLs). Note
      the alternative bridge (row_to_json → Schema) in the spec.

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

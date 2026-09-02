# Backlog

## STM — after stm (2026-09-02, specs/stm.md)
- [ ] stm-ui-close — Ui.scala's closing decision is three atomics
      (pending, unprocessed, upstreamDone) and a maybeClose that
      reads all three; the comment there records the race it once
      lost. Three TRefs and one transaction; the first STM consumer.
- [ ] stm-sessions — McpHttp's session table + pushes fan-out, and
      Fiber's cell/subscribers/cancel, as transactions.
- [ ] stm-orelse — `OrElse(a, b)`: run b when a retries; the
      language grows one node, the handlers one case.
- [ ] stm-js-direct-bench — the direct handler is the JS given by
      construction; price it against tl2 on Node once a JS
      benchmark harness exists.

## Async — after channel-callback (2026-09-02)
- [ ] native-scheduler-pool — the Native Scheduler forks one OS
      thread per fiber (src/main/scala-native/Platform.scala). With
      the callback channel nobody waits in a thread anymore, so a
      fixed-size pool with a task queue (the JVM's Schedulers.pool
      shape) is safe: fibers become cheap on Native. Blocking forms
      (CanBlock) on a pool thread still park it — document, or size
      the pool for it.

## Direct style — the roads named by the 2026-09-01 survey (docs/direct-style.md)
- [ ] direct-try-ctx — `try` inside direct[[X] =>> E ?=> X] (the
      Reader-elimination monad) CRASHES dotty 3.7.4 at erasure
      ("bad adapt for M$proxy2.pure(a)") when a CanTry instance for
      context functions exists (found by the 2026-09-02 audit;
      the instance was withheld so the case is a clean "no CanTry"
      compile error instead). Minimize, report upstream, or emit
      the try's pure branch differently for context-function F.
- [x-landed] condition-typed-reconcile — landed in audit-fixes: Of[A]
      derives the Answers instance (Answers.fromOf), so the two typed
      doors are one door with two spellings (specs/condition.md,
      Typed signals, Reconciled). The two frame overloads verified
      complementary: Restart[V]-capability body + typed recover vs
      the inline direct body + Any recover; unifying them would need
      the macro to accept a beta-redex body — not worth a road.

- [x-resolved] direct-choice-ambiguity — resolved in practice by
      ui-direct's landing: explicit direct[[A] =>> A ! AgentRow]
      with .reflect compiles (ChatDemo.agentTurn on master); Choice
      documents the Monad/MonadPlus overlap in its header. Reopen
      only if the inference form (no type argument) bites a consumer.

## Flakes observed (record → fix loop when they recur)
- [x] demo-live-judgment-flake — FIXED 2026-09-02: `judged` retries
      the whole turn once before asserting in LIVE UNGATED and LIVE
      SEEKER (stochastic judgment — one retry is a quadratic flake
      cut; a consistent failure still fails). 15/15 with the retry.
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
- [ ] mcp-auth-matrix-flake — okay.security.TestMcpAuth "the
      metadata documents are servable without any token" failed once
      under the full matrix (2026-09-02, `java.io.IOException:
      HTTP/1.1 header parser received no bytes` — the client read an
      empty reply from a server the suite had just started); ran
      alone right after: 4/4 green. The port/readiness family
      (flaky-port-roulette); leave filed, fix with that family.
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
      FOURTH sighting 2026-09-01 (pg-sslmode matrix: same test,
      ClosedChannelException, 1 of 12; okay-pg was the only changed
      module) — green in isolation immediately after (4/4). Pattern
      is firmly environmental; escalate to the owner lane for the
      isolate-under-load fix rather than re-triaging per landing.

## Correctness and the core (specs/sim.md, specs/typestate.md)

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

## okay-sql (the neutral seam — Typed/Schema layer)

## okay-pg (specs/sql.md — the wire driver)

## okay-jdbc (specs/jdbc.md — the foreign database)

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
- [x] ui-cmd-flaky — FIXED 2026-09-01 by the unprocessed-counter
      close redesign (the runCmd race: close only when upstream done
      AND pending==0 AND unprocessed==0); TestCmd 3x3 green since.
- [x] demo-chat-live-budget — FIXED 2026-09-01: munitTimeout raised
      to 180s in TestChatDemo (the TestRepoAgent precedent, sized
      for a busy local model under a full matrix).
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

## okay-demo (the showcase lane — specs/demo-chat.md, specs/match.md)
- [ ] demo-streaming-cut — the demo as llm-streaming-cut's first
      consumer: Cut.guarded installs a named prompt over the LIVE
      generation and a validator ABORTS a stream that goes off-policy
      mid-flight (today Cut guards only the token budget). Opens the
      gate on the Elsewhere entry; ADDITIVE — the unguarded path stays.
- [x] demo-ctx-wiring — LANDED 2026-09-02: ChatDemo.handler(budget)
      is `(Transport, Secrets, MatchStore) ?=> Route`; main wires
      Transports.http() + Secrets.env, the test wires a canned wire +
      memory Secrets and runs the LIVE Anthropic.stream path offline;
      offline suites run over a DEAD wire. Closes the Elsewhere gate.
- [ ] demo-market-live — /market subscribes to the SSE inbox and
      updates live as offers/needs/deals land (today: static render
      on page load); attribute-facet filtering while there.
- [ ] demo-deal-timeline — render a deal's negotiation history from
      the log (proposed → responded → settled), each fact with its
      provenance — the append-only + supersede story made visible.
- [ ] demo-sessions — signed sessions over okay-security ES256 JWT:
      the email identity (BadEmail condition already guards entry)
      gets a real confirm-and-sign login instead of trust-the-field;
      cross-channel identity then rides the token. Gated on
      security-es256 landing.
- [ ] demo-mcp-market — expose the market tools (search / assert /
      deal / flow) as an MCP server over okay-http's MCP: any MCP
      client (Claude included) becomes a market participant; the chat
      UI unchanged, the marketplace becomes the shared substrate.
- [ ] demo-two-nodes — two demo processes over one shared durable
      log: Election picks the writer, both serve reads, kill the
      leader and watch the market survive — the persist/Election
      machinery in a consumer-visible showcase. Sized LARGE; take
      only when a distributed demo is named wanted.
- [ ] demo-scenario-editor — scenarios are already data
      (ScenarioDef): a UI page to author one (steps, prompts, deal
      hook), saved through the store, listed by the help command —
      extensibility without touching code, shown not told.
- [ ] demo-en-phrasebook — an English phrasebook beside the Russian
      one in the offline driver, picked per message; makes the
      offline demo shareable outside one language.
- [ ] demo-e2e-browser — a browser-level test of the React UI
      (today's tests hit the HTTP/SSE seam directly, so the React
      layer itself is untested); smallest honest version: build the
      bundle, drive one chat round through a headless browser.
- [ ] demo-embeddings-attr — search-before-create for attributes via
      embeddings instead of substring match (twin of rag-langchain4j;
      an embedding store as Retrieve handler): "разработчик" and
      "программист" should collide BEFORE the registry drifts.
- [ ] demo-package — one-command run: bundle the React build into
      the jar's static assets (+ optionally a Dockerfile); today the
      demo needs sbt and a node dev server side by side.
- [ ] demo-gate-ui — the platform Gate policy (Allow / AfterMatch /
      Withhold) switchable from an admin page per attribute class;
      today it is set in code — the two-gate visibility model is the
      business story, so let a viewer flip it and watch /market react.

## Elsewhere
- [x] ctx-wiring — CLOSED 2026-09-02: the consumer arrived and
      shipped (demo-ctx-wiring — ChatDemo.handler as a
      handlers-awaiting-environment value, genuinely rewired in
      tests) (specs/context-functions.md)
- [ ] ctx-reader-bridge — `(A ?=> B) <-> B ! Reader % A`, one
      Conversion each way; GATED: no consumer named
      (specs/context-functions.md)
- [ ] llm-streaming-cut — the OPEN P9 item given its mechanism:
      Cut.guarded installs a named prompt over a streamed
      generation, a validator ABORTS to it (Delim; the doctrine's
      PRIMARY case — cross-boundary exit has no handler
      equivalent); ADDITIVE as an API: the unguarded path stays
      (specs/llm-agentic.md, Streaming validation; consumer named:
      demo-streaming-cut)
- [ ] logic-named-cut — GATED on a search consumer
      (specs/backtracking.md)
- [ ] r-restarts — GATED twice: on r-subprocess and on a restart
      consumer; the one resumable-capture case (specs/r.md)
- [ ] rag-langchain4j — their EmbeddingStore as a Retrieve handler
      (the other half of the interop sentence; when a consumer
      names a store)

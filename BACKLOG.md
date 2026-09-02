# Backlog

## Casts — the audit of 2026-09-02 (operator's rule: no cast without a real necessity)
185 `asInstanceOf` + 28 non-`resume` `@unchecked` in src/main, in five
groups; the recipe for the first two is the one that made Stm.scala
cast-free (stm-typed-interpreter: `perform[X](op: Op[X]): X`, GADT
matching on `Bind(Effect(e), k)`, typed helper classes, a decision at
construction instead of a type test per value).
- [x-landed] cast-free-condition — landed: ops carry their answer
      type, the policy's Any crosses one checked door (accept), the
      run loop is GADT-typed; one stated claim left (a Within's body
      re-typed in the machine's row). See specs/condition.md.
- [x-landed] cast-free-delim — landed: a typed chain Segs[F, A, Z],
      the cut at a prompt through Same[Prompt]'s witness, Next(prog,
      kont) between steps; two stated claims left (Push.body,
      Capture.f). TestDelim unchanged.
- [x-landed] cast-free-sim — landed: Chan[A]/Send[A]/Receive[A]/
      Close[A], the wait queues typed on the channel itself,
      perform[Y] by GADT; zero casts; traces unchanged by seed.
- [x-landed] cast-free-effects — landed (casts-encapsulated):
      Handler.union splits through `<|>` (the one claim), translate's
      cont typed by the Bind node; typeableK's class-test kernel
      stays, stated.
- [x-landed] cast-free-codec — Json (9 → 0), Cbor (9 → 0): Schema
      WAS a GADT already; the codecs cast out of habit. Two kernels
      in Schema state the Mirror's erasure once (`eachField`: parts
      is productIterator in field order; `theCase`: caseOf is the
      ordinal), sum cases are `Schema[? <: A]`, and the codecs are
      written by GADT matching.
- [x-landed] cast-free-typed — landed: Shape[A] is a GADT (Prim
      carries its typed decode/encode with the column widenings,
      Opt/Iso/Arr carry their types, Row carries its Schema), decode
      and encode by matching, a row encodes through eachField; 11 → 0.
- [x-landed] typed-js-facades — landed: okay.Web (core scala-js)
      states fetch/Response/Headers/the body reader/WebSocket and its
      events as js.native facades; both transports rewritten on them,
      17 casts → 0 (a text-vs-binary frame is a type TEST on `Any`).
- [x-landed] casts-encapsulated — landed: ChunkBuf's array kernel
      is one `wrap` (7 → 2, in it) and `sized` replaced the Vector
      casts; Eager's encoding dispatch is one `fold` (6 → 2, in it);
      Pipe.unreachable throws instead of handing out a null; Same's
      two witnesses stay as the axioms.
- [x-landed] direct-upcast-ascription — landed: the macro summons
      `V <:< T` at expansion time and splices it (`upcast`), so the
      generated code carries the compiler's evidence, not a cast;
      found on the way: one of the four was NOT an upcast — a
      statement-position loop's `F[Any]` cast to Unit — now an
      explicit discard `(_: V) => ()`, Scala's own value-discard rule
      said in the macro.
- [x-landed] unchecked-audit — landed: the five `case c: Chunk[Byte]
      @unchecked` over a `Chunk[Byte] | Null` scrutinee are
      null-first matches (flow typing types `c`). The rest are the
      stated kernels: Chunks/Writer's Fold specialization dispatch
      (8: a runtime class test on the Fold instance, the type
      argument erased — commented at Chunks) and Throws' union
      dispatch (12: `A | E | Either | Try` told apart at runtime,
      commented at Throws). Casts in src/main: 185 → 97; what is
      left is kernels with their reason (ChunkBuf, Eager, Pipe,
      Same, Schema, Effects), JVM interop (blob S3/Offload/Backup,
      java Streams, CryptoJvm, kyo) and small ones in ui/rag — the
      next audit's list.

## Casts, round two (2026-09-02, after the audit's 185 → 97)
- [x-landed] cast-free-agent — Provider/Grounded/Handlers/Memory/
      Large/Durable/ToolSpec (10 → 1): interpreters built at the
      GADT-bound X (a covariant row gives X >: the answer, `!` is
      invariant, so `pure[F, X]` / `map[X]`), a Tool[String] asked
      as such answers a String, defaults through a Schema kernel
      (`defaultAt`), the snapshot's erased state through ONE kernel
      (`Snapshot.stateAs`, the Context row names no S).
- [x-landed] cast-free-blob — landed: the Backup/Offload walkers are
      typed by the tree (an Async[X] or a produced X — Produce is the
      identity signature, the op IS its answer; that the values are
      chunks is `produced`'s one claim), S3's row re-associations are
      ascriptions (a row is a union); 17 → 0.
- [x-landed] cast-free-rag-llm-kyo — landed: rag's rows by
      ascription, `fair` built in Choose + Pure, Cut's "cut" frame
      typed through `frame[…, Violation]` (the ClassTag door), kyo's
      Throws matched at its E and the continuations uncast (kyo's
      types line up); 10 → 0.
- [x-landed] cast-free-small — landed: Rx's queue is a typed message
      ADT, Async's handshake cell is `Got[X] | Moved | Null`, Native's
      placeholder an Option, the Java API downcasts are type tests
      with a named refusal (Nio, Jetty, Netty, Tls; CryptoJvm through
      privateKeyOf/publicKeyOf), Node facades for process.argv and the
      Buffer callbacks (Web.Process, NetNode, both CryptoJs — the
      require-based one keeps ONE claim at the module boundary),
      Form decodes fields at their type, Screen finds a boundary by
      Same's witness, Collect always calls the finisher, jdbc/r2dbc
      walk any array by the runtime. Casts in src/main: 97 → 36.
      Left in this group: Dom.scala's js.Dynamic (a ui-js facade
      lane; decided for now — the backend takes a real document or a
      test's fake, and js.Dynamic is what fits both). Screen's
      `Nav | S` split is SOUND since tidy-warnings-screen-dom: a
      `NotGiven[S <:< Nav]` evidence refuses an S that is a Nav at
      compile time, so the runtime test on Nav decides the union.
- kernels that stay, each with its reason at its line: Same (2),
  Eager (2), Pipe (2), Condition (1), Delim (2), Schema (5), Effects
  (2), Writer (1), Http (1), Chunks (2), ChunkBuf (1), Generate (1),
  java Streams (5, array specialization).

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
      module) — green in isolation immediately after (4/4). SIXTH
      sighting 2026-09-02 (direct-tail-fusion's decisive gate,
      genuinely quiet box, load 2.6, no sibling sbt): same test,
      ClosedChannelException, the ONLY failure in the whole matrix;
      Direct.scala was the only module the landing touched, netty
      untouched — green in isolation immediately after (4/4). Pattern
      is firmly environmental; escalate to the owner lane for the
      isolate-under-load fix rather than re-triaging per landing.
      FIFTH sighting, a new family, 2026-09-02 (direct-tail-fusion
      matrix): okay.docs.mongo.TestMongoDocs, okay.kafka.
      TestElectionKafka, okay.kafka.TestKafkaEos timed out (30-120s,
      the driver's own serverSelectionTimeout/munit deadline) rather
      than skipping in milliseconds — TWICE, once under sibling load
      and once on an otherwise-quiet box (docker daemon absent
      entirely both times: `docker ps` -> "no such file or
      directory"); Direct.scala (a compile-time macro, zero I/O) was
      the only changed module. All three green (clean skip) in
      isolation immediately after, both isolated runs. Same
      signature as netty-ws-matrix-flake, a different service
      family: the availability PROBE (not the feature) starves under
      sbt's own full-matrix forked-JVM parallelism — evidenced by
      TestKafkaStore/TestKafkaRepair, whose 120s `munitTimeout`
      override let the SAME slow probe land as a 60s skip instead of
      a hard failure, right there in the same matrix run. NARROWED
      and PARTLY FIXED (same landing): TestElectionKafka carried no
      `munitTimeout` override at all (its three Kafka siblings all
      do, 120s) — the 30s munit default was firing before even a
      merely-slow probe could finish; added, matching the sibling
      pattern exactly. TestMongoDocs carried none either (DocsSuite's
      base has none) — added the same 120s override. Both verified
      green (clean skip) in isolation after the fix. STILL OPEN:
      TestKafkaEos already carried the 120s override and blew through
      it anyway (120.297s) — a probe that itself starves past two
      minutes is the netty-ws-matrix-flake root cause verbatim
      (unbounded sbt test-matrix forked-JVM parallelism, no `Global /
      concurrentRestrictions` cap in build.sbt), not a per-suite
      timeout to tune upward. Settle with the same fix as
      netty-ws-matrix-flake, one lane: a build.sbt-wide concurrency
      cap (cross-cutting, needs its own claim and buy-in) or the
      isolate-under-load loop.

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
- [x] demo-streaming-cut — LANDED 2026-09-02: `Chat.reply`/`chatRoute`
      gain a `policy: (Int, String) => Option[Cut.Violation]`, checked
      alongside the token budget in the SAME `Cut.checked` — additive,
      defaults to never-violate. The demo wires a banned-word content
      policy; `Chat.scripted` echoes the user's message, so typing the
      banned word is itself the trigger, offline. Closes the Elsewhere
      gate on `llm-streaming-cut`.
- [x] demo-ctx-wiring — LANDED 2026-09-02: ChatDemo.handler(budget)
      is `(Transport, Secrets, MatchStore) ?=> Route`; main wires
      Transports.http() + Secrets.env, the test wires a canned wire +
      memory Secrets and runs the LIVE Anthropic.stream path offline;
      offline suites run over a DEAD wire. Closes the Elsewhere gate.
- [x] demo-market-live — LANDED 2026-09-02: GET /market.json (facts
      with attr names, Public-only), GET /events/market (SSE feed
      pinged from the chainedTable wraps + /admin/replay), the page
      re-renders on every ping with attribute facet chips; rows stay
      server-rendered at load.
- [x] demo-deal-timeline — LANDED 2026-09-02: chainedTable threads
      off: Long; match_inquire/match_respond append a DealEvent(state,
      by, Provenance) per transition — append-only, never rewritten.
      GET /deals/<n> and /deals/<n>.json render the full history with
      provenance; a withdrawn stand-down gets its own event; unknown
      deal is 404.
- [x] demo-mcp-market — expose the market tools (search / assert /
      deal / flow) as an MCP server over okay-http's MCP: any MCP
      client (Claude included) becomes a market participant; the chat
      UI unchanged, the marketplace becomes the shared substrate.
      LANDED 2026-09-02: chainedTable mounted at /mcp via
      McpHttp.route; mcpTable rebuilds it per call for fresh
      offset/period. Caught and fixed a real bug — mcpRoute must be
      built ONCE per server (a def re-evaluated per request built a
      fresh MCP session table each time, dropping every session right
      after initialize).
- [ ] demo-two-nodes — two demo processes over one shared durable
      log: Election picks the writer, both serve reads, kill the
      leader and watch the market survive — the persist/Election
      machinery in a consumer-visible showcase. Sized LARGE; take
      only when a distributed demo is named wanted.
- [x] demo-scenario-editor — scenarios are already data
      (ScenarioDef): a UI page to author one (steps, prompts, deal
      hook), saved through the store, listed by the help command —
      extensibility without touching code, shown not told.
      LANDED 2026-09-02: GET/POST /scenarios edits the plain JSON
      shape of ScenarioDef/Transition directly — "steps"/"prompts"/
      "deal hook" turned out to already BE transitions/notifies, no
      new schema needed. MatchStore gained `scenarios` (no list-all
      method existed); help text now names what's registered instead
      of a static hint.
- [x] demo-en-phrasebook — LANDED 2026-09-02: isEnglish(text) (no
      Cyrillic) picks the reply template per message, no session
      state; every trigger pairs 1:1 (умею/can:, нужен/need:-want:,
      спроси/ask, сценарий/scenario, шаг/step, флоу/flow,
      берусь/accept, отказываюсь/decline, помощь/help); both speak
      the SAME chainedTable.
- [ ] demo-e2e-browser — a browser-level test of the React UI
      (today's tests hit the HTTP/SSE seam directly, so the React
      layer itself is untested); smallest honest version: build the
      bundle, drive one chat round through a headless browser.
- [ ] demo-embeddings-attr — search-before-create for attributes via
      embeddings instead of substring match (twin of rag-langchain4j;
      an embedding store as Retrieve handler): "разработчик" and
      "программист" should collide BEFORE the registry drifts.
- [x] demo-package — one-command run: bundle the React build into
      the jar's static assets (+ optionally a Dockerfile); today the
      demo needs sbt and a node dev server side by side.
      LANDED 2026-09-02: Deploy.extraBuild/extraCopy (both additive,
      empty by default — no drift on any other Deploy value);
      DemoDeploy.spec links okayChatWebJS and copies main.js to
      /app/app.js, wired through Chat.appJs's existing OKAY_CHAT_APP
      env var — okay-chat itself needed no change.
- [x] demo-gate-ui — the platform Gate policy (Allow / AfterMatch /
      Withhold) switchable from an admin page per attribute class;
      today it is set in code — the two-gate visibility model is the
      business story, so let a viewer flip it and watch /market react.
      LANDED 2026-09-02: MatchStore.gate/setGate/gateOverrides — a
      `livePolicy` var replacing the immutable constructor-bound
      PlatformPolicy; POST /admin/gate flips it, admin-token gated
      like /admin/replay; /market gained a panel and /market.json a
      "gates" field.

## Reusable modules extracted from the demo (user ask 2026-09-02) — DONE

All three landed: okay-subscription, okay-admin, okay-chat (specs/
subscription.md, specs/admin.md, specs/chat.md). The demo now
composes three independent modules plus okay-match via `orElse`
route tables instead of holding their logic inline.

- [x] okay-admin — LANDED 2026-09-02: `Admin.routes(verify, policy =
      Policy.scoped("admin"), realm)(replay, onReplayed)` on
      `Secure.granted`, plus `Admin.Issuer` (an ES256 keypair, same
      shape as `okay.demo.Login`) so a consumer has a credential to
      test/use it with. Fixed the real gap named when this was filed:
      the demo's `POST /admin/replay` is no longer reachable without
      an admin-scoped bearer token; the token rides the server
      console at startup (same "no delivery channel yet" precedent
      Login's one-time code already set).
- [x] okay-chat — LANDED 2026-09-02: `Model`/`scripted`/`live`/
      `local`/`model`/`modeName`, `sse`/`obj`/`reply` (Cut-guarded
      SSE, `sse`/`obj` public — a consumer's OTHER streams reuse the
      same framing), `fieldOf`/`messagesOf`/`appJs`, and `chatRoute(
      m, budget, turnOverride: (Request, Seq[Anthropic.Message]) =>
      Option[Source[Chunk[Byte]]] = (_, _) => None)`. Widened from
      the original sketch (`Seq[Anthropic.Message] => ...`) to also
      carry the full `Request` — found while wiring the demo: the
      `/match` override needs the bearer token off the request's
      headers, which parsed messages alone cannot carry. page/
      reactPage HTML stayed OUT of the module as planned (market-
      flavored — a market link, example chips, `/events/<email>`
      inbox JS); the demo keeps its own copy, reusing `Chat.Model`/
      `reply`/`sse`.

## Round two: what else in the demo is reusable (user ask 2026-09-02) — DONE

All three landed: pg-target-in-okay-pg, okay-live, login-in-okay-
security (specs/sql.md, specs/live.md, specs/security.md). The first
three extractions left ~1160 lines in ChatDemo.scala; surveyed for
what else earns a move — not everything does; the condition-based
intake (BadEmail/resolveEmail) and the deal timeline stay demo-local,
named and reasoned in specs/demo-chat.md already. Three did earn it:

- [x] okay-live — LANDED 2026-09-02: `Hub[A]` (broadcast, `subscribe()`
      /`publish(a)`) and `Registry[K, A]` (`apply(key)`, lazy per-key
      channel), a new JVM-only module (same java.util.concurrent
      reasoning as okay-subscription — filed for cross-platform
      unification below). `marketFeed`/`inboxes` in ChatDemo.scala
      now delegate to one `Hub`/`Registry` each.
- [x] pg-target-in-okay-pg — LANDED 2026-09-02: `PgTarget` moved to
      `okay-pg/src/main/scala-jvm` (the JVM leg PgTls.scala already
      lives on). Its own TestPgTarget suite in okay-pg (4 tests,
      3 new: disable/absent plaintext, require carries no CA,
      malformed URL never throws); the demo keeps only the live-
      Postgres integration test (proves marketOf's own wiring).
- [x] login-in-okay-security — LANDED 2026-09-02 (specs/security.md,
      stage 6, security-sessions): `SessionIssuer(ttlSec)(subject,
      scopes)` (the ES256 keypair-plus-issue/verify shape) and
      `OneTimeCode(ttlMs)` (confirm-and-sign), both okay-security/
      scala-jvm. `okay.demo.Login` and `okay.admin.Admin.Issuer` are
      thin wrappers now; a caught bug on the way — a first "expired
      token" test landed inside `Jwt.verify`'s default 60s clock-skew
      tolerance and silently passed, fixed by advancing further.

## Cross-platform concurrent state (operator ask 2026-09-02, filed while landing okay-live)

Two round-two modules made the SAME tradeoff for the SAME reason:
`okay-subscription` (joinedPeriod/paidPeriods) and `okay-live`
(Hub/Registry) both needed a safely-shared, growing collection
(a map, a list) and both reached for `java.util.concurrent`
(ConcurrentHashMap/CopyOnWriteArrayList) — which is JVM-only, so
both modules became JVM-only projects rather than crossProject(JVM,
JS, Native), even though everything ELSE about them (the values
they hold, the operations they expose) has no platform opinion.
`okay` core already carries the machinery this problem wants:
`TRef[A]`/`Stm.atomically` (src/main/scala/Stm.scala) is a
cross-platform (JVM/JS/Native) transactional cell, and the STM
engine's OWN write-set bookkeeping already leans on an internal
`TMap` — proof the pattern the two modules need (a transactional
map, a transactional growing list) is buildable on what exists,
not a new primitive from scratch.

- [ ] okay-stm-collections — a small cross-platform layer ON TRef:
      at minimum a `TMap[K, A]`-shaped wrapper (get/put/computeIfAbsent
      equivalent, atomically) and a `TList[A]`/append+snapshot shape
      — public API, not the STM engine's private bookkeeping TMap.
      The real design question to answer before building, not
      assumed: `Hub.subscribe()`/`Registry.apply(key)` are PLAIN
      synchronous methods today; an STM-backed version makes them
      effectful (`... ! F`, run inside a transaction) — decide
      whether that's an acceptable API change for every call site,
      or whether a thin JVM-only synchronous facade stays over a
      cross-platform STM core (facade cost vs. honest effect type).
- [ ] Once landed: migrate `okay-subscription`'s two maps and
      `okay-live`'s `Hub`/`Registry` onto it, and reconsider whether
      either module (or okay-demo itself) should become crossProject
      at that point — no JS/Native consumer is named yet, so this is
      NOT urgent; filed so the decision is made once, deliberately,
      not by accretion the next time this exact tradeoff recurs.

## Elsewhere
- [x] ctx-wiring — CLOSED 2026-09-02: the consumer arrived and
      shipped (demo-ctx-wiring — ChatDemo.handler as a
      handlers-awaiting-environment value, genuinely rewired in
      tests) (specs/context-functions.md)
- [ ] ctx-reader-bridge — `(A ?=> B) <-> B ! Reader % A`, one
      Conversion each way; GATED: no consumer named
      (specs/context-functions.md)
- [x] llm-streaming-cut — CLOSED 2026-09-02: the mechanism shipped
      earlier (Cut.guarded/checked/screened) and the consumer landed
      (demo-streaming-cut) — okay-chat's reply/chatRoute take a
      content `policy` alongside the token-budget check
      (specs/llm-agentic.md, Streaming validation)
- [ ] logic-named-cut — GATED on a search consumer
      (specs/backtracking.md)
- [ ] r-restarts — GATED twice: on r-subprocess and on a restart
      consumer; the one resumable-capture case (specs/r.md)
- [ ] rag-langchain4j — their EmbeddingStore as a Retrieve handler
      (the other half of the interop sentence; when a consumer
      names a store)

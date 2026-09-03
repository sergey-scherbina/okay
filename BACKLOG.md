# Backlog

## Benchmarks — after kyo-fair-lanes (2026-09-02, docs/benchmarks.md §2/§5/§7)
- [x] test-login-tamper-flake — `TestLogin`'s "a tampered token is
      refused" builds its tamper as `token.dropRight(2) + "xx"`, which
      is the SAME token whenever the JWT happens to end in `xx`. Base64
      url alphabet, so roughly 1 in 4096 runs fails a merge gate for
      nobody's mistake. FOUND 2026-09-03 during http-peer-address's
      gate; reproduced by running the suite alone (3/3 clean), so the
      failure is the data and not the code. Fix: tamper by flipping a
      character to one it is not, e.g. `init :+ (if last == 'x' then
      'y' else 'x')`.
      LANDED 2026-09-03 — and the suggested recipe above was ITSELF
      flawed: flipping the LAST char flaked worse (~40% of runs, not
      1-in-4096), because a 64-byte ES256 signature's base64url tail
      char carries only 2 significant bits (4 are decoder-ignored
      padding) — many flips there decode to the SAME bytes and still
      verify. Fixed by flipping a MIDDLE character instead (always
      inside a fully-significant 6-bit block); 0/50 stress runs clean.

- [x] okay-script-scalac-classpath — `okayScript/test` fails 5/7 on
      master itself, unrelated to any branch: `summonFrom` not found in
      `scala.compiletime`, and `NoSymbol cannot be cast to ClassSymbol`
      in dotc's Namer/Typer. Reproduces identically on master and on a
      fresh worktree, so it is an environment/toolchain break (a JDK or
      dotty version drift most likely), not a code regression. FOUND
      2026-09-03 while gating json-unicode-escape; that claim's own
      suites (okayCodecJVM, okayHttp*, okayMatch*, TestTelegram-style
      consumers) were unaffected and green.
      LANDED 2026-09-03 (okay-script-runtime) — not a toolchain drift:
      `okay-script`'s own `build.sbt` block never set `Test / fork :=
      true` (every other project in this build does), so its tests ran
      INSIDE SBT'S OWN JVM. `ScalaScript.run` built the compile
      classpath from `System.getProperty("java.class.path")`, and in
      that un-forked JVM that property is just `sbt-launch.jar`'s own
      path — sbt manages its real classpath through its own layered
      classloaders, invisible to that property — so dotc compiled
      against a classpath with no scala-library on it at all and
      crashed resolving `scala.Int`. Confirmed by printing the
      property from inside the failing test JVM. Fixed by adding
      `Test / fork := true`; `okayScript/test` went 5 failures -> 0
      before any further change. specs/okay-script.md.

- [ ] chunked-source-sweep — one same-session StreamOps run with every
      library's CHUNKED source (fs2 `Stream.range`, `ZStream.range`,
      kyo `Stream.range`, Okay Chunks) next to the per-element lanes;
      today only kyo's chunked lane exists and the §5 table mixes
      sessions with a ratio-to-floor caveat.
- [ ] shape-check-new-lanes — every new competitor lane built by
      foldLeft gets a right-nested twin before its number is quoted
      (the kyo Env/Emit/Resource lesson: the foldLeft shape is O(N²) in
      kyo, ~1000x, and read as the library's price for a week).

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
- [ ] stm-js-direct-bench — the direct handler is the JS given by
      construction; price it against tl2 on Node once a JS
      benchmark harness exists.

## Async — after channel-callback (2026-09-02)
- [x] native-scheduler-pool — the Native Scheduler forks one OS
      thread per fiber (src/main/scala-native/Platform.scala). With
      the callback channel nobody waits in a thread anymore, so a
      fixed-size pool with a task queue (the JVM's Schedulers.pool
      shape) is safe: fibers become cheap on Native. Blocking forms
      (CanBlock) on a pool thread still park it — document, or size
      the pool for it.
      LANDED 2026-09-03: Schedulers.pool(size), hand-rolled queue (no
      java.util.concurrent assumed on Native's javalib);
      Schedulers.threads keeps today's behavior and stays the
      DEFAULT — a blocking workload on a shared pool can starve it,
      so pool is opt-in, sized per workload. cancel() best-effort,
      tracked per-task so a stale cancel never hits a later task on
      the same worker.

## Direct style — the roads named by the 2026-09-01 survey (docs/direct-style.md)
- [x-landed] direct-try-ctx — `try` inside direct[[X] =>> E ?=> X] (the
      Reader-elimination monad) CRASHES dotty 3.7.4 at erasure
      ("bad adapt for M$proxy2.pure(a)") when a CanTry instance for
      context functions exists (found by the 2026-09-02 audit;
      the instance was withheld so the case is a clean "no CanTry"
      compile error instead). Minimize, report upstream, or emit
      the try's pure branch differently for context-function F.
      LANDED 2026-09-03: the crashing shape reused CanTry.strict
      verbatim, which was also the WRONG semantics (a context
      function is a closure — constructing it never runs the body,
      only applying it does, so a strict try never sees a throw
      from inside). `ctxFn` defers the try to APPLICATION time
      instead (the honest counterpart to the Free row's per-step
      guard) — different generated code, no crash, no version bump.
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
- [x] chunk-size-representation — SETTLED, and the premise was wrong
      (2026-09-03). The suspected cause (Vector-then-ArraySeq per
      chunk) was tried: filling a `ChunkBuf` in `Stage.chunked`
      measured 11% better at chunk 256 and 8% at 1024 while 2.2%
      WORSE at the default 16, bars non-overlapping — helping only
      sizes nobody uses. Declined and reverted. The real reading is
      that the curve was never a chunking defect: it compared our
      per-element `Source`, chunked after the fact, against a stream
      chunked by construction. Against the like-for-like pair — okay
      `Chunks.merge` 23.2us against ZIO `ZStream.merge` 58.6 on
      2x2000 — okay is 2.5x AHEAD. docs/benchmarks.md §6b.

- [x] nio-port-scope-flake — SETTLED, and the timing was not what the
      name says (nio-port-scope, 2026-09-03). The assertion took the
      ephemeral port its listener had been given, closed the scope,
      and required a connect to it to FAIL. Under the full matrix that
      is not a fact about our Resource: the port returns to the
      ephemeral pool the instant we release it, a sibling suite binds
      it, and the connect reaches THEIR listener and succeeds — so the
      test reported "the listener outlived its Resource scope" about a
      listener that closed exactly on time. The claim is about the
      listener, so it is now asked of the listener: `Nio.listen`'s
      resource value IS the ServerSocketChannel, and `isOpen` answers
      deterministically, with no port and no neighbours in it. Every
      suite that BINDS a real port (14 of them, found by survey rather
      than by waiting for each to flake) is also Live-tagged now.

- [ ] channel-impls — implementations behind the `Channel` seam
      (channel-seam landed the interface; `StmChannel` is the default
      and unchanged). Each is its own lane, each measured against the
      others AND against `zio.Queue` on the same harness, because the
      point of the seam is that none of them is simply better:

      * RingChannel (bounded, mutable) — `Ring` is already landed and
        measured at 3.4x the rebuild model. Needs the claim-not-remove
        waiter protocol from channel-ring-integration; that lane's
        diagnosis is the starting point.
      * an UNBOUNDED implementation, for the capacity
        `Channel.merge` actually defaults to — see
        channel-ring-unbounded, which is the same work: linked ring
        SEGMENTS. Not a separate Michael-Scott lane, because a
        segmented ring dominates MS on every axis (allocation
        amortised over a segment rather than a node per element,
        cache-friendly, no per-element node) and is the Segmented
        Queue construction Koch-Sanders-Williams 2025 SS3 surveys.
        How it differs from `StmChannel`, so the lane need not
        re-derive it: `StmChannel` costs THREE allocations per send
        (a cons cell, a `Queue`, a `State`) and CASes the whole
        six-field state, so a concurrent operation on an unrelated
        field forces the entire transition to re-run — measured at
        28-49% CAS-failure rates in channel-cas-contention. A
        segmented ring allocates once per segment and contends only
        on the head and tail positions.
      * RelaxedChannel / MultiFifoChannel — Koch, Sanders & Williams
        (arXiv:2507.22764). An order of magnitude at p=32..192, at
        the price of bounded rank error, so ONLY for a channel whose
        consumer accepts relaxed order — not for `merge`, whose
        per-source order `TestChunkEdges` asserts. See
        channel-multififo-many-producers for when it applies.

      The comparison harness belongs to the first of these to land,
      parameterised over the implementation rather than written per
      lane.

- [ ] channel-ring-integration — wire `Ring` into `Channel`. The ring
      itself is landed, tested (MPMC and SPSC on real threads) and
      measured at 3.4x the rebuild-per-operation model it replaces
      (channel-ring), which matches the 3.6x gap to `zio.Queue`. The
      integration was ATTEMPTED in that lane and reverted, with the
      bug found and understood rather than left as "it hung":

      THE BUG — the take-do-put-back window. The first protocol had
      `deliverToReceiver` temporarily REMOVE a waiter from the queue
      to try a `pop`, putting it back if the ring turned out empty.
      A producer pushing during that window sees an empty waiter
      queue, wakes nobody, and the element sits in the ring with the
      receiver parked forever. Reproduced deterministically by the
      existing `TestChannel` "producer/consumer/close accounting"
      test (capacity 4, 200 rounds); the virtual-thread dump shows
      the consumer parked in `receiveBlocking` on a callback that is
      never invoked. `admitOneSender` has the identical hole.

      THE FIX — claim, do not remove. Keep the waiter in the queue
      and give it a one-shot `AtomicBoolean`; a deliverer takes the
      ELEMENT first, then CASes a waiter's claim, retrying the next
      if that one was already claimed. The element is in hand the
      whole time, so nothing can be stranded. Needs the waiter
      representation changed on both sides (`receivers` and
      `senders`), which is why it is its own lane rather than a
      patch. Also still to settle there: `close` must hand the end to
      claimed-but-undelivered waiters, and `receiveManyRing`'s batch
      must admit parked senders without reopening the same window.

- [ ] channel-ring-unbounded — channel-ring gives the allocation-free
      fast path to BOUNDED channels only (a ring is a fixed array).
      `Channel.merge`'s own default capacity is `Int.MaxValue`, so it
      does NOT get the fast path; `Source.merge` (64) and
      `Channel.buffer(n)` do. Options, in the order they look
      sensible: linked ring SEGMENTS (covers everything, the
      Segmented-Queue shape from Afek et al. that Koch-Sanders-
      Williams 2025 SS3 surveys); or change `Channel.merge`'s default
      to bounded (an API decision, not a performance one); or leave
      unbounded on the rebuild path permanently. Settle by measuring
      whether the unbounded path matters in practice first — nothing
      in the library defaults to it except `Channel.merge` itself.

- [ ] channel-multififo-many-producers — if a channel ever has MANY
      producer fibers (work distribution to p workers), head/tail
      contention becomes the bottleneck the ring does not solve, and
      the known answer is relaxed multi-subqueue FIFO (MultiFIFO /
      BlockFIFO, Koch, Sanders & Williams, "BlockFIFO & MultiFIFO:
      Scalable Relaxed Queues", arXiv:2507.22764, an order of
      magnitude at p=32..192). NOT applicable today and deliberately
      not taken: their gain needs many threads (at p=1-2 all designs
      are within a small factor, their Fig. 6.2) and it costs RELAXED
      ordering — elements come out with bounded rank error — while
      `Channel` promises FIFO and TestChunkEdges asserts each
      source's own order survives a merge. Revisit only if a
      many-producer channel appears AND its consumer can accept
      relaxed order.

- [x] netty-ws-matrix-flake — SETTLED by moving it out of the gate
      (netty-integration, 2026-09-03, operator decision). It failed
      the default gate a second time with the identical signature
      (jetty StaticException: Closed, one in 12) and was green in
      isolation immediately after, both times — which is the evidence
      the settle-plan asked for, pointing at load/port timing rather
      than code. okay-netty's suites are now Live-tagged and run under
      `sbt integrationTest`, per AGENTS.md's no-flaky-in-the-default-
      gate policy. Investigating the timing itself remains open, but
      no longer at the cost of every landing's gate.

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-codec
- [x] json-value-parser — landed: JsonValue.parse, a strict
      recursive-descent parser yielding to the lossless CST parser on
      any doubt; Json.parseValue wires it in. 61x over Json.parse on
      the fixture, 2.0x faster than circe's own parser; end to end
      with the staged decoder, 2.3x faster than circe's fused
      parse+decode. specs/codecs.md, "Value parser".
- [x] staged-cbor — landed: Staged.cbor[A], sharing the Reflect base
      with Staged.json; Cbor.scala's Out/In made public so both the
      fold and the staged generator call the same primitives. Encode
      1.6x, decode 2.0x over the interpreted fold. Named by
      okay-persist's own wire path. specs/codecs.md, "Staged CBOR".
- [ ] staged-runtime — `scala.quoted.staging` for run-time schemas
      (ToolSpec from a model, Pg composites from the catalog);
      JVM-only opt-in module, compiler dependency; only on a named
      workload (specs/codecs.md, Staged fold mode, Out of scope)

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

## okay-persist (specs/persist.md — staged design; stage 0 landed)
- [ ] persist-raft — RaftStore: consensus as one more control-log
      engine under the unchanged Election machinery (specs/
      consensus.md own-Raft notes; typestate per specs/typestate.md)
      STAGE 0 LANDED 2026-09-03 (operator: "start it anyway," a
      months-scale effort taken as a staged climb, not attempted
      whole): okay.persist.Raft — the pure algorithm core, leader
      election + log replication, seven tests proving election
      safety, log matching, the Figure 8 commit trap.
      STAGE 1a LANDED 2026-09-03: okay.persist.RaftWire.Node — a
      real peer-to-peer wire transport, RaftMsg over real sockets
      (the SAME [len:int32][CBOR] framing Wire.scala uses), real
      wall-clock election timeouts/heartbeats. Three real nodes
      elect a leader, replicate and commit a client entry, and fail
      over on a killed leader — all over an actual network. Stage
      1b (the Store/Topic engine wrapper — the actual RaftStore this
      bullet names — plus persistent currentTerm/votedFor) and stage
      2 (compaction, membership changes) remain open; box stays
      unchecked for those.

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

## okay-demo (the showcase lane — specs/demo-chat.md, specs/match.md) — DONE, all 11 landed
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
- [x] demo-two-nodes — two demo processes over one shared durable
      log: Election picks the writer, both serve reads, kill the
      leader and watch the market survive — the persist/Election
      machinery in a consumer-visible showcase. Sized LARGE; take
      only when a distributed demo is named wanted.
      LANDED 2026-09-02 (named wanted by the operator): TwoNode
      polls a shared OKAY_CHAT_LOG directory (FileStore has no live
      cross-process tailing, stated not hidden), Election picks the
      writer, POST is 503-gated to the leader, GET always serves.
      TestTwoNode launches two REAL OS processes, kills the leader,
      proves the survivor takes over and the market holds.
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
- [x] demo-e2e-browser — a browser-level test of the React UI
      (today's tests hit the HTTP/SSE seam directly, so the React
      layer itself is untested); smallest honest version: build the
      bundle, drive one chat round through a headless browser.
      LANDED 2026-09-02: okay-demo-e2e-browser (Playwright, real
      headless Chromium) — typed text sends, the scripted reply
      streams in via the SAME fetch+ReadableStream glue Main.scala
      ships. Kept OUT of okay-demo's test sourceset and the root
      aggregate (a real ~450MB one-time browser download); invoke
      via `sbt "okayChatWebJS/fastLinkJS" "okayDemoE2eBrowser/test"`.
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

- [x] okay-stm-collections — a small cross-platform layer ON TRef:
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
      LANDED 2026-09-03: `TRef.modify` is ALREADY synchronous, so a
      single-cell dict/list never needs `Tx`/`Stm[F]` at all — no
      facade, the plain shape IS the honest one. Named `TDict` (not
      `TMap`: that name is taken, by exactly the engine bookkeeping
      type this bullet warned about). A 64-thread stress test found
      a real, stated limit: `computeIfAbsent`'s `mk` may run more
      than once under CAS contention (only the winner's value is
      ever stored) — fixed the doc, not hidden.
- [ ] Once landed: migrate `okay-subscription`'s two maps and
      `okay-live`'s `Hub`/`Registry` onto it, and reconsider whether
      either module (or okay-demo itself) should become crossProject
      at that point — no JS/Native consumer is named yet, so this is
      NOT urgent; filed so the decision is made once, deliberately,
      not by accretion the next time this exact tradeoff recurs.
      PARTIAL 2026-09-03: `okay-subscription` migrated, a pure swap
      (9/9 existing tests unchanged). `okay-live`'s `Hub`/`Registry`
      NOT done — the operator's ask named `okay-subscription`
      specifically; box stays unchecked for that half.

## okay-script (specs/okay-script.md) — markdown ```scala fenced blocks as Scala source
- [x] okay-script core — LANDED 2026-09-03: `blocks`/`run`, one .md
      file = one compilation unit (blocks concatenate in document
      order, later blocks see earlier ones' val/def), driven through
      dotty.tools.dotc IN-PROCESS (no scala-cli subprocess, no custom
      language/interpreter). Success = compiles + runs without
      throwing; stdout captured. Investigated ../scalascript first —
      unrelated (a full custom markdown-as-syntax language), nothing
      reusable found, recorded as a negative result in the spec.
- [ ] okay-script: sbt-test / CI integration — a task walking
      `specs/*.md` (or a configured dir), failing the build on the
      first `!ok` Result. Deliberately not built with the core
      (operator: "библиотека/API, без интеграции в sbt test пока").
- [ ] okay-script: mdoc-style output-comparison literate testing — a
      block's expected stdout written inline in the markdown, checked
      against `Result.stdout` from a real run. `run` already captures
      everything needed; the markdown convention for "expected
      output" and the comparison step are the missing piece.
- [ ] okay-script: line-accurate compiler-error mapping back from the
      synthetic wrapped source to the original `.md` file's line
      numbers. `Block.startLine` is captured for this but unused so
      far — a dotc diagnostic's line currently reports against the
      synthetic file, not the markdown.
- [x] okay-script-runtime — LANDED 2026-09-03: the REAL goal named by
      the operator is runtime app generation (generate a `.md`,
      compile+run it AT RUNTIME, come up as a live web app — a
      storefront), not a doc smoke-test; see specs/okay-script.md
      "The real goal". Added `Classpath` (explicit classpath entries,
      `ambient` as a documented-fragile default) and `Deps`
      (`//> using dep "org:artifact:version"`, resolved via the
      `cs`/`coursier` CLI) so a generated script can be handed exactly
      the jars it needs (okay-ui/okay-jetty, an extra library) instead
      of inheriting the host process's own classpath. Also fixed
      okay-script-scalac-classpath above as part of the same pass (the
      bug that made the classpath question concrete, not hypothetical).
- [x] okay-script-lifecycle — LANDED 2026-09-03: the `Server !
      Resource` lifecycle question above is settled, no new
      `ScalaScript` API needed. `Resource.run` releases every acquired
      finalizer on ANY escaping `Throwable` (Resource.scala's `_loop`),
      and `ScalaScript.run` invokes the compiled script synchronously
      on whatever thread called it — so a caller runs `run` on its own
      `Thread` (does not block the generator) and stops the app with
      `Thread.interrupt()` (makes the script's own `Thread.sleep(
      Long.MaxValue)` throw, which `Resource.run` turns into a real
      `server.stop()`, not just an abandoned thread). Proved against a
      REAL `okay-jetty` server in `TestScalaScriptLifecycle` (Live):
      answers HTTP while alive, stops answering after interrupt, the
      returned `Result` carries the `InterruptedException`.
      specs/okay-script.md "Lifecycle".
- [x] okay-script-storefront-example — LANDED 2026-09-03:
      `okay-script/examples/it-consulting-storefront.md` — a real
      `okay-jetty` server (page + `/order/<key>` route), content
      (services/prices) taken verbatim from `../it-consulting/site/
      site.md`, compiled and run end to end through `ScalaScript.run`
      using the proven lifecycle recipe (own `Thread`, `Thread.
      interrupt()` to stop). No `busi`/`scalascript` DSL reused — only
      the data; the page and `/order` handler are ordinary Scala.
      Proved by `TestScalaScriptStorefront` (Live): all five services
      render with prices, `/order/<key>` confirms the right one,
      interrupt stops the server. Found and fixed along the way: the
      example's first cut used a QUERY STRING (`/order?key=<x>`), which
      okay-jetty's `Request.url` silently never carries (`Jetty.scala`'s
      `requestOf` uses `getPathInContext` — path only, no query-string
      field on `okay.http.Request` at all) — see the next entry.
      specs/okay-script.md "Worked example".
- [x] http-request-query — LANDED 2026-09-03: NOT "no query-string
      support at all" as first written — `okay.http.Server` (JDK) and
      `okay-netty` both already carry the query string in `Request.url`
      (`getRequestURI().toString()` / `req.uri` are both full
      request-targets). Only `okay-jetty` was broken: `requestOf` built
      `url` from the static `getPathInContext(req)`, PATH ONLY. Fixed:
      `req.getHttpURI.getPathQuery` instead — a route with no query
      string sees the byte-identical string as before (`HttpURI`'s own
      `getPathQuery` returns the bare path when there is no query), and
      no `ContextHandler` is used anywhere in `Jetty.serve`, so nothing
      about an existing route's matching changes. `TestJetty` gained a
      query-string test; `TestBackends`' cross-backend matrix never
      exercised one either (noted in specs/http-backends.md as the gap
      that let this ship unnoticed). specs/http-backends.md.
- [x] okay-script-classloader-isolation — LANDED 2026-09-03: each
      `run` call already had its OWN `URLClassLoader` (scripts do not
      collide with EACH OTHER), but its parent was
      `getClass.getClassLoader` — `okay-script`'s own defining
      classloader — and `URLClassLoader` is parent-FIRST, so a script
      could silently resolve a class from `okay-script`'s own build
      (munit, in Test scope okay-jetty, ...) regardless of what the
      caller's explicit `Classpath` actually listed — the isolation
      `Classpath`/`Deps` (okay-script-runtime) were built for was not
      actually enforced. Fixed: parent is now
      `ClassLoader.getPlatformClassLoader()` (JDK core only) — a script
      sees exactly its own compiled classes, its own `Classpath`, and
      the JDK. No behavior change for `Classpath.ambient` callers (it
      already lists ~everything). Proved by
      `TestScalaScriptClassloaderIsolation`: a script given a minimal
      `Classpath` can no longer reach `munit.Assertions` (present on
      `okay-script`'s own test classpath, absent from that minimal
      one) — confirmed as a REAL regression check by temporarily
      reverting the fix and watching the test fail before restoring
      it. specs/okay-script.md "Classloader isolation".
- [x] okay-script-interpolation — LANDED 2026-09-03: the operator's
      own framing for `okay-script` — "a new JSP, but Scala+Markdown".
      New `ScalaScript.render(markdown, classpath): Result`, separate
      from `run` (untouched — still for apps/effects like the
      storefront). `render` recognizes `${expr}` in PROSE (outside
      ```scala fences; `$${` escapes to a literal `${`) as a Scala
      expression evaluated in the SAME document-order scope
      ```scala blocks build, `.toString`-printed in place; everything
      else — prose, other-language fences — passes through verbatim.
      Brace-depth- and quote-aware scanner (handles a NESTED real
      `s"${x}"` string interpolation inside an `${...}` marker's own
      expr). The rendered document is `Result.stdout`. Worked example:
      `examples/render-storefront.md`. One design refinement made
      BEFORE any test ran: direct `print(...)` per segment instead of
      a buffer flushed at the end, so a code block's own `println`
      output stays in true document order instead of reordered after
      the whole rendered text. specs/okay-script.md "Interpolation".
- [x] okay-script-page — LANDED 2026-09-03: the HOT-RELOAD half of
      "per-request execution + hot-reload" (the REQUEST-OBJECT
      injection half is still open, see the next entry). New
      `Page(path, classpath)`: compiles a `render`-mode `.md` file
      ONCE, cached by the file's mtime, re-INVOKES (not re-compiles)
      on every `render()` call while the file is unchanged — the
      actual JSP shape (a page's servlet class compiles once, its
      per-request method runs once per request). No new dependency;
      an actual `okay-jetty` route stays glue code a caller writes.
      Split `ScalaScript.compileAndRun` into `compileOnly` (returns an
      invokable `Compiled` handle or a `Result` with compile errors)
      and `Compiled.invoke()` (callable repeatedly). Found and fixed
      along the way: a SECOND `invoke()` on the same compiled program
      silently printed nothing — a real, previously-invisible bug from
      okay-script-classloader-isolation (the isolated script
      classloader loads its OWN separate `scala.Console`, so the
      original host-side `scala.Console.withOut` fix for capturing
      `println` never touched it; it only "worked" for a one-shot call
      by coincidence). Traced to a minimal bare-classloader
      reproduction before writing the fix. Fixed by driving the
      isolated classloader's OWN `Console` via reflection
      (`setOutDirect`) on every `invoke()` — applies to `run`/`render`
      too, though it was invisible there. specs/okay-script.md
      "Hot-reload".
- [x] okay-script-web — LANDED 2026-09-03: the REMAINING half of "a
      new JSP" — a script reading the CURRENT HTTP request (method,
      path, query, headers) the way it already reads `Meta.current`
      for file metadata. Scoped to avoid the dependency this entry
      itself flagged: new `Web` is a plain, dependency-free case class
      (`String`/`Map` only) — no `okay.http.Request` import anywhere
      in `okay-script`'s own code; a caller (an `okay-jetty` route)
      translates its own `Request` into `Web` before calling `render`/
      `Page.render`. `Page.render(web)` sets it FIRST, inside the
      page's existing lock, so concurrent requests never race on which
      request's `Web` a given call sees. Found and fixed along the
      way: `Web` hit the SAME classloader-identity trap
      okay-script-page's Console fix found, one level up, for a
      user-defined type — a host-built `Web` handed directly to the
      isolated script fails reflection's argument-type check (the
      isolated loader compiles its own separate `Web` class). Fixed by
      encoding `Web` into a flat `Array[String]` host-side and
      decoding it back INSIDE the isolated classloader — only
      `String`/`Array[String]` cross the boundary — which meant
      abandoning `@main def okayScriptMain(): Unit` (its generated
      forwarder never hands `args` through when the `@main` method
      itself takes zero parameters, which it always did here) for a
      plain `object OkayScriptMain: def run(args: Array[String]):
      Unit`, confirmed via `javap` before writing the change. That
      wrapper change broke output for EVERY existing example — caught
      immediately by `TestScalaScriptRender`'s own test, not a
      `Web`-specific failure — because the naive fix (re-indent the
      already-built body by prefixing every physical line) corrupted
      DATA inside a `Text` segment's multi-line raw string literal,
      indistinguishable from source formatting to a blind line-prefix
      pass. Fixed by having every body-line producer build lines at
      their FINAL depth directly. Also repeated (and fixed the same
      way as) `hasMeta`'s own self-sufficiency lesson: an unconditional
      `Web` reference broke `TestScalaScriptClassloaderIsolation`'s
      minimal-Classpath case again; `hasWeb` gates it now.
      specs/okay-script.md "Request context".
- [x] okay-script-meta — LANDED 2026-09-03: code inside an .md file
      reads the metadata defined in the markup AROUND it, as its
      current context (operator ask). Front-matter (`---`, file-level)
      plus nested ```yaml fences scoped by heading ancestry — the
      shape `../it-consulting/site/site.md` already uses. New module
      `okay.script.Meta`: a typed AST (`Value`/`Section`/`Doc`) built
      by a minimal YAML-subset parser, plus `Context(doc, path)` with
      untyped `get`/`apply` AND the full typed `doc` — both forms of
      access asked for, through one value, reachable via
      `Meta.current` (a plain always-fresh method, NOT a `given` — see
      the spec's "How code reaches it" for why a `given` genuinely
      does not work for this: confirmed empirically, a plain `given`
      is evaluated once and local re-declaration at the same flat
      scope is a compile error, neither known before this landing
      tested it directly). `run`/`render` emit the `Meta` wiring only
      when a document actually HAS metadata (`hasMeta`), preserving
      self-sufficiency for the common metadata-free case — the first
      cut skipped that check and broke it, caught by
      `TestScalaScriptClassloaderIsolation`'s own minimal-Classpath
      test. A ```yaml fence is now metadata (consumed, not shown in
      `render`'s output) — every other fenced language is unaffected.
      The storefront example now reads its tagline/contact from real
      front-matter instead of a hardcoded second copy.
      specs/okay-script.md "Metadata as context".

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
      PARTIAL 2026-09-02: the EmbeddingModel half landed as
      okay-langchain4j-embed (a local ONNX embedder, String =>
      Embedding + Handler[Embed] — MemoryMatch's exact `embed` seam,
      no okay-rag pipeline needed). Deliberately OUT of okay-demo's
      build and the root aggregate (a real ~90MB model download).
      The EmbeddingStore/VectorStore half named in the title is
      still open — this box stays unchecked for that.

## okay-agent: intent classification — after intent-classify (2026-09-03, specs/intent-classify.md)
- [x] intent-other-collapse — LANDED 2026-09-03. Six arms over the same
      24-message fixture, in-repo as `TestClassifyLive` (Live-tagged):
      the answer is examples + a binary in-domain gate, 0.955 macro F1
      over 23/24 decoded replies with `Other` recall 0.83 at precision
      1.00, against 0.587 and recall 0.00 for the prompt as it was.
      Two things fell out that were not the question: the decode rate
      is a PROMPT property (4 -> 23 of 24 decoded, same model, purely
      on how the answer was asked for — a rendered example beats a
      schema), and a harness sentinel must not enter the confusion
      matrix or macro F1 tracks the decode rate instead of the
      classification. Original entry follows.
- [ ] intent-other-collapse (original) — the lane's own measurement: declaring an
      `Other` case is NOT enough. On 24 labelled messages the local 4B
      model gave `Other` recall 0.17 with reasoning first and 0.00
      without, absorbing every out-of-domain message into a positive
      class (charged twice -> Request, birthday wishes ->
      Notification). Candidates, in the order I would try them: an
      explicit none-of-the-above instruction in the prompt, `Other`
      examples shown in the prompt, and a separate binary in-domain
      gate ahead of the taxonomy. Measure each against the same
      fixture with `Eval.regressions` — this is exactly the loop that
      rule exists for.
- [ ] intent-precedence-rule — `Proposal` vs `Request` confused 3 of 6
      in the same run, and it is genuine overlap rather than model
      error ("Can we move Thursday's sync to Friday?" is both). Needs a
      stated precedence rule travelling WITH the taxonomy (a doc
      comment the prompt renders), not a better classifier.
- [ ] intent-symbolic-tier — an LU dictionary over `Postings`/BM25 as a
      first pass that answers the easy majority without a model call.
      TRIGGER: measurement shows cost or latency binding on the model
      tier. Linagora's ontology system answers in <150ms with no model
      at all, so the tier is not a rudiment — it is just not yet
      justified here by a number.
- [ ] intent-vector-tier — class centroids, then a linear probe over
      frozen embeddings, trained from LLM-distilled labels (keep only
      `Conf.High` plus the human confirmations the `Clarify` path
      produces). 18x1024 weights is 72KB; a cosine at 1536 components
      measured 1.04us in `Store.scala`, so ~18us for 18 classes — the
      "sub-millisecond encoder" tier with no dependency and no training
      pipeline. Needs 30-100 examples per class, not the 1k-5k a
      fine-tuned encoder wants. TRIGGER: the symbolic tier starts
      missing paraphrases.
- [ ] intent-temporal-slots — a `When` slot takes ISO-8601 and refuses
      anything else through `SIso`, so "next thursday" cannot be
      filled. A Duckling-equivalent over `okay-lex`/`okay-parse` is its
      own lane; until it exists the model does the conversion and the
      schema checks it.
- [ ] intent-live-provider — `Classify.prompt`/`read` are tested
      against hand-built and round-tripped values, not a live model.
      The end-to-end run belongs with `TestLive`'s gating, and it is
      what would let `Structured.cut`'s token saving be measured rather
      than reasoned about.
- [ ] intent-eval-on-journal — bind `Eval` to a `Rerun` journal so an
      evaluation run is replayable and a regression names the step that
      changed, not just the class that fell. The spec names this as the
      intended fixture; the lane deliberately did not build it, and the
      seam it needs is only that `Eval` takes label pairs from
      anywhere.
- [ ] intent-domain-in-names — the residue the gate does not catch: one
      of six out-of-domain messages is still absorbed, and the fixture's
      `Other` mixes "not about this at all" (a birthday wish) with
      "another topic in the same register" (a double charge, a
      cancellation). A taxonomy of `Proposal`/`Request`/`Notification`
      with a bare `what: String` never says its domain is meetings — the
      case NAMES carry the domain or nothing does. Try domain-bearing
      case names before adding any prompt machinery.
- [x] intent-fixture-too-small — LANDED 2026-09-03. 120 messages, 30
      per class, domain stated inside the fixture, hard cases marked;
      plus a parallel set of 12 meanings in 6 languages so a language
      effect is attributable instead of anecdotal. The n=24 conclusion
      reproduced (macro F1 0.553 -> 0.906, `Other` F1 0.00 -> 0.86).
      Original entry follows.
- [ ] intent-fixture-too-small (original) — at n=24 a difference of one or two
      replies is not a difference, and a mid-lane wording change moved
      an arm by two. Grow `IntentFixture` past the reference's minimum
      (30 per class) before defending any gap in the arms table as real.
- [ ] intent-gate-non-english — the gate loses PRECISION outside
      English: `Other` precision 1.00 en, 0.75 fr, 0.60 ru, with recall
      1.00 everywhere, so it is pushing genuine in-domain messages out
      rather than failing to catch out-of-domain ones. Opposite
      direction to the English failure. Try stating the domain in the
      gate prompt in the message's own language, or giving the gate the
      same few-shot treatment that fixed the taxonomy prompt; measure
      per language on `IntentFixture.parallel`, not in aggregate.
- [ ] intent-decode-rate-residue — 11 of 120 replies still undecodable
      on the best arm (9%). The rendered example took this from 32% to
      9% and then stopped; what remains has not been looked at, and a
      caller cannot tell a hard message from a malformed reply.

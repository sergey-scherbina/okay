# Backlog

## matrix-kill-by-process-group — one suite takes down every sbt on the box

Found 2026-09-04 while gating an unrelated four-line change. The full
matrix dies with SIGTERM (exit 143) immediately after
`okay.cluster.TestCluster`, and it is not the change, not memory and
not a flaky test:

- a feature branch died three times at exactly 1427 tests;
- CLEAN MASTER, run from the main checkout with no changes at all,
  died the same way at 1897;
- different counts, the SAME suite boundary every time;
- 53% of 36GB free, 5.7GB of java resident — nothing is being
  OOM-killed, and 143 is a TERM rather than a KILL.

`TestCluster`'s two tests are "a killed in-process worker's chunks
recompute" and "a socket worker dies mid-stream". A kill issued to a
process GROUP rather than to a pid does exactly what is observed: it
reaches every sbt on the machine, including a sibling worktree's
matrix minutes from finishing.

This is the mechanism behind the folklore that two concurrent sbt runs
SIGTERM each other here. It is not contention — it is one suite
killing processes it does not own, and the practical cost is that a
green matrix is currently obtainable only by running ALONE, which for
several agents on one box is a serialisation nobody agreed to.

The work: kill by pid, and if a test genuinely needs to signal a
group, give the spawned worker a group of its own
(`setsid`/`ProcessBuilder` with its own session) so the blast radius
stops at the thing under test. Wants a law that a worker-killing test
leaves other JVMs alone — hard to assert directly, but a sentinel
child process that must still be alive at the end of the suite would
catch a regression.

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
- [x] script-temp-snapshot-crosstalk — FIXED 2026-09-05 (same defect as
      the since-removed `script-temp-tests-watch-a-shared-directory`
      entry, filed separately and merged here): `TestScalaScript`'s
      "run: leaves no temp file/directory behind" and `TestPage`'s
      "close() deletes the cached compiled program's temp output
      directory" both snapshotted the SHARED `java.io.tmpdir` for
      `okay-script-*` entries before/after, so either failed whenever
      another concurrent suite (a sibling worktree's own okay-script
      tests, in a parallel matrix) created a matching entry between
      the two snapshots — nothing to do with either test's own
      cleanup. `ScalaScript` now takes an explicit `tempRoot: Path`
      (threaded through `run`/`render`/`compileRender`/`check`/
      `compileOnly` and `Page`), defaulting to the old shared lookup;
      both tests now point it at a private directory and snapshot
      that instead. `TestScalaScript`'s test is back in the default
      gate (dropped the `Live` tag). See specs/okay-script.md
      "okay-script-tests-watch-a-shared-directory".
- [x] demo-live-judgment-flake — FIXED 2026-09-02: `judged` retries
      the whole turn once before asserting in LIVE UNGATED and LIVE
      SEEKER (stochastic judgment — one retry is a quadratic flake
      cut; a consistent failure still fails). 15/15 with the retry.
- [x] persist-election-replicated-flake — SETTLED by exclusion
      (flakes-integration, 2026-09-03, OPERATOR CALL). History:
      okay.persist.TestElectionReplicated errored at suite level on
      one platform under the full matrix (2026-09-01, Errors 1 with 0
      failures; JVM tests of the same suite green in the same run);
      the second platform run printed the header with no tests.
      TRIAGED 2026-09-01: alone on JS (3/3) and Native (3/3) — did
      NOT reproduce. The suite is pure and deterministic (MemoryStore
      + Election + Replicated, a manual clock, no threads, no IO), so
      what failed was the RUNNER under parallel matrix load, the same
      family as the Native-SIGKILL-under-load incidents, not a code
      defect. Now `Live`-tagged with the rest of the recorded flake
      family and run by `sbt integrationTest`. Noted honestly at the
      suite and in specs/integration-test-gate.md: this one is
      excluded by DECISION, not by evidence against the suite — it is
      the only member of the family that touches nothing outside the
      JVM. Re-triage only if it recurs with a non-environmental
      signature, where it will now show up in the integration run.
- [x] mcp-auth-matrix-flake — SETTLED by exclusion (nio-port-scope
      tagged the suite, 2026-09-03; closed here with the rest of the
      family, flakes-integration). okay.security.TestMcpAuth "the
      metadata documents are servable without any token" failed once
      under the full matrix (2026-09-02, `java.io.IOException:
      HTTP/1.1 header parser received no bytes` — the client read an
      empty reply from a server the suite had just started); ran
      alone right after: 4/4 green. It BINDS A REAL PORT, so its red
      can be the machine's rather than the code's: `Live`-tagged, out
      of the default gate, run by `sbt integrationTest`.
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

- [ ] raft-wire-election-flake — okay.persist.TestRaftWire "killing
      the leader: the survivors elect a new one and keep committing"
      failed once under the full sbt matrix (2026-09-03, one gate in
      three), green 3/3 in isolation immediately after. Leader
      election is timeout-driven, so this is the netty-ws-matrix-flake
      / nio-port-scope-flake family: a schedule-sensitive assertion
      under matrix load. Established as NOT caused by the lane that
      observed it (ring-channel): that lane's only edit to existing
      code is an added `Ring.isFull`, everything else is new files
      nothing references, and the Channel factory still returns
      StmChannel -- there is no code path from it to Raft. Settle by
      the survey in AGENTS.md (does it bind ports / depend on
      timeouts?) and either tag it Live or fix the timing.

- [ ] channel-impls-correctness — bring RingChannel and CasChannel
      back, now that channel-laws exists to judge them. They were
      written and measured (casChannel 143.9 +/-16.3 against
      stmChannelUnbounded 187.7 +/-18.2 and zio.Queue 122.2 +/-9.7;
      ringChannel 249.9 +/-31.2 against stmChannel 418.2 +/-95.3) and
      withdrawn because their accounting failed one full gate in
      three. The code is in 83ff8b23.

      What changed: `TestChannelLaws` now states the contract and is
      parameterised over the implementation, and it was PROVEN to
      catch this class of defect -- CasChannel with its in-flight fix
      reverted fails law 1 in 0.05s, naming the law, where the full
      gate needed roughly three runs to show the same thing. And
      `Channel.finished` now asks implementations for the conclusion
      ("nothing further can ever be delivered") rather than letting
      them derive it from a raw flag plus an emptiness check, which
      is the derivation three of the four defects got wrong.

      So: add each implementation to `impls` in TestChannelLaws, make
      the laws pass, and only then land. A fourth defect remains
      undiagnosed in CasChannel -- suspect a double-invocation of the
      continuation (CompletableFuture.complete silently drops the
      second value, which looks exactly like a lost element) or a
      waiter dropped by wakeOne's CAS-and-claim.

- [ ] ring-channel-waiters — (after channel-impls-correctness) the
      ring's waiter protocol measures 1.7x over the bounded default where
      the RING MECHANISM alone measured 3.4x (channel-ring). The
      waiter protocol around it eats more than half the win, and the
      causes are known rather than suspected: the waiter queue is an
      `AtomicReference[List]` walked with `last`/`init` (O(n) per
      wake), a fresh `Waiter` is allocated on every retry iteration
      and purged afterwards, and the close barrier spins. Fix by
      giving the waiters their own lock-free queue (CasChannel's node
      code is right there) and by not re-registering per iteration.
      Measure against the 3.4x mechanism ceiling, not against the
      default.

- [x] channel-ring-integration — SUPERSEDED by channel-seam plus
      ring-channel: the implementation lives behind the interface
      instead of replacing Channel's mechanism in place, which is why
      three real defects could be found without master ever being at
      risk. Original
      diagnosis kept below for the record.

- [x] channel-ring-integration (original) — wire `Ring` into `Channel`. The ring
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
- [x] okay-script-check — LANDED 2026-09-03: mdoc-style literate
      testing — a block's expected stdout, written inline as a NEW
      ```stdout fence, checked against what a real `run` actually
      printed. `ScalaScript.check(markdown, classpath): CheckResult`
      is purely ADDITIVE and host-side — no synthesis changes, no new
      fence recognized by `tokenize`/`withMeta` at all (deliberate,
      right after two landings in a row hit the same re-indentation
      bug shape there). Extracts every ```stdout fence's (trimmed)
      content via a plain line-scanner mirroring `blocks`' own, runs
      the document once via ordinary `run`, then verifies each
      expected chunk appears as an IN-ORDER, non-overlapping substring
      of the actual stdout — proving the right output happened in the
      right relative sequence without injecting a checkpoint into the
      compiled program. All mismatches collected, not just the first;
      a `run` that fails to compile fails `check` immediately with one
      summarizing mismatch. First cut passed all 8 tests on the first
      run — no bug found, unlike the two landings right before it.
      specs/okay-script.md "Output-comparison testing".
- [x] okay-script-line-mapping — LANDED 2026-09-03: a compile error's
      line number now reports the ORIGINAL `.md` file's line, not the
      SYNTHETIC wrapped source dotc actually compiled. `Segment.Code`/
      `Interp` gained a `startLine` (`tokenize` computes it, same
      convention as `Block.startLine`); `withMeta` builds the
      synthesized body PLUS a parallel `Vector[Int]` line-origin map
      (one entry per physical body line, `-1` for injected/synthesized
      lines); `collectingReporter` reads `dia.position()` (confirmed
      0-based empirically, via a throwaway probe, before writing
      anything) and prefixes a mapped diagnostic `"L<n>: "`. A
      multi-line block's error correctly reports ITS OWN line, not
      just the block's first. Found and fixed along the way: the SAME
      bug shape as okay-script-web's `compileOnly` fix, one function
      over — `withMeta`'s first cut indented EVERY physical line of a
      `Text`/`Interp` segment's synthesized `print("""...""")`
      uniformly, corrupting embedded multi-line string DATA the same
      way the earlier fix corrupted it at a different layer;
      `TestScalaScriptRender`'s own test caught it again immediately.
      Fixed with an explicit `isStatement` flag per item (`Text`/
      `Interp` indent only their first physical line; `Code` indents
      every line). specs/okay-script.md "Line-accurate errors".
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
- [x] intent-other-collapse (original) — the lane's own measurement: declaring an
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
- [x] intent-precedence-rule — LANDED 2026-09-04 as a REFUSAL. The
      design answer (a `Taxonomy[I]` typeclass beside the schema, so a
      tie-break travels as far as the type) was built and measured, and
      stating the rules cost 0.043 macro F1 with every class falling —
      including the class the second rule was aimed at. Not shipped: an
      API whose only measurement says it hurts is an unearned claim in
      code. The four lines are in the lane's history. Original entry
      follows.
- [x] intent-precedence-rule (original) — `Proposal` vs `Request` confused 3 of 6
      in the same run, and it is genuine overlap rather than model
      error ("Can we move Thursday's sync to Friday?" is both). Needs a
      stated precedence rule travelling WITH the taxonomy (a doc
      comment the prompt renders), not a better classifier.
- [x] intent-symbolic-tier — LANDED 2026-09-04, built on the operator's
      word rather than its trigger, and NOT wired in: 112us per message
      (fast enough), but agreement plateaus at 60-64% and does not rise
      with the margin, so the margin is not a confidence signal and
      there is no threshold at which it can safely answer. At margin 0.2
      it would spend ~14 points of end-to-end accuracy to save 55% of
      calls. Original entry follows.
- [x] intent-symbolic-tier (original) — an LU dictionary over `Postings`/BM25 as a
      first pass that answers the easy majority without a model call.
      TRIGGER: measurement shows cost or latency binding on the model
      tier. Linagora's ontology system answers in <150ms with no model
      at all, so the tier is not a rudiment — it is just not yet
      justified here by a number.
- [x] intent-vector-tier — LANDED 2026-09-04, and it EARNS its place,
      unlike the symbolic one. Agreement rises monotonically with the
      margin (80% -> 87% -> 96.3%) where BM25's plateaued at 60-64%: the
      constraint was representation. At margin 0.05 it answers 45% of
      messages at 96.3%, ABOVE the model tier's ~90%, for 12ms of
      embedding plus 90us. Composition is three lines at the call site;
      no wrapper, so the caller sees which call is being paid for.
      Original entry follows.
- [x] intent-vector-tier (original) — class centroids, then a linear probe over
      frozen embeddings, trained from LLM-distilled labels (keep only
      `Conf.High` plus the human confirmations the `Clarify` path
      produces). 18x1024 weights is 72KB; a cosine at 1536 components
      measured 1.04us in `Store.scala`, so ~18us for 18 classes — the
      "sub-millisecond encoder" tier with no dependency and no training
      pipeline. Needs 30-100 examples per class, not the 1k-5k a
      fine-tuned encoder wants. TRIGGER: the symbolic tier starts
      missing paraphrases.
- [x] intent-temporal-slots — LANDED 2026-09-04. `Temporal` parses the
      shapes scheduling mail uses, relative to a reference day passed as
      an argument (a parser that reads the clock cannot be tested), and
      REFUSES everything else rather than guessing — a wrong date is
      acted on, a declined one is asked about. Hinnant's civil algorithm
      rather than month tables, no `java.time`, so the JS build keeps
      it. 13 tests, 3 properties. Original entry follows.
- [x] intent-temporal-slots (original) — a `When` slot takes ISO-8601 and refuses
      anything else through `SIso`, so "next thursday" cannot be
      filled. A Duckling-equivalent over `okay-lex`/`okay-parse` is its
      own lane; until it exists the model does the conversion and the
      schema checks it.
- [x] intent-live-provider — LANDED 2026-09-03, and it REFUTED the
      claim it set out to quantify: the early stop saves 0.0% against a
      real model, under a strict prompt (nothing follows the closing
      brace) and under a prose-inviting one (the value never decodes,
      so the walk runs to the end). The mechanism itself works — proven
      on a counting synthetic stream in the default gate — but a
      classification prompt that says "and nothing else" already buys
      what `cut` would buy. Spec sentence removed rather than softened.
      Original entry follows.
- [x] intent-live-provider (original) — `Classify.prompt`/`read` are tested
      against hand-built and round-tripped values, not a live model.
      The end-to-end run belongs with `TestLive`'s gating, and it is
      what would let `Structured.cut`'s token saving be measured rather
      than reasoned about.
- [x] intent-eval-on-journal — LANDED 2026-09-04. A recording IS a
      journal: `Durable.Entry` + `Rerun.Version` + `FileVersions` held
      it with nothing new invented. 13 minutes of live calls become
      0.046 s in the default gate, reproducing the live report exactly.
      Two guards, both verified by breaking them: the prompt
      fingerprint refuses a stale recording, and `Eval.regressions`
      finally guards something. Original entry follows.
- [x] intent-eval-on-journal (original) — bind `Eval` to a `Rerun` journal so an
      evaluation run is replayable and a regression names the step that
      changed, not just the class that fell. The spec names this as the
      intended fixture; the lane deliberately did not build it, and the
      seam it needs is only that `Eval` takes label pairs from
      anywhere.
- [x] intent-domain-in-names — LANDED 2026-09-03, and it changed the
      recommendation: naming the domain in the case names does the
      gate's work at half the calls (macro F1 0.907, `Other` F1 0.92,
      vs 0.906 / 0.86 for generic names + gate), and the two do NOT
      compose (0.830 together). The gate is now documented as the
      fallback for taxonomies that cannot be renamed. Original entry
      follows.
- [x] intent-domain-in-names (original) — the residue the gate does not catch: one
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
- [x] intent-fixture-too-small (original) — at n=24 a difference of one or two
      replies is not a difference, and a mid-lane wording change moved
      an arm by two. Grow `IntentFixture` past the reference's minimum
      (30 per class) before defending any gap in the arms table as real.
- [x] intent-gate-non-english — LANDED 2026-09-04, and it REFUTED its
      own premise. Re-measured on domain-bearing names, the gate does
      not pay in any of six languages: neutral in three, costly in
      three, and its worst damage is in ENGLISH (0.881 -> 0.602), not
      outside it. The "non-English" framing came from having measured
      the gate only against generic names. The language gap itself is
      real and naming does not close it — Russian stays weakest (0.652)
      across two independent runs. Original entry follows.
- [x] intent-gate-non-english (original) — the gate loses PRECISION outside
      English: `Other` precision 1.00 en, 0.75 fr, 0.60 ru, with recall
      1.00 everywhere, so it is pushing genuine in-domain messages out
      rather than failing to catch out-of-domain ones. Opposite
      direction to the English failure. Try stating the domain in the
      gate prompt in the message's own language, or giving the gate the
      same few-shot treatment that fixed the taxonomy prompt; measure
      per language on `IntentFixture.parallel`, not in aggregate.
- [x] intent-decode-rate-residue — LANDED 2026-09-04 as 03cf0da4. Not a
      residue: 9 of 10 failures were ONE malformation, the model closing
      the intent's object a brace too late and swallowing `conf`.
      Declaring `conf` before `intent` took undecodable from 10/120 to
      0/120. The diagnosis was a `groupBy` over failures the harness
      already collected and threw away. Original entry follows.
- [x] intent-decode-rate-residue (original) — 11 of 120 replies still undecodable
      on the best arm (9%). The rendered example took this from 32% to
      9% and then stopped; what remains has not been looked at, and a
      caller cannot tell a hard message from a malformed reply.
- [x] intent-name-sensitivity — LANDED 2026-09-03. The control held: a
      nonsense qualifier (`Zarnic`) is the WORST arm, not a free win, so
      the effect is the domain and not the appearance of deliberate
      naming. A wrong domain (`Shipping`) proved the word is read, by
      damage: it lifts `Other` recall as much as the true domain while
      halving `Proposal` recall, i.e. it pushes meeting messages into
      `NotAboutShipping`. `Other` PRECISION separates right from wrong
      domain (0.92 vs 0.72) where recall does not. Original entry
      follows.
- [x] intent-name-sensitivity (original) — the whole result rests on four
      identifiers, so the obvious question is how much of it is the
      word "Meeting" and how much is any qualifier at all. Try a third
      taxonomy with a DIFFERENT domain word and a fourth with a
      nonsense qualifier: if the nonsense one also lifts `Other`, what
      helps is the model noticing the names were chosen, not the domain
      they name.
- [x] intent-language-gap — LANDED 2026-09-04. Precondition done (12 ->
      30 meanings per language) and it refuted the spec's own ordering
      claim: at n=30 English is best (0.929) and the middle four cluster
      at 0.887-0.895, so "Spanish and French above English" was
      small-sample noise. The gap survives: Russian ~0.19 below English
      across two runs at two sizes. BOTH candidates failed — native case
      names −0.029 on average (helps fr/ru, badly hurts de/es), an
      explicit domain sentence −0.052 (only ru gains). Original entry
      follows.
- [x] intent-language-gap (original) — Russian 0.652 and German 0.813 against
      Spanish 0.914 and French 0.900, with domain-bearing names and no
      gate, so this is not a gate artifact and not a simple
      English-first ordering (English is 0.881, below both Spanish and
      French). Two candidates, worth trying SEPARATELY because they
      cost different things: case names written in the message's
      language, and an explicit domain sentence in the prompt. Measure
      per language on `IntentFixture.parallel`; and grow that set past
      twelve per language first, because twelve supports "there is an
      effect" and not the size of any one number.
- [x] intent-tiebreak-by-example — LANDED 2026-09-04 as a REFUSAL, and
      worse than the prose it was meant to improve on: 0.854 against
      0.866 for prose and 0.909 for neither, with `Request` recall
      collapsing 0.87 -> 0.63. An example of a BOUNDARY generalises
      past the boundary, where an example of a CLASS generalises
      usefully — the first measurement in this line where few-shot
      examples cost anything. Conclusion: the Proposal/Request overlap
      is not fixable from the prompt by either channel; it is a
      labelling question. Original entry follows.
- [x] intent-tiebreak-by-example (original) — the precedence lane's own suggestion
      for what to try before reaching for stated rules again: render a
      tie-break as EXAMPLES of the disputed case rather than as prose
      (few-shot examples are the one lever that has consistently paid
      in this line), and use one rule rather than a list. The prose
      version cost 0.043 macro F1 and diluted every class.

## channel-sentinel-default — buy drain-on-close as a layer, not as an invariant

Measured 2026-09-04 (`ChannelGuaranteeBenchmark`, N=4000, cap=1024):

| lane | us/op | contract |
|---|---|---|
| okayStrong (`StmChannel`) | 333.6 | drains on close, termination detected |
| okayWeak (`AbruptChannel`) | 206.0 | close discards, no detection |
| **okayLayered** (`AbruptChannel` + FIFO sentinel) | **177.4** | **same as okayStrong** |
| zioStrong (`Queue[Option]`) | 187.1 | our contract, their queue |
| zioWeak (`Queue` + `take(N)`) | 168.9 | count known up front |

The result that matters is the one that refuted the expectation. The
strong contract costs ZIO 11% (187 vs 169) and costs us 62% (334 vs
206) — so the guarantee was never the whole gap, but the WAY we buy it
is. They express it ABOVE the queue, as one sentinel travelling in
FIFO order behind the buffered elements. We express it INSIDE the
transition, so every send and every receive reads the state that makes
termination derivable, and pays for it whether or not close is ever
called.

Buying it their way and keeping our contract lands at 177.4 — 1.9x
cheaper than `StmChannel` and past `zioStrong`.

The work: an end-marker carried in the channel's own element slot
rather than a boxed `Option` (the benchmark boxes, and still wins),
with the layer owning `close` so the sentinel cannot be overtaken.
Then `Channel.apply` can default to the weak mechanism plus the layer
and lose no promise. Blocked on nothing; wants the two-tier laws
(landed) to hold the line while the default moves.

## channel-bulk-send — the push side is still per-element

`Ring.popMany` batched the consumer's head CAS; `push` still takes the
tail one element at a time, so a chunked SEND (`Channel.mergeChunked`,
`feedChunked`) pays a tail CAS per element the way the receive side
used to pay a head CAS. Symmetric fix: claim a run of writable slots
with one `compareAndSet` on the tail, then publish each stamp. Wants
the same contending-producers law the bulk receive got.

## channel-callback-allocation — two allocations per element

Leaf samples on the elementwise lane: `boxToBoolean` 8% (Function1 is
not specialized on Boolean, so every `sendAsync` callback boxes) and
`Right.apply` 3.4% plus the `Some` beside it (`End =
Either[Throwable, Option[A]]` wraps each element twice). Chunking
makes both per-batch, which is why they were left; they still stand on
the elementwise path. A dedicated SAM with `onValue`/`onEnd`/`onError`
removes both without a cast, but it changes an abstract primitive on
`Channel` and every implementation with it.
- [ ] intent-examples-in-language — the candidate this lane deliberately
      did not confound into itself: the example MESSAGES stayed English
      throughout, so the native-names arm moved one variable. Translating
      the five few-shot examples is untried, and examples OF A CLASS are
      the one lever that has consistently paid here — unlike every prose
      addition, which has now cost four times running.
- [x] intent-symbolic-patterns — ALREADY DONE, closed 2026-09-04: the
      bake-off built it as the `Patterns` tier (88.6-90.9% where a cue
      fires, 58.3% coverage, 96us, no network) and nobody marked the
      entry. Original entry follows.
- [x] intent-symbolic-patterns (original) — the symbolic tier failed as BM25 over
      examples because BM25 matches CONTENT words and an intent is
      carried by function words and syntax ("could you" vs "shall we").
      Linagora's system did not do BM25: it matched lexical-unit
      PATTERNS tied to frames. That is a different mechanism, it targets
      exactly the failure measured here, and it is cheap to try — but
      only worth it if a zero-network tier is wanted, since the vector
      tier already covers 45% at 96.3% for 12ms.

## okay-agent: understanding without a model — after intent-tier-bakeoff (2026-09-04, specs/intent-classify.md)

The goal is a classifier with NO GENERATION on the request path.
Measured so far: linear probe 86.7% at full coverage (one 12ms embed),
centroid 80.0%, kNN 58.3%, chargrams 60.0%, patterns 51.7% (89% where
they fire), BM25 45.0%; the model tier is ~90%. Everything below is
ordered by what it would FIX, not by novelty.

- [x] conversation-runtime — LANDED 2026-09-04 as `Conversation.scala`
      on durable-waiting-on-a-person. Frame/Slot described by the
      caller, a Reply that is a choice, an intake that asks the next
      unanswered slot, re-asks once, reads back, and holds no state of
      its own. The compiler found the design hole: with no rendering
      at ask time `lang` was unused, which meant the language of an
      exchange was stored nowhere — every Say now carries the text as
      it was asked. Original entry follows.
- [x] conversation-runtime (original) — specs/conversation.md. The runtime a
      human-facing conversation needs, with the boundary drawn so a
      caller owns only its own domain: an intake driver over frames and
      slots, a re-ask when a slot cannot read its answer, a read-back
      before anything is written, a language pinned to the exchange,
      and a reply that is a CHOICE rather than a string. Depends on
      durable-waiting-on-a-person for the suspension; without it this
      is another hand-written state machine, which is what it exists to
      stop. The spec carries the incidents from a working
      implementation of the same shape, including the one where a
      re-derived language flipped mid-intake.
- [x] durable-waiting-on-a-person — LANDED 2026-09-04. `OnRepeat.Await`
      is read in BOTH branches (an awaiting operation has no inner
      effect to run, so it is recognised on its first encounter too),
      `Durable.Awaiting` is the control transfer out, and
      `Durable.awaiting(journal)` names the entry a program is parked
      on. Resuming is `complete` plus re-running: no new mechanism.
      Two properties held down by tests — an awaiting operation never
      reaches the inner handler, and the program's own sequence
      decides what runs next rather than the order answers arrived.
      Original entry follows.
- [x] durable-waiting-on-a-person (original) — `Durable` journals INTENT FIRST and
      the answer after, so an `Entry` with `answer = None` is
      structurally a question asked and not yet answered. But recovery
      reads every missing answer as the crash window, for `OnRepeat` to
      resolve; there is no state for "asked a person, waiting, and this
      is normal, possibly for days". With one, a conversation is a
      durable program — ask, ask, act, resumed across a restart from
      the log — instead of a hand-written state machine, which is what
      a consumer built for want of this. Two constraints it must carry:
      replay resumes from RECORDED verdicts rather than recomputed ones
      (a refitted classifier otherwise rebuilds a different
      conversation), and a suspension takes a message that may be a
      correction, an unrelated request or a command rather than the
      answer, so the resumed value is a choice and the handler decides.
      See "Open requests from a consumer" in specs/intent-classify.md.
- [x] intent-taxonomy-value — DONE 2026-09-04 as `Taxon` (0cf1f7c5): a value with `of[I]` from a Schema and `parsed` from strings, deriving a Schema so it rides as data, plus `check` refusing a label the taxonomy does not hold. Original entry follows.
- [x] intent-taxonomy-value (original) — the model tier reads its classes from
      `Schema[I]`, `NoModel.fit` infers them from its training rows,
      and nothing connects the two: the tiers cannot be aimed at one
      taxonomy without aligning it by hand, and a taxonomy that
      arrives as DATA cannot reach the model tier at all. Blocks
      intent-label-distillation from defining classes rather than only
      examples. A `Taxonomy` (classes, optionally examples per class)
      with `Taxonomy.of[I]` as one constructor and a parsed form as
      another. See "Open requests from a consumer" in the spec.
- [x] intent-language-in-fit — DONE 2026-09-04 as `Row.lang` and `ByLanguage.fit` (0cf1f7c5), with a pooled fallback below `minRows` (32, from the learning curve). The MEASUREMENT is deliberately not run: the parallel set has 30 messages per language, so an arm would train on fifteen — see intent-language-fixture-growth. Original entry follows.
- [x] intent-language-in-fit (original) — a training row is `(text, embedding,
      class)` and cannot say which language it is in, so a multilingual
      fit pools every language into one boundary. intent-language-gap
      measured what that costs (0.741 against 0.929) and
      intent-embedding-choice is about to compare encoders PER
      LANGUAGE, which this row shape cannot express. A grouping key,
      not new mathematics; a pooled fallback where a language is too
      thin. WORTH DOING BEFORE the embedding comparison, not after.
- [x] intent-verdict-ranking — DONE 2026-09-04 (8fe8e809), and then CORRECTED by a sibling (fdcf0d97): the ranking I handed back was invented below rank 1, because `blend` asked the probe one class at a time and split the remainder evenly. The seam was right and what flowed through it was not — my tests asserted the shape and never that rank 2 is the second most likely class. Original entry follows.
- [x] intent-verdict-ranking (original) — `Probe.Verdict` carries `margin` and
      `runnerUp`; `NoModel.Verdict` drops both, so an abstaining caller
      knows only THAT it declined. Wanted by an interface that offers
      the two candidates it could not separate, and required by
      intent-active-learning, which samples by uncertainty and needs
      the distribution. The value exists one layer down.
- [x] intent-trained-codec — DONE 2026-09-04 as `Fitted` (c2fc1949): a record with a derived Schema for every trained model, numbers as bytes rather than digits (21KB against 36KB, 1.7x), and round-trip tests that compare PREDICTIONS rather than fields. Original entry follows.
- [x] intent-trained-codec (original) — `Trained` is arrays with no codec, so
      fitting lives wherever loading lives. A caller that compiles its
      vectors at build time wants to fit there too and load weights at
      boot. Makes "no generation on the request path" also mean "no
      fitting on it".
- [x] intent-slot-descriptor — PROPOSED 2026-09-04 and sent for review,
      not declared finished. `Slot[A]` is a name, a question per
      language and a parser whose failure is a RE-ASK; `Frame[I]` holds
      what is filled and answers `missing` in the reader's language.
      `Temporal` is now one implementation of `parse` rather than a
      special case, and `intent-crf-slots` becomes an alternative
      behind the same seam. The descriptor holds no conversation state:
      suspension stays `Conversation`'s, on `Durable`. Original entry
      follows.
- [x] intent-slot-descriptor (original) — `Temporal` parses one slot in one
      language and intent-crf-slots is filed for the general case. A
      slot as a NAME, a question per language, and a parser
      `String => Option[Value]` whose failure is a RE-ASK: then
      `Temporal` is one parser, another language is another parser
      rather than a rewrite, and the CRF lane is an alternative
      implementation of the same seam. Gives "a filled frame" somewhere
      to live.
- [x] intent-label-distillation — LANDED 2026-09-04 for the ZERO-NETWORK
      tiers, as the learning curve required. 320 generated, 182 kept by
      self-consistency (57% — the model disowns 43% of its own labels),
      and chargrams go 60.0% -> 66.7% on held-out HUMAN data when the
      distilled corpus is ADDED to the fixture. Trained on the
      distilled corpus alone it scores 50.0%, so this is a supplement
      and not a substitute: the model's writing has a different
      distribution from real messages. 66.7% is now the best
      no-network number, above the static table's 63.3%. Original
      entry follows.
- [x] intent-label-distillation (original) — REPRIORITISED 2026-09-04 by
      intent-learning-curve: NOT the one that moves the probe, whose
      curve is flat past 32 examples. It is the lane for CHARGRAMS,
      which are still climbing at 65% and are the only candidate for a
      classifier with no network at all. The text below was written
      before the curve and its premise about the probe is refuted.
      ORIGINAL: Every
      tier here is fitted on 60 labelled messages, and the probe's
      86.7% is a data limit rather than a method limit: it fits 4096
      weights on 60 rows. Use the model OFFLINE, once, to label a large
      unlabelled corpus (its own accuracy is ~90%, and label noise at
      that level is survivable), keep only `Conf.High` plus whatever a
      human confirmed, and refit. This is the reference's own advice
      ("few-shot LLM as a bootstrap for data generation") and the only
      route by which a no-model classifier reaches model accuracy.
      TRIGGER: none needed — it is the cheapest large gain available.
- [x] intent-learning-curve — LANDED 2026-09-04, and it OVERTURNED the
      plan it was meant to confirm. The probe flattens at ~32 examples;
      32 to 60 moves it 81.7-86.7%, which is noise. The centroid, with
      three orders of magnitude fewer parameters, flattens in the same
      place — a signal ceiling, not a capacity one. So labels are NOT
      the binding constraint for the embedding tiers and
      intent-embedding-choice moves ahead of intent-label-distillation.
      Chargrams are still climbing (30 -> 65%) and are the tier that
      distillation should feed. Original entry follows.
- [x] intent-learning-curve (original) — before distilling, measure what more data
      is worth: refit the probe at 15, 30, 45, 60 examples and plot.
      If the curve is still climbing steeply, distillation pays; if it
      has flattened, the ceiling is the representation and
      `intent-embedding-choice` is the lane instead. One afternoon,
      no new code, and it decides which of the two to fund.
- [x] intent-embedding-choice — PARTLY LANDED 2026-09-04, and blocked on
      installation rather than code: exactly one embedding model is
      served and the gateway refuses any other id with 400. Established
      anyway that the ceiling IS representational — the model tier and
      the probe share ZERO errors out of 60, so the signal is in the
      text and the vector is losing it — and that framing moves the same
      model 6.6 points (81.7% to 88.3%), with a SHORT classify
      instruction the best of four. Original entry follows.
- [x] intent-embedding-choice (original) — every tier above 80% goes through ONE
      embedding model, and the Russian gap (0.741 against English's
      0.929) is plausibly that model's multilingual quality rather than
      anything in this code. Swap in a second embedding model behind
      the same seam and re-run the bake-off per language. Cheap, and it
      is the only way to tell a representation problem from a
      classifier problem.
- [ ] intent-rule-induction — patterns are 88.6-90.9% accurate where
      they fire and fire on only 58.3% of messages, and the cues are
      hand-written. Induce them instead (RIPPER-style: grow a rule,
      prune it against held-out data, repeat) so coverage grows with
      the corpus rather than with someone's patience. Keeps the zero-
      network property, which nothing else above 60% has.
- [ ] intent-tfidf-word-linear — the classical baseline nobody ran:
      word-level TF-IDF into the same logistic regression. It sits
      between BM25 (45.0%) and chargrams (60.0%) in what it sees, and
      it is thirty lines given `Probe`'s optimiser. Worth it to know
      whether chargrams' 60% is about characters or just about having
      a linear model at all.
- [ ] intent-fasttext-subword — subword embeddings TRAINED on the
      corpus plus a linear head, i.e. fastText's actual algorithm in
      plain Scala. Bridges chargrams (language-agnostic, no network,
      60%) and the probe (86.7%, needs a server): a trained
      representation that still ships as an array. Only worth it if
      `intent-embedding-choice` says the server is the problem.
- [ ] intent-grammar-parse — intent by GRAMMAR over `okay-lex` and
      `okay-parse`, the way `Temporal` does dates: deterministic,
      explainable, and refusing rather than guessing. Expensive in
      rules, and the honest reason to want it is a domain where a wrong
      answer is worse than no answer.
- [ ] intent-crf-slots — sequence labelling for the frame's SLOTS
      (who, when, where) rather than its class. `Temporal` fills one
      slot with a parser; the general case is a tagger, and a CRF is
      the classical one. Only after the class problem is settled.
- [ ] intent-active-learning — labels are the bottleneck everywhere
      above, so choose the next ones to label by uncertainty rather
      than by order. Directly compounds with `intent-label-distillation`
      (the model labels, a human confirms the uncertain ones), and
      needs the calibrated confidence `intent-no-model` is building.
- [ ] intent-ensemble-weights — `NoModel` blends the probe with the
      pattern tier using ONE fitted weight from a six-point grid,
      because sixty rows cannot support a fitted second-level model.
      When the corpus grows (see distillation), replace the grid with a
      real stacking model and measure whether it beats the blend.

## channel-chunk-batch-size — the batch is set by how far the producer runs ahead

`SentinelChannel` wins elementwise (208.9 against `StmChannel`'s
300.1) and is level chunked (175.3 against 172.3), so the one lane we
lose to `zio.Queue` — `zioStrongChunk` at 128.0 — is still open.

The lever is measured and it is not per-operation cost. Average
elements per bulk receive: `StmChannel` 363.6, `AbruptChannel` 65.6,
`SentinelChannel` 43.5. A ring wakes a receiver on every push, so the
consumer returns before the buffer accumulates; `StmChannel`'s
transaction hands over whatever the buffer holds, and its consumer
therefore takes it in eleven operations rather than ninety.

Two directions, neither obviously right. Hold a woken receiver back
until either a small dwell has passed or the ring has n elements —
throughput bought with latency, and the flush machinery from
`source-merge-chunked` already exists to bound it. Or wake on a
watermark rather than on every push, which costs nothing in latency
when the consumer is already behind and nothing at all when it is
keeping up. Measure both; the second looks cheaper.

Related: this is also why `Ring.pushDeciding` takes a flag and not a
function. Anything between the claim and the publish truncates a
concurrent `popMany` scan, which counts CONSECUTIVE published slots —
a closure there cost 65.6 elements per batch down to 43.5.

## channel-elementwise-wakeups — the other side of the offer-first trade

`channel-send-fastpath` took the chunked lane from 175.3us to 58.7 and
cost the elementwise one 208.9 -> 268.7, which also puts
`SentinelChannel` behind `StmChannel` on that axis (235.5).

The cause is not the extra failed `offer`. It is that the producer can
now saturate the ring, so every send parks and every pop wakes a
sender — one unpark per element on the consumer's critical path. A
chunked consumer amortizes those wakeups across a whole batch; an
elementwise one pays one each.

Worth trying, cheapest first. Give the RECEIVE side the same fast path
the send side just got: `receiveBlocking` still allocates a handshake
slot per element, and in the elementwise shape the consumer is the
bottleneck, so speeding it may pay twice — directly, and by keeping
the ring off its full mark. Failing that, wake senders on a watermark
rather than on every pop, which is the same idea as
`channel-chunk-batch-size` read from the other end.
- [x] intent-second-embedder — LANDED 2026-09-04 and it settled the
      ceiling question: 4B (2560 dims) does NOT beat 0.6B (1024). Bare
      it scores ten points lower, but that is framing — the larger
      instruction-tuned model gains +8.3 from a classify instruction
      against the small one's +1.6 — and framed it reaches 85.0%
      against 88.3%. The mechanism is the learning curve's: 2.5x the
      weights on the same 60 examples, in a regime where data binds.
      So 88.3% is the TASK at this data size, not the vectoriser.
      Original entry follows.
- [x] intent-second-embedder (original) — install a second embedding model and
      re-run the bake-off and the per-language table; this is the
      experiment intent-embedding-choice could not run. Candidates
      against our constraints (local, MLX, multilingual):
      `Qwen3-Embedding-4B/8B` (same family, direct swap), `BGE-M3` and
      `multilingual-e5-large` (multilingual strength, for the Russian
      arm), `jina-embeddings-v3` (has a CLASSIFICATION adapter, not
      just retrieval), `gte-multilingual-base` (half the size).
- [x] intent-static-embeddings — LANDED 2026-09-04. Distilled from our
      own teacher rather than downloaded, so no foreign tokenizer had
      to be matched. Words alone cap at 51.7% even with complete
      vocabulary coverage — a bag of words cannot tell "could you" from
      "we could", the same mechanism that sank BM25 — and adding
      adjacent PAIRS lifts it to 63.3%, the best no-network number so
      far, above chargrams' 60.0%. The remaining 23 points to the
      teacher are CONTEXT, which a static table cannot have. Original
      entry follows.
- [x] intent-static-embeddings (original) — `model2vec`/`potion`: a transformer
      distilled into a LOOKUP TABLE, so there is no neural inference at
      request time at all — roughly 30MB, no server, no round trip.
      The only candidate that could give embedding-grade accuracy with
      the zero-network property chargrams have, and it drops straight
      into `Centroid` and `Probe`, which do not care where a vector
      came from. Directly serves the no-generation goal.
- [ ] intent-instruction-prefix — a short "Classify the intent of this
      message: " prefix measured +1.6 (probe) and +3.3 (centroid), and
      the spread across four framings was 6.6 points. Both are at or
      near the noise floor on 60 messages: re-measure on the grown
      fixture before making it the default, and keep the finding that
      LONG instructions cost (81.7% for the e5-style one).
- [ ] intent-static-trigrams-and-pca — two obvious extensions of the
      static table, both filed rather than guessed: adjacent TRIPLES as
      well as pairs (pairs were worth 11.6 points, and the same
      argument applies once more with diminishing returns and a bigger
      table), and `model2vec`'s PCA step to cut 1024 dimensions to
      256 — 1303 units already cost 5.2MB as float32, and a production
      vocabulary of 30k would be 120MB.
- [ ] intent-language-fixture-growth — SHARPER NOW (2026-09-05): the
      fixture is eight languages wide (uk and pl added) and every
      non-English language has at least one class at F1 0.00 when
      fitted on fifteen rows of it. The construction is
      language-agnostic; the DATA is what is missing. Original entry:
      the per-language arms train on
      FIFTEEN examples each, where the learning curve put the probe's
      stabilisation at about thirty-two, and the numbers swing from
      46.7% to 86.7% accordingly. No per-language claim about
      embedders or classifiers is defensible until the parallel set has
      at least 30 messages per language, which means growing it from 30
      meanings to 120. That is a translation job, and the
      author-written-translation limitation grows with it.
- [ ] intent-4b-with-more-data — the 4B embedder is worse at 60
      examples because 2560 dimensions need more of them, which is a
      prediction rather than a defeat: re-run the learning curve on
      BOTH embedders and find where the lines cross. If the 4B
      overtakes past some n, it is the right vectoriser for a
      distilled corpus even though it is the wrong one today.
- [ ] intent-distil-more — 320 generated messages bought chargrams 6.7
      points and the curve was still climbing when the fixture ran out.
      The generator is resumable, so this is machine time rather than
      work: raise the target, re-filter, and find where the gain stops.
      Also worth trying on the STATIC table, which was not fed here
      because its vocabulary would have to be re-embedded — a second
      pass over the teacher rather than a change of method.
- [ ] intent-distil-diversity — the distilled corpus alone scores TEN
      POINTS below a human fixture a third its size, which says its
      distribution is narrow rather than its labels wrong. Prompting
      for a persona, a register or a length before each batch is the
      cheap thing to try; measuring the corpus's own diversity (say,
      distinct trigram ratio against the human fixture's) is the honest
      way to tell whether it worked.
- [ ] intent-distil-dose — a little distilled data is worth ten points
      to the centroid (80.0% -> 90.0% at +40 rows) and more is worse,
      monotonically, down to 78.3% at +320. The optimum was found by
      accident between two arms; find it properly, and find out whether
      the self-consistency filter (which kept 182 of 320 and was NOT
      applied in that run) moves it. Also worth asking whether the
      right knob is a dose at all or a WEIGHT — distilled rows counted
      at less than one in the fit, which a centroid can express and a
      grid over doses cannot.
- [x] intent-centroid-reconsidered — LANDED 2026-09-04 and it RETRACTED
      the previous lane's headline: the centroid's 90.0% was measured on
      BARE embeddings, and with the classify instruction the same recipe
      gives 83.3% -> 85.0%, a peak gain of +3.4 at 20 rows instead of
      +10 at 40. Both are near the noise floor on 60 test messages; what
      survives is the DECLINE at large doses. The 4B and
      instruction-prefix conclusions stand for the centroid too.
      Original entry follows.
- [x] intent-centroid-reconsidered (original) — the centroid at 90.0% is now the
      best result in the programme, beating the probe it was supposed
      to be a baseline for, at four vectors against 4096 weights. Every
      arm in the bake-off and the embedder comparison was read as
      "probe first"; re-read them with the centroid as the subject, and
      re-run the ones where the conclusion turned on the probe's
      number.

## channel-sender-livelock — the mirror of the isEmpty/hasReady defect

`receiveAsync` used to recheck `isEmpty` before parking, which counts
a CLAIMED-but-unpublished position as "something is there", so the
consumer spun instead of waiting and starved the very publisher it
waited for. Fixed by asking `hasReady` instead.

The sender side still has the mirror: it rechecks `ring.size <
ring.capacity`, and `size` is `tail - head`, which counts a position
whose slot has been popped but whose stamp has not yet been
republished. So a sender can be told there is room, fail its push, and
go round again. The window is two stores wide on a bounded ring and it
has never been observed, but it is the same shape and deserves the
same treatment: a `hasRoom` on `Buffer`, answered from the stamp of
the position the tail is about to claim rather than from a
subtraction.
- [x] intent-state-the-framing — LANDED 2026-09-04. `Conditions` prints
      embedder, framing, split, corpus and any extra beside every live
      row, with no overload that omits them, so a row cannot be written
      without its terms. Wired into the two suites whose rows were
      compared across lanes. The first version DERIVED the distilled
      count and printed `distilled=260` for an arm with no human rows —
      a condition that lies being worse than one that is missing, the
      counts are passed now. Original entry follows.
- [x] intent-state-the-framing (original) — two measurements an hour apart
      disagreed because one silently changed whether the embedding was
      framed with a classify instruction, and nothing in either printed
      row said which. Make every live arm print its conditions —
      embedder, framing, split, corpus — beside its number, so a table
      cannot be compared against one taken under different terms. This
      is cheap, and it is the defect behind the retraction in
      intent-centroid-reconsidered rather than a nice-to-have.
- [ ] intent-split-other — NOW WITH A NUMBER (2026-09-05): `Other`
      scores recall 0.47 in the shipped composite, the worst class by
      a distance, and it is the one whose failure routes out-of-domain
      traffic into a meeting intent. Original entry: `Other` is one
      diffuse bin (mean pairwise
      cosine 0.55-0.645 against 0.68-0.78 for every real class) and it
      carries two thirds of the probe's lead over the centroid while
      being a quarter of the rows. Converting it to an abstention is 20
      points WORSE (68.3% against 88.3%), so the remaining option is
      the consumer's other one: split it into named classes that are
      individually coherent — Gratitude, SupportIssue, Unrelated, or
      whatever the corpus actually holds. Cheap to try on the existing
      fixture by relabelling, and it would let every per-class number
      in the spec be read without an asterisk.
- [x] intent-russian-rows-fixed — LANDED 2026-09-04. All three hazards
      the consumer named were present: the person marker carrying the
      class on one letter, eight Requests in three templates with a
      duplicated pair, and three calques. Ten rows rewritten by
      CONSTRUCTION and one meaning replaced across all six languages.
      Russian fell 86.7% -> 73.3%, which is what fixing a fixture that
      was flattering itself looks like.
- [x] frame-walk-end-to-end — LANDED 2026-09-05 as okay-demo's
      TestWalk: one message to one produced booking, through the real
      tiers, a real journal, a simulated process death and a
      read-back, asserting the VALUE, the DIRECTION and that anything
      was produced. It caught its own instance of the defect it exists
      for on the first run. Original entry follows.
- [x] frame-walk-end-to-end (original) — the consumer's third point, and the one
      I cannot argue with: okay-frame and okay-intent have ONE caller
      and it is a classifier demo. Both defects they hit today lived
      BETWEEN two correct code paths with 237 unit tests green —
      contacts shown to the wrong side, and a notification that was a
      claim rather than a call. A test that walks a whole exchange
      (classify, fill from the message, ask, answer, assume, confirm,
      act) would have caught both and is worth more than another tier.
- [x] intent-per-class-not-aggregate — LANDED 2026-09-05. `Eval` got
      `support`/`balance`/`majorityBaseline`/`worst`; the shipped
      model's tests print per class and assert both the balance
      (majority baseline < 0.40) and a per-class floor (F1 >= 0.50).
      Found what the total hid: `Other` recall 0.47. Original entry
      follows.
- [x] intent-per-class-not-aggregate (original) — the consumer's imbalance
      finding: they filled a corpus hole, one class grew to 137 of 184
      rows, a probe leaned to the majority, and "сегодня в москве шёл
      дождь" came back as a REQUEST at 0.90 — while HEADLINE ACCURACY
      ROSE, 95.8% to 96.2%, because accuracy on an imbalanced corpus
      rewards predicting the biggest class. Every aggregate this
      module publishes has the same exposure, starting with the
      shipped model's 76.7%. Print per-class numbers and the class
      BALANCE beside every total; `Eval` already computes per-class
      scores, so this is a reporting lane, not a measurement one.
- [ ] frame-rebind — after a restart a caller rebuilds its `Slot`
      values, and `valueOf` matches by IDENTITY, so a frame read back
      from a journal cannot be read with the new descriptors unless
      the caller threads the rebuilt ones through everything.
      `TestWalk` hit this and the rule ("one descriptor value per
      exchange, passed with the frame") is documented, but a
      `Frame.rebind(slots)` would make the restart case ordinary. THE
      HAZARD, which is why it is not built yet: re-deriving a value
      means re-parsing the stored text, and "next Tuesday" against a
      new reference day is a DIFFERENT DATE — the exact defect
      intent-frame-typed-values removed. It must be an explicit
      request that reports what it re-derived, not a silent
      convenience.
- [ ] intent-typo-robustness — character n-grams are supposed to
      survive a typo, and this model does not: one deterministic
      transposition in the longest word takes it from 61.7% to 55.0%
      (2026-09-04). At 60 training messages the hashed 3-5-grams are
      too sparse for the redundancy that argument depends on. Two
      measurable fixes — more rows, or a smaller n — and the suite
      that found it (`TestSecondAuthor`) already measures the result.
- [ ] intent-second-author — PARTLY ANSWERED 2026-09-04 by measuring
      the gap instead of the corpus: 66.7% on the least-familiar half
      against 86.7% on the most, 65-67% under mechanical register
      shifts, and every shipped quote corrected to 65-70% for a
      message somebody else wrote. What remains is the part no
      measurement replaces — a corpus this repository did not write.
      Original entry follows.
- [ ] intent-second-author (original) — the provenance problem the review could
      not fix: the rows are still one hand's Russian, rewritten by the
      same hand that wrote them. A gap measured against my own language
      is a joint measurement of the model and me. The consumer offered
      REVIEW, which is what was available and is now spent; what is
      missing is a second AUTHOR, for Russian and for whatever
      languages the fixture keeps.
- [x] intent-end-to-end — LANDED 2026-09-04 as `okay.demo.IntentRouter`,
      the first caller these tiers have had, and it exposed three
      frictions no test had found: a filled `Frame` hands back TEXT
      rather than the parsed value, the pattern tier speaks canonical
      names so every caller writes a mapping, and `Taxon` is connected
      to none of the tiers that classify. All three filed rather than
      quietly fixed. Original entry follows.
- [x] intent-end-to-end (original) — NOTHING CALLS ANY OF THIS. Thirteen files, a
      dozen measured lanes, and inside okay there is no path where a
      message arrives and a decision leaves. A consumer has their own
      router; okay-intent has no caller of its own. This finds what
      measurement cannot — awkward signatures, missing errors, the
      order a caller actually needs things in — and there is already
      one symptom: `Frame.filled` throws the parsed value away, which
      surfaced the moment I tried to write how it would be used and
      never in any test. Highest value of anything on this list.
- [ ] intent-structured-output — every lane bought its answer's SHAPE
      by persuasion: a rendered example, written rules, field order.
      OpenAI-compatible gateways take `response_format` with a JSON
      schema, which makes the shape a property of DECODING rather than
      of asking nicely. Never tried once. If the gateway supports it,
      it may close the decode question outright and take accuracy with
      it — one experiment, not a research programme, and it is not a
      data problem.
- [ ] intent-multi-intent-measured — spans have been in the type since
      the first lane and were argued for as the thing a flat list
      cannot express, and the fixture contains NOT ONE message with two
      intents. The mechanism has never been exercised. Either measure
      it or stop claiming it.
- [ ] intent-jmh-row — this line quotes microseconds everywhere (76us
      probe, 90us centroid, 92us chargrams) and every one of them is a
      `System.nanoTime` around a loop inside a test: no warmup, no JIT
      accounting, one run. The repo keeps `src/jmh/history.tsv` for
      exactly this, and by its standard those are not measurements. A
      benchmark row per tier, or the numbers should stop being quoted.
- [x] intent-frame-typed-values — LANDED 2026-09-04. `Frame` keeps
      `Answered` (slot, text, parsed value) and `valueOf` takes the
      SLOT, which is the evidence the answer has that type. One
      isolated cast, guarded by `a.slot eq s` and tested with a
      same-named slot of a different type getting nothing back. The
      caller's test that recorded the defect now pins the property.
      Original entry follows.
- [x] intent-frame-typed-values (original) — `Frame.filled` is `Map[String,
      String]`: a slot knows its type `A`, parses the answer to prove
      it is acceptable, and then stores the raw text. A caller that
      wants the date parses it a SECOND time, with the same reference
      day, and nothing in the type says so. Demonstrated from
      `IntentRouter` rather than argued. The obstacle is holding
      heterogeneous parsed values without a cast — a type-indexed map,
      or `Frame` carrying a tuple of its slots' types; the consumer has
      built one and may already know which.
- [x] intent-cues-for-a-taxonomy — LANDED 2026-09-04. `Patterns.Cues`
      pairs a cue set with the `Taxon` it decides, checked once at
      construction; `renamed` moves it onto another taxonomy and is
      total in both directions, so the router's silent `case _ =>` is
      now a `Left` naming the class nobody mapped. `IntentRouter` lost
      both the translation and the `.filter(taxonomy.has)` behind it.
      Original entry follows.
- [x] intent-cues-for-a-taxonomy (original) — `Patterns.meeting` hardcodes the
      canonical class names, so a caller with a domain-bearing taxonomy
      writes a mapping, as `IntentRouter.canonicalToTaxonomy` does and
      as every caller after it will. `Cue.cls` is a `String` and could
      carry any names: what is missing is a way to state a cue set
      AGAINST a `Taxon`, and a check that every cue names a class the
      taxonomy holds.
- [ ] frame-language-with-grammatical-gender — the migrating consumer
      raised it and could not test it: `Slot.ask` is keyed by a
      language CODE, and a language whose question differs by the
      grammatical gender of the ADDRESSEE (Polish Pan/Pani, and the
      formal registers around it) needs more than a code — or needs
      the caller to key by "pl-formal-f" and own the choice. Their
      four languages dodge it because Polish there addresses
      informally. Worth settling before the Map-keyed language is
      called done for all languages.
- [ ] intent-temporal-multilingual — (absorbs the older
      intent-temporal-other-languages, filed twice by me before I
      noticed the first.) `Temporal` parses English, so
      `Frame.fillFrom` fills English rows and declines the other five
      languages: measured 5/5 in English and 0/5 in fr, de, es, ru, ja
      over the parallel fixture (2026-09-04). The router degrades
      correctly — it asks, in the reader's language — so this is a
      coverage lane rather than a correctness one. The shape is
      already there: `parse` is a word-list scan over weekday, month
      and relative-day vocabularies, so a second language is those
      three vocabularies plus its own qualifier words ("prochain",
      "nächsten", "próximo", "следующий"), not a new design. Japanese
      needs a different tokeniser and should be its own decision.
      The fixture's parallel set already carries "jeudi prochain", "am
      Montag", "el martes", "во вторник" and Japanese weekday forms,
      so the test data for this exists.
- [x] intent-fitted-model-ships — LANDED 2026-09-04. `Models.meeting`
      is a fitted CharGrams model that ships (43KB generated source,
      cross-platform), `Fit` is the corpus->model->file->model door,
      and `MakeModel` regenerates the artifact with a test asserting
      the committed bytes are what it produces. 76.7% at full coverage
      behind the cues on held-out English, no network. Original entry
      follows.
- [x] intent-fitted-model-ships (original) — NOTHING SHIPS A FITTED MODEL, and
      there is no documented way for a caller to obtain one. Every
      measured tier above the pattern cues needs a `Centroid.Trained`
      or a `Probe.Trained`, which today exists only inside a test that
      fitted it from the fixture. A caller reading the module has the
      types, the accuracy tables, and no path from "I have messages"
      to "I have a model" — `Fitted` writes one as data and nothing
      writes the file. Named to the operator as a usability blocker on
      2026-09-04 and not filed until now, which is the miss this entry
      exists to correct.
- [x] intent-one-entry-point — LANDED 2026-09-04. `okay.intent.Router`
      holds the measured tier order and the four outcomes;
      `Router.of` refuses a tier that does not speak the taxonomy;
      `Router.offline()` needs nothing. The demo is a caller now and
      is shorter for it. `CharGrams.renamed` came with it, so a
      domain-bearing taxonomy can use the shipped model. Original
      entry follows.
- [x] intent-one-entry-point (original) — the composition of the tiers lives ONLY
      in `okay.demo.IntentRouter`: cue tier first, vector tier below
      it, escalate under the margin, fill the frame, ask what is
      missing. That order is the measured one and a caller outside the
      demo has to re-derive it by reading twenty Results sections.
      okay-intent should hold the composed door itself, with the demo
      as its caller rather than its definition. Also named on
      2026-09-04 and unfiled until now.
- [ ] intent-extract-more-slots — only `when` and whole-message text
      have extractors. Named entities (who), durations, places and
      amounts are the obvious next ones, and each is a `Slot.extract`
      rather than a design.
- [ ] intent-taxon-wired-to-tiers — request 1 asked for one taxonomy
      both tiers read, and what landed is one taxonomy that NEITHER
      tier reads: `Classify` takes a `Schema[I]`, `Patterns` takes
      cues, `Centroid` takes whatever labels it was fitted on, and a
      caller checks `taxonomy.has` by hand afterwards. The value is
      right and the wiring is absent — `Taxon` should be what a tier is
      fitted or built against, so a mismatch is a compile or fit error
      rather than a silent disagreement.

## channel-per-part-waiters — a global waiter queue over a partitioned buffer

`Queues.strong.relaxed(parts, each)` — a relaxed buffer with BOUNDED
parts — measured 111546us at 16 producers against a single ring's
3150. Not a tuning problem: a design mismatch.

The channel keeps ONE queue of waiting senders while the resource is
per part. A consumer frees a slot in part 7 and wakes an arbitrary
sender, who finds its own part still full and parks again. With k
parts the chance of waking a sender that can proceed is 1/k, and the
rest is park/unpark churn. The consumer pays a scan across parts on
top.

`relaxedUnbounded` avoids the whole thing — parts that never fill mean
senders never park — and is the fastest lane in the file: 169.9us at
16 producers, 17.5x past `zio.Queue`, and FASTER at 16 than at 1.

The fix, if the bounded form is to be kept: waiters per part, so a
freed slot wakes a sender that can use it. That means the channel must
learn WHERE room appeared, which `Buffer` does not currently say —
`popMany` and `pop` would answer a route alongside their element, and
`wakeOne` would take one. Until then `relaxed` stays in the menu with
its measurement written next to it, and `relaxedUnbounded` is the one
to reach for.

Related: this is the fourth defect in this family, all the same shape
— the channel asking about the buffer as a whole where the question
belongs to one part. `isEmpty` vs `hasReady`, `size < capacity` vs
`hasRoom`, the route taken on the waker's thread, and now the wakeup
itself.

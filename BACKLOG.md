# Backlog

## okay-r: two claims the spec made that the module does not (spec-truth, 2026-09-09)

Found by auditing specs/r.md's Behavior list against the 24 tests that
exist: five boxes were already proven and are now checked, two are half
built and say so, and these two are simply absent. Neither blocks a
consumer today — okay-r is used through tools, where the caller's own
supervision applies — so they are named here rather than built on
speculation.

- [x] r-call-timeout — DONE 2026-09-09 (r-finish): the process killed at
      the deadline, a fresh one in its place, the call answered as a
      Condition; proven against the dockerized R. Was: `timeout` appears nowhere in okay-r's main
      sources: a hung `Rscript` hangs the calling fiber, with no way
      to report it as data. The seam already has the shape for it
      (`Condition` is the data channel, `Async.timeout` the mechanism,
      and `RSubprocess` owns the process it would have to kill). Spec
      box: "a timeout kills the call, reports as data, and the engine
      is usable after".
- [x] r-frame-schema — DONE 2026-09-09 (r-finish): `RFrame.rows[A]` /
      `RFrame.of[A]` over Schema, every mismatch a Condition naming the
      column, the field or the row. Was: `RFrame` is `Vector[(String, Vector[RValue])]`
      and okay-r names no `Schema` anywhere. The spec's box promises a
      frame mapping to a Seq of a flat case class and back, with a
      column the Schema does not name an error naming the column —
      the analyst-facing half of the frame story. The column round
      trip itself IS built and tested; this is the typed layer over
      it, the same move `Typed.rows` makes over `Sql`.

## bulk — after the seam (specs/bulk.md, landed 2026-09-09)
- [x] bulk-rewrite — DONE 2026-09-09, and the premise refuted by
      measurement (`TestWroclawStages`): the plan built in 6.8 s, same
      as DataFrames; the 18 s was `cache` through Java serialization.
      Landed instead: broadcast join for a small right side in
      `SparkBulk.join`, Kryo in the demo session — analysis 15.3 → 8.1 s.
      A whole-plan rewrite needs a first-order plan (`Pipeline`'s
      shape), because a `Free` continuation cannot be looked ahead of;
      filed below as bulk-plan if the need appears.
- [x] bulk-plan — DONE 2026-09-09: `Tables.Plan`, a heap of plans
      forced at actions, `Columns` pushed into `Read` (Spark 2.5 →
      1.3 s, local 1.9 → 0.95 s on stop_times ⋈ trips) and joins turned
      small-side-right by `Plan.estimate` (Spark 5.0 → 2.6 s, local
      3.3 → 2.2 s on three joins written the wrong way round).
- [ ] bulk-plan-next — with a workload that asks: a `Where` whose
      predicate is structural (a column equals a value) pushed under a
      join, and a size estimate for a held table from the count its
      `Cache` already made. Neither is worth a line until something
      measures for it.
- [ ] bulk-parquet — `Bulk.csv` is the only source; the taxi demo
      (TestTaxiAlgebra) still reads its parquet through Spark's API.
      A `source` per format, or `Bulk.read(Format)`, with the local
      instance reading parquet without Spark (okay-delta already
      carries a Delta Kernel road, specs/data.md).
- [ ] bulk-flink — `flink-core` alone carries no DataStream; an
      instance needs flink-streaming-java. The seam's `Any`-element
      choice is what a `DataStream[AnyRef]` instance would do too.
- [x] bulk-join-cost — CLOSED 2026-09-09 by bulk-rewrite: the join was
      not the cost, the persist was; see above.

## runner-floor — what is left under the fused pass (specs/handler-fusion.md, after the arc)

The handler-fusion arc closed 2026-09-09 with the floor measured: a
fused pass over `Free` is ~13.7 ns and ~122 B per operation
(`FusionBenchmark.fusedSWr`, 122 641 B/op per 1 000 ops). The
operator asked for the four things left under it, in this order:

- [x] writer-test-no-some — REFUTED 2026-09-09, no code landed. Built
      (ClassTag class test in the companion, Typeable fallback by given
      priority) and measured: 0 B/op on fusedSWr, nestedSWr and nestedWS
      to the byte — the JIT already scalarises `Typeable.unapply`'s Some
      on the runner path, and the fused loop never tests Writer at all
      (it tests State and takes Writer by exclusion). And the ClassTag
      road is UNSOUND for a union told type: `ClassTag[String | Int]` is
      the LUB (`Object`), so `Writer % (String | Int)` would claim every
      told value — the same failure TypeableK.derived refuses for rows.
      Was: `Writer`'s own `TypeableK` tests the told
      value through `scala.reflect.Typeable`, whose `unapply` answers a
      `Some` per tell (333 per run here). A `ClassTag`-based test where
      one exists (boxed for primitives), `Typeable` as the fallback,
      by given priority. Gate: B/op on `fusedSWr` drops by ~16 B per
      tell; TestRowIdentity's two-Writers row still misroutes loudly.
- [x] eff-stack-safety — DONE 2026-09-09: `Cont.Defer` + a deferring
      `Eff.flatMap`; a million left-nested binds run; cost +11% B/op,
      ~+14% time on Eff's right-nested path, kept with the reasoning and
      the one-line revert in specs/eff-stack-safety.md. Was: `Eff` is not
      stack-safe on left-nested binds
      (the Church encoding calls inward once per bind before any Cont
      exists). Measure the depth at which it dies, decide between a
      trampoline in `Eff.flatMap` (if one exists that is not "reify to
      Free") and a documented law + `fromFree` as the road; either way
      a test pins the answer.
- [x] either-scalarised-in-one-nesting — DONE 2026-09-09: explained (the
      Either escaped only in State.handle's loop; Writer's wrappers were
      always scalarised), and Writer.run moved to a List finished inside
      the loop (-13% / -35% B/op). specs/handler-fusion.md. Was: after
      stage A, the shipping
      runners saved both wrappers in `Writer.run(State.handle(p))` and
      only the Option in `State.run(Writer.run(p))`; bytecode has no
      `Left`/`Right` in `State$`. Per-runner lanes with the old `<|>`
      loops kept benchmark-local as the A/B; explain, then fix or
      record.
- [x] single-shot-row — PRICED AND REFUTED 2026-09-09: a mutable cell
      buys Writer's reverse and nothing else (-8.9% B/op on the mixed
      program, gate was 10%; -18.7% Writer-only); the evidence type is
      not shipped, specs/single-shot-row.md has the table. The
      runner-floor list is closed. Was: the only road under 122 B/op: a mutable cell
      in place of the threaded accumulator, which is sound only when
      no handler below resumes a continuation twice. A type-level
      evidence for that (per signature, derived for a row; NOT for
      Choice/Logic/List/Vector), the handlers that consume it, and the
      honest statement of what a user-written `handle` can break.
      SPEC FIRST, then the measured win on `fusedSWr`.

## resilience — the microservice handlers (operator's direction, 2026-09-09)

Stage 0 of specs/resilience.md landed (okay-resilience: Breaker,
Bulkhead, Limiter, Hedge, Deadline, one `Refused` type). What
follows, in the spec's order:

- [x] resilience-http — DONE 2026-09-09 (with resilience-metrics, one
      lane). Was: stage 1: `Resilient.http(inner, ...)` in the
      fixed order deadline → breaker → bulkhead → limiter → hedge, a
      5xx counting as a breaker failure, hedging safe methods only;
      `Resilient.route` mapping `Refused` to 429/503/504 with
      `Retry-After`, keyed on `Request.peer` (the field specs/http.md
      added for exactly this); `Deadline.read`/`carry` across two
      hops under a controlled clock. Spec: specs/resilience.md
      Behavior, stage 1.
- [x] resilience-metrics — DONE 2026-09-09 (in resilience-http). Was:
      stage 1's other half: okay-ops renders
      `Breaker.Stats`, `Bulkhead.Stats`, `Limiter.Stats` as
      Prometheus rows beside `Store.Stats`, `name` as the label;
      `Ops.routes` takes a `Vector[Reporting[?]]`. okayOps gains the
      okayResilience dependency (JVM + JS only — okay-ops is already
      JVM + JS).
- [x] resilience-faults — DONE 2026-09-09. Was: stage 2: `Faults.http(seed, plan)(inner)`,
      the seeded fault-injecting Http (delays, drops, 5xx by
      ordinal), and the composite under a plan behaving per the
      pieces' contracts. Adaptive concurrency stays deferred until
      stage 1 is in use somewhere.
- [x] timeout-masks-failure — DONE 2026-09-09: timeout on `await`, the
      first outcome of either kind settles it; law in TestAsyncCross on
      all three platforms, failed first on the old shape. Was: CORE.
      `Async.timeout(ms)(p)` is
      `race(p.map(Some), sleep(ms).map(None))`, and `race` lets a
      FAILING contender lose without ending the race: a program that
      fails at once under `timeout` comes out as `None` after the
      whole `ms`, its exception replaced by a timeout. Found by
      resilience-http (a breaker's refusal under `Deadline.enforce`
      became a 504 after 5 s); okay-resilience now races on its own.
      Decide the law — "a failure ends a timeout at once" reads
      right — write the test on `Async.timeout` first, then change
      `timeout` (not `race`, whose contract is stated and tested).
- [x] retry-js — DONE 2026-09-09: `Retry.async` in the shared core over
      Async.attempt + Async.sleep, three laws in the cross suite on all
      platforms. Was: `okay.retry` lives in scala-jvm-native and sleeps
      the thread; a JS twin over `Async.sleep` (Timer) would make the
      one resilience primitive the core already has cross-platform.
      Found by the resilience audit; not taken there because the
      module needed none of it.
- [x] deploy-termination-grace — DONE 2026-09-09 (deploy-stop-grace):
      `Health.stopSeconds` (30), rendered as Kubernetes's
      `terminationGracePeriodSeconds`, compose's `stop_grace_period`,
      systemd's `TimeoutStopSec` and the ECS task's `stopTimeout`; no
      preStop (the process answers the signal). Was: the rendered Kubernetes manifest
      (okay-deploy `Cluster`) should set `terminationGracePeriodSeconds`
      above the lifecycle's delay + grace (2 + 15 s by default → 30),
      and a `preStop` is NOT needed (the process handles SIGTERM
      itself). Regenerate the committed rendering (drift test).
      Found by service-lifecycle, not taken there.
- [ ] microservices-next — the audit's remaining gaps, each its own
      spec when picked. DONE 2026-09-09 (service-lifecycle): graceful
      shutdown and RED metrics, both in okay-ops. DONE 2026-09-09
      (outbox): transactional outbox / inbox / dead-letter as
      okay-outbox (specs/outbox.md). DONE 2026-09-09 (discovery):
      service discovery + client-side balancing in okay-resilience
      (specs/discovery.md). DONE 2026-09-09 (schema-compat): Schema
      compatibility between services, `okay.codec.Compat`
      (specs/codecs.md). DONE 2026-09-09 (obs-log): a Log effect with
      trace correlation, `okay.obs.Log` (specs/obs.md, "The third
      leg") — the audit's list is now closed except: saga over `Durable`
      + persist with compensations as values; transactional outbox /
      inbox / dead-letter when the truth is in SQL; service discovery
      + client-side balancing (cluster.md lists it out of scope);
      Schema compatibility checks between services; a `Log` effect
      with trace correlation (0 hits for one today).
## di — modules, wiring and the containers next door (specs/di.md; operator's ask, 2026-09-09)

Stage 0 landed in the core (Module over Providing and Resource,
TestModule). What follows, in the spec's order — each stage's gate is
its Behavior checklist:

- [x] di-qualifiers, di-plan, di-conf — stage 1, DONE 2026-09-09 as
      one lane (di-stage1): opaque-type roles, `m.plan` as a macro over
      the module's type (not TypeableK — see the spec's Decisions),
      the okay-conf example with a Secret resolved inside the
      acquisition.
- [x] okay-spring — DONE 2026-09-09: `OkaySpring.register`/`bean`,
      the ReactiveAdapterRegistry adapter + Boot auto-configuration;
      core gained `Resource.open` and `m.exports`. Not done, deferred
      to a lane if wanted: a WebFlux end-to-end test with a real
      server (the adapter is tested on the registry, the
      auto-configuration under ApplicationContextRunner).
- [x] zio-layer + okay-guice — DONE 2026-09-09 as one lane
      (di-bridges): `ZioLayers` in okay-zio, the new okay-guice
      satellite. CDI documented as the same shape, not built.
- [x] di-deploy — DONE 2026-09-09: `Needs[A]` + `Needs.of[Root]` in
      okay-deploy. The "di" arc of specs/di.md is closed at stages
      0-3.
- [x] di-tails — DONE 2026-09-09: okay-cdi (Weld SE tests) and the
      WebFlux end-to-end (handler stack in the gate, Netty under
      Live), which found and fixed the result-handler gap. Nothing
      left in this arc.
- [x] di-cross — DONE 2026-09-09: the module vocabulary pinned on
      JVM, JS and Native (`TestModuleCross`, src/test/scala-cross).
      Found nothing; the guard was what was missing. The arc is now
      complete on every platform it ships to.
- [x] di-dogfood — DONE 2026-09-09: ChatDemo wired by modules, the
      first application to use the arc. Found and filled two gaps in
      the vocabulary (`moduleAs`, `Module.use`) and one in the demo (a
      global store every reader reached for). specs/di.md "What using
      it taught". Stage 3 on the demo declined with a reason: its root
      needs a Timer, which no place provides.
- [x] needs-runtime — DONE 2026-09-09: `Needs` says which KIND of
      input each is (`Place` / `Runtime`), so a root whose inputs are
      mixed can be read without lying and without losing the
      undeclared-input error. Timer and Scheduler declared once in the
      companion; ChatDemo.Root named and pinned by okay-demo's test.

## persistence-audit — what the database layer still lacks (operator's go, 2026-09-09)

The audit (2026-09-09) read every seam: `Sql` (JDBC, pg wire, R2DBC),
`Topic`/`Store`, `Docs` (Mongo, TopicDocs), `Cache`/`View`, `Blob`,
Delta, migrations, bulk load. Local transactions are right in shape
(region + sync brake + compile-time nested refusal + granted
isolation); XA/2PC stays refused (specs/jdbc.md, specs/data.md) and
the saga-over-journal answer stands. The gaps are concrete, ordered
correctness first:

- [x] sql-commit-tag — DONE 2026-09-09 (988b4ab6): pg-wire COMMIT reads
      its tag and throws on ROLLBACK; r2dbc-postgresql already refused.
      Was: an error inside a region that the BODY
      handles (no unwind) leaves Postgres in the aborted state; its
      `COMMIT` then answers the command tag `ROLLBACK` with NO error,
      and `PgSql.commit` ignores the tag (`simple("COMMIT")`), so the
      region reports success on a transaction that rolled back — the
      exact "rollback that does not roll back" specs/jdbc.md refuses.
      Test first (TestPg, Live): handled error, commit, row absent,
      region must FAIL. Fix: read the `C` tag on COMMIT, throw on
      ROLLBACK. Same probe through R2DBC-postgresql (R2dbcSuite) and
      note what pgjdbc does. Spec: specs/sql.md Behavior.
- [x] sql-serialization-retry — DONE 2026-09-09 (233ce589): Sql.sqlState per
      driver, Async.attempt, Typed.transactRetry with Retried(value, attempts).
      Was: SQLSTATE 40001/40P01 is handled
      nowhere (0 hits in main sources). Under RepeatableRead/
      Serializable a serialization failure is a NORMAL outcome the
      region must retry, not an exception the program sees.
      `Typed.transact(..., retry = Retry(max, backoff))` re-runs the
      BODY (a program value — re-runnable by construction) on 40001/
      40P01/deadlock, with the count exposed; anything else propagates.
      The driver names the class: `Sql.Failure.Serialization` from a
      pg ErrorResponse's SQLSTATE and from `SQLException.getSQLState`.
      Test: two connections, SSI conflict on pg (Live), and a fake
      driver that fails N times (unit).
- [x] sql-temporal-types — DONE 2026-09-09 (e3c7d563, 728ceb37): the five
      cases, Temporal, java.time givens on the JVM, three drivers, Delta;
      the H2 Calendar road refuted on the way. Was: `SqlValue` has no timestamp/date/time/uuid/
      json: they travel as Text under `SqlType.Other`, so every
      `created_at timestamptz` is hand-parsed per field. Add
      `SqlValue.Timestamp(Instant)`, `Date(LocalDate)`, `Time`,
      `Uuid(UUID)`, `Json(String)` with `SqlType` mirrors; Schema
      givens for the java.time types and UUID; both drivers encode/
      decode; verify accepts the pairs. Keep Text←Timestamp as the
      lossless fallback so nothing that reads today stops reading.
- [x] sql-readonly-region — DONE 2026-09-09 (0777e790): readOnly on begin and
      the regions, Granted.readOnly read back per engine, JdbcSql restores
      isolation + readOnly with autocommit. Was: (a) `transact(readOnly = true)`: `SET
      TRANSACTION READ ONLY` on pg, `Connection.setReadOnly` on JDBC,
      the FOREIGN posture's honest declaration where the DBA gave us
      reads; (b) `JdbcSql.commit/rollback/cancel` restore autocommit
      but NOT the isolation level — after `transact(Serializable)`
      every later autocommit statement on the connection runs
      Serializable. Save and restore it beside `autoBefore`.
- [x] sql-pool — DONE 2026-09-09 (46498266): okay.sql.Pool, borrow/pinned/stats/
      close, cancel-safe hand-off, the brake on return. Was: no connection pool anywhere; one `Sql` = one
      connection. A `Pool[Sql]` as a Resource: `borrow` hands a
      connection to a program and returns it after, a region pins one
      for its scope, size + acquire timeout, a health probe on return
      (a connection whose transaction is still open is rolled back,
      not returned). Hikari behind the JDBC driver as the interop
      hatch; the pool itself is driver-neutral (pg wire has none).
- [x] persist-saga — DONE 2026-09-09 (9e8ef60d): okay.persist.Saga, steps with
      compensations, intent-first on a keyed topic, recover by Forward/Backward
      policy, Stuck, status. Was: the multi-item change is spec'd as "a journaled
      sequence of conditional writes" (Docs.scala, specs/data.md) but
      nothing packages it: every consumer hand-rolls steps,
      compensations and recovery. `Saga` over `Durable.Journal` +
      CAS: steps with a compensation each, intent journaled before
      each step, `recover` replays the tail (finish forward or
      compensate back, a declared policy), status as a Schema value.
      Test: crash between steps (journal cut), both policies.
- [x] docs-dynamo — DONE 2026-09-09 (c4831720, 5ad9b24c): okay-docs-dynamo, the
      DocsSuite contract Live on dynamodb-local. Was: `Docs` was designed for Dynamo/Cassandra/Mongo and
      is implemented on Mongo only; the seam has not met condition
      expressions or eventual reads. DynamoDB adapter over the REST
      API with SigV4 (okay-blob already signs): `Cond` → condition
      expressions, `grants(Quorum)` → ConsistentRead, declared indexes
      → GSIs. Live against dynamodb-local in docker.

## handler-fusion — one composite handler for a row, staged at compile time (specs/handler-fusion.md)

The operator's proposal, 2026-09-09, assessed in the spec: compose a
row's handlers into ONE handler first, run once, stage the composite.
Already the design for comonadic rows (Handler.union + runFree is one
pass); the continuation-aware class (Writer/State/Throws/Choice/
Reader.local) runs one effect at a time and rebuilds every foreign
operation once per pass. Fold fusion (Wu & Schrijvers 2015), evidence
passing (Xie & Leijen 2020/2021) — licensed by the initiality Free/Eff
already claim. Order is semantics; the product state is immutable;
compile-time inline is the ONLY staging admitted (staged-effects.md
refuted run-time closure composition 3/3).

- [x] handler-fusion-gate — DONE 2026-09-09, GATE NOT CLEARED (1.13–1.29x
      against a 1.3x bar; specs/handler-fusion.md Results has the table
      and the corrected cost model). Was: STAGE 0, the measurement gate: a
      hand-written fused loop for `State % S + Writer % W` (one
      @tailrec match, product accumulator, immutable) against
      `State.handle(s)(Writer.run(p))` and the other nesting, JMH
      µs/op AND B/op (-prof gc), N=1000, plus the three-effect row
      (+ Throws, no abort taken). Agreement test on generated
      programs for BOTH orders. Threshold ≥ 1.3x; below it the
      spec's Results record the refutation and the stages below
      are not built.
- [x] split-without-either — DONE 2026-09-09: -26.7 KB/op, 7–11% on the
      hot loops, zero churn for the walks; specs/handler-fusion.md Stage A.
      Was: THE LEVER STAGE 0 FOUND: `<|>` allocates
      an `Either` per operation in EVERY runner (≈20 KB of the 149 KB a
      fused right-nested pass allocates for 1000 ops). A split that
      answers by a flat class match with no wrapper — for nested and
      fused runners alike — is the per-operation cost fusion cannot
      touch, and the next thing to price. Measure on `Fused.stateWriter`
      first (the loop is small and its B/op is known to the byte), then
      on `State.handle`/`Writer.foldWith`.
- [ ] handler-fusion-flat — GATED OFF by stage 0 (the ceiling for pass
      fusion measured 1.13–1.29x); reopen only with a new number. Was:
      `Handler.flat`: Handler.union assembled
      inline so the nested <|> chain unrolls to one match; measured
      on the four-effect agent row, fourth position is the number.
- [ ] handler-fusion-step — GATED OFF by stage 0, same reason. Was:
      `Step[F, Acc]` (tail-resumptive by type)
      and `Fused.run` over `F + G` with the row-shaped product state;
      instances for State, Writer (Fold-generic), Reader incl. local;
      abort/choose fall back to a shift with the state captured
      immutably; laws: agrees with nested for both orders, stack-safe
      at 1M, multi-shot and abort survive.
- [x] handler-fusion-eff — DONE 2026-09-09, REFUTED: Eff + composite is
      0.58x of the fused Free loop, 2.4x the bytes; the best tree-free road
      0.86x (specs/handler-fusion.md Stage B). The arc is closed. Was: the
      same composite `!>` for Eff (no tree),
      after the Free loop has its numbers.

## flush-premium — `flushAfter` costs 30% over the chunked merge where the page said 9%

Found 2026-09-08 by `bench-stale-tables`, while correcting prose that
quoted a table which had just been re-measured. Not investigated by
that lane and not by any since.

`okayChunkedFlush` reads 382 against `okayChunked`'s 293 at k = 16 —
a 30% premium for bounding how long a partial chunk may wait. §6b has
said 9% (244.3 against 223.7) since 2026-09-06.

- [x] flush-premium — FIXED 2026-09-09. The two flusher fibers were
      forked and DROPPED, so `done.get` stopped them only at their
      next tick: a merge finishing in 380 microseconds left two fibers
      asleep for a further second under `flushAfter = 1000`, each
      holding a timer entry, and at a few thousand merges a second
      that is thousands of live sleepers.
      Identified by an INVERSION: a 1 ms window, which does strictly
      more work because its timer actually fires, measured 1.14x where
      the 1000 ms window measured 1.29x. A shorter window costing less
      is not something a correct implementation can do.
      Each flusher is now cancelled when its own source finishes — it
      has already flushed its tail by then. Premium 1.29x -> **1.11x**
      (`okayChunkedFlush` 400.0 -> 349.4, the `okayChunked` control
      +1.5%). The 11% left is two forks and two timer registrations
      per merge, which is work rather than waste.

      (as filed) STILL OPEN, and now with a firm number instead
      of a suspicion. Six rounds, tight bars (spread 1.07x and 1.10x
      within a lane): `okayChunked` 314.5 min / 325.4 median,
      `okayChunkedFlush` 386.8 / 404.1 — a premium of **1.23x**, not
      the 9% §6b has claimed since 2026-09-06 and not the 30% the
      re-measure suggested. 9% is outside the bars: 314.5 x 1.09 is
      342.8 and the flush lane starts at 386.8.
      I was asked to close this and the measurement refused: my own
      prediction was that the bars would overlap and it would be
      noise, and it is not. It stays open as a defect with a number.
      What remains is the original question — The ratio to both competitors
      stayed comfortably in okay's favour (15x ZIO, 41x fs2), which is
      presumably why nobody looked, and is also why this is a
      curiosity rather than a defect.
      DISQUALIFYING: `okayChunked` improved 13.7% in the same run
      under the new default. If the whole 9% -> 30% is the DENOMINATOR
      moving, there is nothing wrong with the flusher and the entry
      closes as arithmetic.

## growing-elementwise-pop — a partitioned buffer pays a part scan per pop, and only chunked consumers amortise it

Found 2026-09-08 by the A/B for `growing-default`, after
`growing-part-sizing` was fixed and the regression did NOT move.

`Source.merge` reads **3.5x slower** when `Channel.apply` defaults to
`growing`, and every control is flat:

| lane | ring | growing | |
|---|---|---|---|
| `okaySourceMerge` | 135.7 | 471.9 | **+248%** |
| `sourceMerge` n=250..2000 | | | **+189% to +260%** |
| `okayChannelMerge` | 85.8 | 86.7 | +1.0% |
| `okayChunksMerge` | 13.7 | 13.4 | −2.3% |
| `sourceSingleDrain` | 98.5 | 99.2 | +0.7% |

WHY THOSE TWO AND NOT THE OTHERS, established rather than guessed:

- `Channel.merge` defaults to `capacity = Int.MaxValue`, which takes
  `Channel.apply`'s `> MaxRing` branch and gets `Segments` — the
  growing default never touches it. That is why it is flat.
- `Source.merge` defaults to `capacity = 64`, takes the ring branch,
  and therefore becomes `growing`.
- Buffer SIZE is not the cause: with `growing-part-sizing` each part
  is a full 64 and the regression is unchanged.
- `growing_chunk` — the same buffer, many producers, a CHUNKED
  consumer — is not merely fine but the best on the page (126 at
  sixteen producers). `Source.merge` consumes PER ELEMENT.

So the cost is the per-element pop on a partitioned buffer: each one
scans parts for a ready element, where a ring pops one. A chunked
consumer pays that scan once per chunk; a per-element consumer pays it
per element.

- [x] growing-small-capacity-merge — CLOSED 2026-09-08, there was no
      such problem. The capacity sweep it asked for found `Source.merge`
      at capacity 64 reading 127.7 against the ring's 127.5 — no
      regression — and 20% ahead at 256 and 1024. The 3.5x that
      created this entry came from a diagnostic build that bypassed
      the sizing fix. Original text kept below.

      (superseded) RENAMED and re-scoped
      2026-09-08 after THREE hypotheses were measured and refuted. The
      name it had, `growing-elementwise-pop`, was one of them.

      REFUTED, each with the evidence, so nobody retries them:
      1. "a part scan per pop". `AdaptiveFifo.pop` already takes a
         straight line at `open == 1` and `popScanning` already starts
         from a remembered `startAt`; at two producers it scans at
         most two parts. Not 3.5x.
      2. "the parts are too small". Fixed in `growing-part-sizing`
         (each part now a full `capacity`, worth 70% at four and
         sixteen producers) and the regression did not move at all.
      3. "a per-element consumer cannot amortise it". `Source.merge`
         is NOT per-element on the channel: it reads through `Drain`,
         in batches, and its own comment says so.

      AND THE CONTROL WAS NOT A CONTROL. `okayChannelMerge` looked
      flat under the growing default because `Channel.merge` defaults
      to `capacity = Int.MaxValue`, which takes `Channel.apply`'s
      `> MaxRing` branch and gets `Segments` — the growing default
      never touched it. The only lane in that A/B that exercised the
      change at all was the one that regressed.

      WHAT IS ESTABLISHED. At capacity 1024 with a chunked consumer,
      growth pays at every count including TWO — `oneRing` 772.4
      against `growing` 188.0, 4.1x (a `producers=2` lane was added
      for this and did not exist before). At capacity **64**, two
      producers, through `Channel.merge`, growing is 3.5x slower. The
      profile says the time is in WAITING and that the proportions are
      unchanged — 4.8x more of the same, not different work.

      WHAT IS NOT. Why 64 behaves unlike 1024. The untested suspect is
      the merge's READINESS interleaving: with two parts the consumer
      drains the part it last drew from, so one producer's part fills
      and it blocks where a shared ring would have let both through.
      That is a hypothesis and is written as one.
      NEXT STEP: sweep `Source.merge`'s capacity (64, 256, 1024) under
      both buffers. If the regression vanishes as capacity rises, the
      answer is a floor below which the default does not partition,
      and it is one line.
      BLOCKS: `growing-default`.

## growing-part-sizing — `growing` divides capacity by `parts` however few producers arrive

Found 2026-09-08 by the A/B that `Channel.apply`'s own comment had
demanded for two days and nobody had run: every single-producer path
the default feeds, with the plain ring against `growing`.

| lane | ring | growing | |
|---|---|---|---|
| `okaySourceMerge` | 136.4 | 475.7 | **+249%** |
| `sourceMerge` n=250 / 500 / 1000 / 2000 | 74.7 / 133.6 / 273.3 / 559.5 | 208.6 / 478.4 / 990.5 / 1933.7 | **+179% to +262%** |
| `channelMerge` (control) | 175.0 | 177.6 | +1.5% |
| `okayChunksMerge` (control) | 13.6 | 13.6 | +0.3% |
| `sourceSingleDrain` (control) | 101.5 | 98.1 | −3.4% |

Everything with ONE producer is unmoved. `Source.merge` — which runs
exactly TWO, one fiber per side — is 3.5x slower.

CAUSE, and it is arithmetic rather than a race. `Source.merge` uses
`capacity = 64`. `growing(64, parts = 8)` builds part 0 at 64 and
every later part at `64 / 8 = 8`. So the moment the second producer
appears it is handed a buffer of **eight elements** where the plain
ring gave it 64, and six more parts are sized and never opened.

`growing` divides the capacity by the part count it MIGHT need rather
than the one it has. At sixteen producers that is right and it wins
7x; at two it is a 3.5x loss.

- [x] growing-part-sizing — DONE 2026-09-08, landed as 51adaf00, and
      this box went unticked until the operator asked what was left.
      Every part is now a full `capacity`; parts open lazily, so a
      channel holds `capacity x producers that actually arrived`.
      Worth 70% at four and sixteen producers (552 -> 165, 429 -> 126)
      and nothing at one. It is also what unblocked the default,
      though a broken diagnostic hid that for a day.

      (the original plan) size a part for the producers that
      actually arrive, not for `parts`. The obvious shapes, in
      increasing cost: give every part the full `capacity` (that is
      what `adaptive.parts(n).each(c)` does, and it is `n * c` of
      memory — see `growing-capacity-semantics`); or open parts at
      `capacity` and let total memory grow with the producer count;
      or halve the surviving parts as each new one opens.
      Expected win: `Source.merge` back to the ring's number, and the
      default switch unblocked.
      DISQUALIFYING: whatever is chosen changes how much memory a
      channel holds, which is a contract, not a tuning. Measure the
      one-producer paths again after — this entry exists because that
      is the check that caught it.
      BLOCKS: `growing-default`.

## growing-default — make `growing` the default behind `Channel.apply`

Operator decision 2026-09-08. The performance case is made at an equal
memory budget: `growing` ties the ring at one producer (171 against
169) and is 2.4x and 7.2x ahead at four and sixteen, while `adaptive`
as a default is refuted — it splits the budget into parts, so a lone
producer gets 64 slots of 1024 and reads 1 119, 6.6x the ring.

- [x] the exact-FIFO builder — LANDED. `Queues.strong[A].fifo(
      capacity)` is the single ring under a name that says what it
      gives, for callers who need order BETWEEN producers once the
      default stops promising it.
- [x] the switch itself — DONE 2026-09-08. `Channel.apply` builds
      `growing(capacity, 8)`. 1.10x at one producer, 4.1x / 7.9x /
      22.8x better at two, four and sixteen.
      The block was MY ERROR: the A/B that reported `Source.merge`
      3.5x slower built `Growing` directly inside a diagnostic
      `Channel.apply` with `Ring(capacity / 8)`, bypassing
      `Queues.Mechanism.growing` where the sizing fix lived — so it
      re-measured the old sizing and I read that as evidence. Swept
      properly, `Source.merge` under the fixed sizing reads 127.7
      against the ring's 127.5 at capacity 64 and is 20% AHEAD at 256
      and 1024. A variant that reimplements the code under test does
      not test it.
      Three Scala Native gaps surfaced the moment `AdaptiveFifo`
      became reachable from the default and are fixed with it:
      `AtomicIntegerArray`, `Thread.threadId()`, and
      `ThreadLocal.withInitial`, plus `Thread.onSpinWait()` removed.

## default-not-measured — three lanes where the library already holds a faster answer and the default does not pick it

Found by `bench-refresh` (2026-09-08, docs/benchmarks.md §4, §4b).
One defect, three appearances, which is why they are one entry: in
each case okay ships something that beats every competitor on the
lane, and the DEFAULT path chooses something slower. The fix is a
choice, not an optimisation, and the numbers to choose by exist.

TRIMMED 2026-09-08 (forkjoin-pairing): the fork/join row was a
MISMATCHED PAIR — okay's outside-the-runtime shape against kyo's
inside one — and the cancel row is the same asymmetry one step
milder. Both are corrected in §4b and neither is a defect. What
survives is the channel default.

| lane | the default | what okay already has | best competitor |
|---|---|---|---|
| many-to-many 4x4, one channel | 4806 | adaptive **870** | zio 3106 |
| many-to-many 16x16 | 9325 | adaptive **1042** | zio 6325 |

- [x] default-scheduler-shape — REFUTED 2026-09-08 by its own
      disqualifying evidence, and the entry was built on a mismatched
      pair besides. Matched by shape, okay is AHEAD of kyo in both:
      1957 against 33 900 outside the runtime, 796 against 884 inside
      it, and 3218 against 26 260 inside it at `work=10000`. And the
      adaptive default this entry accused of choosing badly reads
      796 / 3218 where its own fixed policies read 709 / 27 640 and
      2618 / 3200 — it already picks best-of-both, at a 12% premium
      over whichever policy wins each end. Nothing to fix. §4b now
      carries both rows instead of the one that mixed them.
- [x] channel-default-adaptive — ANSWERED 2026-09-08, and the answer
      was NOT adaptive. The default became `growing` (17404a4e):
      1.10x at one producer, 4.1x / 7.9x / 22.8x better at two, four
      and sixteen. `adaptive` as the default is refuted by the same
      run — it splits its capacity across parts, so a lone producer
      gets a fraction of the buffer and reads 1 119 against the ring's
      169, which is exactly the hazard `Channel.apply`'s own comment
      had predicted. Exact FIFO across producers, which the ring gave
      and this does not, is now `Queues.strong[A].fifo(capacity)`.

      (as filed) UNBLOCKED 2026-09-08: `Growing`'s
      one-producer premium is now 11%, not 31% (growing-wrapper-cost),
      so the trade the default has to make is 11% at one producer
      against 5.5x and 9x at four and sixteen. That is a decision, not
      an optimisation, and it is the operator's: `Channel.apply` gives the plain ring;
      the adaptive buffer reads 870/1042 where the ring reads
      4806/9325 (4x4 and 16x16). Expected win: 5.5x and 9x at those
      shapes. DISQUALIFYING: the ring is FASTER at one producer (see
      docs/queues.md — 123 against the partitioned 144), which is the
      case the default is presumably chosen for. A fix must show the
      one-producer case does not regress, or must adapt rather than
      switch. BLOCKED ON `growing-onep` below: `Growing` is the
      mechanism that would let the default adapt, and it is broken.

## growing-onep — `Growing` grows with ONE producer, because it mistakes the consumer for a second one

Found 2026-09-08 while asking why `Growing` costs 53% over the ring at
one producer (192 against 125.6, where until a second producer appears
it IS the ring and should cost what the ring costs).

MEASURED, not reasoned. A probe running the benchmark's own shape —
one producer, 8 000 elements, capacity 1 024, producer and consumer
concurrent — over 30 repetitions:

| outcome | runs |
|---|---|
| grew DURING the send, with one producer | **10** |
| grew at close (the sentinel push) | 0 |
| never grew (the correct outcome) | 20 |

So a third of runs silently become an `AdaptiveFifo`, and the lane's
192 is not "a ring plus a wrapper" — it is a half-grown buffer, which
is why it sits near `adaptive`'s 158.5 rather than near the ring's
125.6. A JMH `-prof stack` of the lane shows the giveaway directly:
`AdaptiveFifo.popManyScanning` frames at `producers=1`.

CAUSE. `Growing` identifies a producer as `Thread.currentThread()` at
push time, in both `sample()` and `refused()`. But when the ring is
full `SentinelChannel.attemptSend` parks the sender behind a
continuation — `Waiter(() => attemptSend(a, granted0 = true, route)(k))`
— and that continuation is run by `wakeSender()`, ON THE CONSUMER'S
THREAD. The resumed push therefore arrives with the consumer's
identity, `Growing` sees a thread that is not the one it sampled, and
grows. One real producer, two apparent ones.

- [x] growing-onep — FIXED 2026-09-08 as `growing-onbehalf`, shape
      (a). `Buffer.pushDecidingAtOnBehalf` (default: the ordinary
      push), `Growing` overriding it to neither sample nor grow,
      `SentinelChannel` calling it when its `granted` flag is set. The
      law moved to the layer that broke: through a CHANNEL with a
      concurrent consumer, since the two existing one-producer tests
      drive the Buffer directly from one thread and could not see it.
      Fails on the old code in 32 ms, passes on the new.
      **It bought no speed, and the entry says so.** growing/ring at
      one producer went 1.53x -> 1.47x over six rounds with the ring
      as the in-run control — inside the noise. The arithmetic agrees:
      adaptive is 1.20x the ring and the spurious growth hit a third
      of runs, so it was worth ~6 points of the 53. THE REMAINING ~45
      IS THE WRAPPER'S OWN DISPATCH and is still open below.

- [x] growing-wrapper-cost — DONE 2026-09-08, and this entry named
      the wrong causes. It blamed "one extra virtual call and a
      `@volatile inner` read". MEASURED, eight rounds on a
      verified-quiet box with the ring as the in-run control:

      | lane | vs ring | |
      |---|---|---|
      | `forwarded` — a buffer that ONLY delegates | 1.02x | the call layer is FREE |
      | `onePart` — partitioned, one part, cannot grow | 1.17x | a separate question |
      | `growing` | 1.31x | |

      Then the 29% split by alternating diagnostic builds, five rounds
      each: removing `sample()` -> 1.03x (**-22 points**); removing
      the `volatile` -> 1.30x (**-5 points, i.e. nothing**). So the
      volatile is free, the extra call is free, and the whole cost is
      the per-push sample counter.

      And 60% of THAT is false sharing: the producer stores `seen` on
      every push into the object the consumer loads `inner` from on
      every `popMany`. Moving the counter to its own padded object,
      semantics untouched: **1.200x -> 1.109x**, with 1.049x (no
      sampling at all) as the floor. Landed.
      The remaining ~6 points are the branch itself and are left
      alone; a trick there would buy noise.

- [~] growing-grown-cost — PART DONE 2026-09-08. `sample()` kept
      storing its counter on every push FOR EVER: the `!grown.get`
      guard sat inside the every-64th branch, so the store outlived
      the one swap it existed to trigger. At sixteen producers that is
      sixteen threads storing to one line — contention, not the false
      sharing the padding answers. The counter now stops at the swap,
      gated on a RACY plain hint (a producer still reading `false`
      does a useless increment; the real decision re-reads the
      AtomicBoolean).
      Measured, alternating, four rounds, `adaptive` stable to 1% as
      the control: **-24.1% at sixteen producers, -8.5% at four,
      +1.8% at one** (inside the noise; a first run had suggested a
      +11.4% penalty there and it did not reproduce).
      Ratio to the buffer it grows into: 4.80x -> **3.62x** at
      sixteen, 3.45x -> 3.19x at four.
      WHAT WAS LEFT was 3.6x against `adaptive`, and it turned out
      NOT to be mechanism at all: the two lanes differ 8.3x in buffer
      capacity. At matched capacity `Growing` is at parity with the
      buffer it grows into (0.97x / 0.98x). See
      `growing-adopted-part0` below, refuted 2026-09-08.

- [x] growing-adopted-part0 — REFUTED 2026-09-08, and there was never
      a defect. The entry rested on a MISMATCHED PAIR, filed by me:

        growing_chunk   Growing(Ring(1024), 16, () => Ring(1024/16))
                        part 0 at 1024, fifteen more at 64 -> 1 984 slots
        adaptive_chunk  AdaptiveFifo(16, () => Ring(1024))
                        sixteen parts at 1024              -> 16 384 slots

      `adaptive` had 8.3x the buffer. A new diagnostic lane,
      `adaptiveSmall_chunk` — the same mechanism with parts the size
      `growing`'s grown parts actually are — settles it. Four rounds,
      quiet box, medians:

      | producers | growing | adaptiveSmall | adaptive | growing/small |
      |---|---|---|---|---|
      | 1 | **178.7** | 1149.6 | 174.6 | **0.16x** |
      | 4 | 527.4 | 543.1 | 172.1 | 0.97x |
      | 16 | 422.0 | 428.7 | 118.2 | 0.98x |

      At matched capacity `Growing` is at PARITY with the buffer it
      grows into — marginally ahead. The whole 3.06x/3.57x was buffer
      size. The adopted part 0 is not a cost; it is the design, and
      the one-producer column prices it for the first time: **6.4x
      faster than a partitioned buffer of the same capacity** (178.7
      against 1149.6), which is exactly what `Growing` exists to buy.

      WHAT IS LEFT is not performance but SEMANTICS, and is filed
      below: `growing(capacity, parts)` and `adaptive.parts(n).each(c)`
      spell capacity differently, so two lanes that look comparable
      are not.

- [x] growing-capacity-semantics — CLOSED 2026-09-09 by RENAMING, once
      `growing-part-sizing` had made the behaviours agree. The
      parameter is now `each`, the same word `adaptive.parts(n).each(c)`
      uses for the same thing, and the scaladoc says it is per part
      and that parts open lazily. There was no behaviour left to fix:
      since 51adaf00 both builders give `each` per part; only the
      names disagreed, and that is what cost two false starts.

      (as filed) `Queues.strong[A].growing(capacity,
      parts)` gives `capacity` for part 0 and `capacity / parts` for
      every other part — about 2x `capacity` in total once grown —
      while `adaptive.parts(n).each(c)` gives `n * c`. A caller
      reading the two builders side by side has no way to see that,
      and the benchmark lanes built from them differ 8.3x in buffer
      while looking like a mechanism comparison. It cost this session
      two claims.
      Not a defect in either buffer; a decision about what the word
      `capacity` promises. DISQUALIFYING: if `capacity` is documented
      somewhere as per-part rather than total, then the builders are
      consistent and only the benchmark lanes and docs/queues.md need
      the note.

  (superseded plan, kept for the record) fix the identity, not the symptom. Two shapes,
      and the choice is a DESIGN decision on a knob the operator
      personally decided to keep (see the `growing` CHANGELOG entry),
      so it is filed rather than taken unilaterally:
      (a) the channel tells the buffer that a push is a RESUMED send
          rather than a fresh one — `attemptSend` already carries
          `granted0` and already captures `route` at entry, so the
          information exists and only the Buffer API lacks a way to
          pass it;
      (b) `Growing` stops deriving identity from the calling thread
          altogether and takes it from the route the channel captured
          when the send ENTERED — which is already per-producer for
          `AdaptiveFifo`, but is a constant 0 for a plain `Ring`, so
          this needs the ring to carry a producer token it does not
          have today.
      DISQUALIFYING for both: if after the fix `growing` at one
      producer does not approach the ring's ~126, the thread identity
      was not the whole cost and the remaining gap is the wrapper's
      own dispatch — measure before claiming the fix worked.
      NOTE the second prize: `growing_chunk` at 4 and 16 producers
      reads 601.6 and 623.6 against `adaptive`'s 162.1 and 116.6, so
      even when growth is CORRECT the grown buffer is 3.7x and 5.3x
      off the thing it grew into. That is a separate question and is
      not answered here.
- [x] cancel-default-drive — CLOSED 2026-09-08 as not-a-defect. The
      1116 is Loom's thread INTERRUPT; cats does its thousand cancels
      inside one `unsafeRunSync`. okay's matched lanes are the pool
      ones — `cancel1k_okayOwn` 750 against cats' 748 is a tie, and
      `cancel1k_okayDrive` 597 is the best number in the block. The
      benchmark's own comment predicted this ("§4b blamed the
      interrupt, and this is the lane that says whether it was
      right"). The lane count still says every cancel is delivered,
      so `drive` is not winning by doing less.

## bench-sendbulk-inverted — `sendManyNow` used to be 1.63x ahead and is now 17% behind

Found by `bench-refresh` (2026-09-08, §15). The page says "batch both
ends or neither" and prices the bulk send at 1.63x against a draining
consumer. Re-measured, the pair has INVERTED:

| lane | now | as §15 recorded it |
|---|---|---|
| `okaySendBulkRecvChunk` | 63.2 | 66.9 |
| `okaySendElemRecvChunk` | **54.1** | 109.0 |

Consistent across all three rounds (63/74/78 against 55/54/57), so it
is not the host. Note WHAT moved: the bulk lane is where it was; the
ELEMENT lane halved and overtook it.

- [x] bench-sendbulk-inverted — CLOSED 2026-09-09 as "no cause in the
      mechanism". Profiled: both lanes are ~60% WAITING and their
      RUNNABLE time is dominated by `Ring.popMany` on the CONSUMER
      side; neither `pushMany` nor its scan appears at all. A variant
      that stopped the per-element wake loop early moved the bulk lane
      8.5% the wrong way and the element lane — which never calls
      `sendManyNow` — 7% the right way, i.e. noise on lanes that
      spread 64-92 across rounds. Four explanations, four
      refutations. §15's "1.63x ahead" is withdrawn; the primitive
      stays, since nothing shows it is wrong, only that it is no
      longer faster where the page said. Original text below.

      (superseded) STILL OPEN, but narrower: the
      disqualifying question is ANSWERED and the answer was no.
      Measured 2026-09-08 against a chunk-draining consumer, same N,
      Cap and Batch as the lane, 300 reps / 22 090 `sendManyNow`
      calls: the scan finds room in **97.3%** of calls, mean claim
      **55.8 of 64**, 18 364 calls take the full 64, and only
      **0.05% of elements** hit the per-element fallback. So the
      fallback is NOT the cause; the bulk path runs almost perfectly
      and is still 17% slower than sending one at a time.
      §15's text is corrected to say this. What remains is to find
      where the bulk mechanism spends it. Standing hypothesis, NOT
      measured: `Ring.pushMany` touches every slot twice — once to
      scan its stamp, once to write — to save a tail CAS that is
      uncontended with a single producer. NEXT STEP IS A PROFILER,
      not a rewrite; the `performance` skill's rule applies, a hot
      frame is a place to look and never a size of prize.

## bench-chunk-fold-lane — a lane named `_chunk_` costs more than its `_elem_` twin

Found by `bench-refresh` (2026-09-08, §6c). In
`IdiomaticApiBenchmark`:

| lane | us/op |
|---|---|
| `okayChannelForeach_chunkNative_runForeach` | **20.1** |
| `okayChannelForeach_elem_runForeach` | 196.8 |
| `okayChannelForeach_chunk_fold` | **364.5** |
| `zioChannelForeach_chunk_runForeach` | 129.7 |

The `_chunk_fold` lane is 18x its own chunk-native twin and 1.9x the
ELEMENT lane, and against ZIO that pairing reads as a 2.8x loss where
the chunk-native pairing is a 6.4x win.

- [x] bench-chunk-fold-lane — ANSWERED 2026-09-08, and the answer was
      already in the tree when this was filed. The comment over the
      lane in `IdiomaticApiBenchmark.scala` says in capitals: "A LANE
      THAT DOES NOT MEASURE WHAT IT LOOKS LIKE, kept with the
      explanation rather than deleted... `.drained` already batches
      internally through `receiveMany`. Putting `.chunked()` on top of
      it adds a layer instead of removing one." So the lane is a
      deliberate diagnostic, the 2.8x "loss" to ZIO is a pairing that
      does not exist, and there is nothing to optimise. NOT A DEFECT.
      Filed in error: the entry itself said READ THE LANE BODY FIRST
      and it was filed from the results table without doing so.

## bench-producer-inverted — `Producer` beats `LazyList` in one suite and loses in the other

Found by `bench-refresh` (2026-09-08, §5 against §8).

| suite | `okayProducer` | `okayLazyList` |
|---|---|---|
| §8 generator, per element | **19.5** | 35.5 |
| §5 pipeline, map/filter/take/sum | 188.1 | **167.5** |

Same two representations, opposite ordering, both stable across three
rounds. The `performance` skill calls a disagreeing twin the single
most informative pattern in a ratio table, and this is one.

- [x] bench-producer-inverted — ANSWERED 2026-09-08, and it is not an
      inversion of anything. The two suites do not ask the same
      question: §8 pits okay's `fibs` over two okay carriers, while
      §5's LazyList lane uses the STANDARD LIBRARY's map/filter/take
      on a `LazyList`, not okay's combinators. And the 188-vs-57 gap
      INSIDE §5, between `okayProducer` and `okayIterator` over the
      identical source, is stated in the paragraph above that table:
      `toLazyList` is "the memoized, re-observable bridge — you pay
      for the caching", `.iterator` is "linear, fused, consume-once".
      A documented price, not a defect. Filed in error, from the
      results table, without reading the prose beside it.

## bench-strong-chunked-tie — a pair that was 2.24x ahead is now level

Found by `bench-refresh` (2026-09-08, §16). `bounded strong, chunked`
read okay 56.2 against zio 125.9; it now reads **136.0 against 137.5**.
ZIO barely moved (125.9 -> 137.5, inside host drift); okay did.

- [x] bench-strong-chunked-tie — NOT A REGRESSION, answered
      2026-09-08 by the A/B this entry asked for, and by its own
      disqualifying clause. Boundary `0e32ed6c` (last on 3.7.4) vs
      `4ce13ec7` (first on 3.9.0); the migration commit changed ZERO
      files under src/main, so the channel sources are byte-identical
      and only the compiler differs. Five alternating rounds each:

      | lane | 3.7.4 | 3.9.0 | |
      |---|---|---|---|
      | `okayStrongChunk` | **127.2** | 138.2 | +8.7% |
      | `zioStrongChunk` (control) | 142.7 | 141.4 | -0.9% |
      | `okayWeakChunk` | 65.1 | 57.0 | -12.4% |
      | `zioWeakChunk` (control) | 133.8 | 138.7 | +3.6% |

      On 3.7.4 the lane reads **127.2, not 56.2** — the old number
      does not reproduce on the compiler it was taken with, so it was
      an artefact of that session's `f=3 i=8` protocol. The pair is
      1.12x in okay's favour on 3.7.4 and 1.02x on 3.9.0; it was never
      2.24x. §16's text is corrected; no code was involved.
      By-product worth keeping: 3.9.0 costs this lane 8.7% and gives
      the weak one 12.4% back, with both ZIO controls inside 4% — a
      real, modest, two-directional compiler effect.

## bench-known-prices — internal costs, mostly already written down

Found by `bench-refresh` (2026-09-08). None is a defect; each is a
price this library pays on purpose. TRIMMED 2026-09-08 after triage:
four of the five were already explained in docs/benchmarks.md when
they were filed, so they are kept only as pointers, not as work. The
one with an open decision is the first.

- [ ] json-strict-is-now-the-slow-door — `Json.readStrict` reads 1104
      ns against `Json.read`'s 1004. The strict door was built to
      avoid the lossless road's cost, and 131cedc2 + b4172242 removed
      that cost. Either make the strict walk cheaper than the CST road
      it was meant to replace, or leave it and keep it for its
      REFUSAL — docs/benchmarks.md §10 already says the latter.
      DISQUALIFYING: if the strict walk's extra 100 ns is the field
      map and `make` (the breakdown says it is ~3.3x the bare parse),
      there is no cheap win and this closes as wontfix.
- [ ] elements-door-cursor — the `.elements` door reads 23.8 against
      the chunk transformers' 10.78 (§5), 2.2x for the per-element
      cursor. Known mechanism, stated in the doc.
- [x] chunked-lexer-bookkeeping — REFUTED 2026-09-09, nothing landed.
      The per-chunk bookkeeping was rewritten away (one traversal into
      a growable array) and measured: 1.8% of allocation at chunk 64,
      nothing at 512, and time unmeasurable — two four-fork rounds on
      the same code disagreed 1.5-2x in both directions. §10 carries
      the numbers. The 19% gap is NOT this.
- [x] lexer-state-allocation — HALF DONE 2026-09-09: the two `P` case
      classes are gone from the state (six ints flat), -6.1% B/op
      element-wise and -5.5% chunked, semantics unchanged, time not
      measurable on the day's box. The other half is BLOCKED and the
      reason is in §10: `buf: String` cannot become a start offset
      because `Lex.chunks` has no input to slice — a token may span
      chunks. A fix must serve both paths.
- [x] lexer-buf-without-concat — REFUTED 2026-09-09, nothing landed.
      The one candidate that needs no input (a doubling char array in
      the state) is WORSE by 11-12% B/op: JSON's tokens are short, so
      reserving a buffer per token costs more than concatenating one
      to four characters, and the old `Base` state allocated nothing
      at all between tokens. §10 carries the table and the per-object
      decomposition it produced: of the ~171 B per character, `S`
      itself is ~33%, the `Tuple2` ~19%, the concat ~23% — the concat
      is the smallest of the three. The other two candidates were not
      built and the reason is structural, not arithmetic:
      `Scan.finish(s, input)` helps ONLY the element-wise path (the
      chunked one still has no input), and `Either[offset, chars]` is
      that half-fix plus this refuted one. Was: the blocked half,
      reopened as its own
      question: how does a scanner accumulate a token's text without a
      String per character AND without the whole input? Candidates not
      yet priced: a small immutable char array grown by doubling in
      the state (allocation per GROWTH, not per char); an
      `Either[offset, chars]` that slices where the input is there and
      accumulates where it is not (two scanners in one type — state
      the cost before building it); or `Scan` gaining a
      `finish(s, input)` the element-wise path can feed. Gate as
      before: B/op first, the target is the ~171 B/char that remains.
- [ ] bracket-over-region — `okayBracket` 27.2 against `okayResource`
      22.4 (§7), 21%.
- [ ] vector-search-dominates — `searchVectors` 379 us dominates §11's
      per-query table, where everything else is under 20. Not a
      defect (240 segments x 1536 dims is real work), filed because it
      is where retrieval's time actually goes.

## spark-4-2 — the Spark pin is now free to move on its own

Found by scala-3-9 (2026-09-07) and deliberately NOT taken there.
okay-spark pins `spark-sql` 4.0.0; 4.1.x and 4.2.0 have shipped. 4.2.0
was tried during the LTS migration to see whether it fixed the
SparkSession classpath failure — it does not, the mechanism was ours
to fix (see the CHANGELOG entry and okay-spark's settings), so the pin
was put back at 4.0.0 rather than bundling an unrelated upgrade into a
Scala version bump.

- [ ] spark-4-2 — bump `spark-sql` 4.0.0 -> 4.2.0 and move
      `scala-reflect`/`legacyStdlib` from 2.13.16 to 2.13.18 with it
      (4.2.0 resolves 2.13.18; the two must stay a matched pair, and
      build.sbt says so beside the pins). The suite passed on 4.2.0
      during the migration, so this is a read of the release notes and
      a gate, not an investigation. Spark is still 2.13-only at 4.2.0,
      so nothing about the `for3Use2_13` arrangement changes.

## deploy-everywhere — one declaration, every place it runs (specs/deployment.md)

Designed 2026-09-07 on the operator's ask, with their answers taken:
a FULL dependency model (over my closed-list recommendation), a config
value with defaults plus a file plus the environment, secrets reaching
cloud managers and SOPS, and targets for a laptop, a rented server, a
cluster, a PaaS and AWS/Azure/GCP. The spec draws the line the model
does not cross and stages the work; each stage below is its own claim.

- [x] script-cli — LANDED 2026-09-07: `okay script run|render|build|
      check|serve|new`. `build` renders a whole pages directory ONCE
      to files — a static site with no JVM in the deployment — by
      driving a REAL `Site` with real requests, so page resolution,
      content types, static files, language variants and the error
      page cannot drift from what `serve` does. Two spec corrections
      came out of the writing: the combined `okay` binary is
      okay-script's assembly (okay-deploy cannot depend on
      okay-script), and `build` refuses a session touch and a redirect
      by name but CANNOT refuse `Web.current.form`, which is a stated
      limit rather than a guess.

- [x] deploy-cli — DELIVERED by deploy-doctor-cli (2026-09-07), which
      is what "folded INTO stage 0" meant: `okay deploy render/doctor/
      up/down/diff/targets` over `deployment.json`, a fat jar plus
      okay-deploy/bin/okay, exit codes that mean something (3 = a
      prerequisite is missing), --dry-run, --json, and no prompts at
      all. Left open by mistake until the arc was reviewed.

- [ ] deploy-cli-native — a GraalVM/Scala Native binary needing no
      JRE. The renderers are pure string builders and would port; the
      question is whether a second build toolchain is worth paying
      for, and nobody has asked yet.
- [x] deploy-bootstrap — DELIVERED by deploy-doctor-cli (2026-09-07):
      `Tool`/`Ready`/`Manager`/`Presence` with the second probe for
      installed-and-still-unusable, a table saying what is missing and
      WHY the deployment asked for it with the install command for the
      detected manager, and opt-in `--install` that prints every
      command and never pipes the network into a shell. Left open by
      mistake until the arc was reviewed.

- [x] deploy-model — LANDED 2026-09-07, stage 0's first half:
      `Deployment`/`Service`/`Run`/`Need`/`Settings`/`Scale` with
      derived Schemas, `Settings.of[A]` deriving env names from a
      Schema, `ordered` naming a cycle, and the two pure targets —
      `laptop` (one compose file: services, their databases and
      caches beside them, volumes, healthcheck, secrets as
      pass-throughs plus .env.example) and `host` (a hardened systemd
      unit per service, an EnvironmentFile, install/uninstall that
      leave settings and data alone). okay-script's deployment is
      expressed BOTH ways side by side, so the model is proven on a
      real application: the port that was written in five places is
      written once.
- [x] deploy-doctor-cli — LANDED 2026-09-07, stage 0's second half:
      `Tool`/`Ready`/`Manager`/`Presence` with a second probe for the
      installed-and-still-unusable case, a why-carrying catalogue fed
      by the target, the secret SCHEMES and the TLS mode, opt-in
      `--install` that prints every command and refuses to pipe the
      network into a shell, and the `okay deploy` CLI
      (render/doctor/up/down/diff/targets) reading `deployment.json`
      rather than evaluating Scala. Proven by a real `docker compose
      up` through the CLI (Live), a doctor run with a PATH emptied of
      docker, and the CLI driven from a COPY of okay-script's own
      artifacts directory.
- [x] script-config — LANDED 2026-09-07: `Serve.Config` is okay-script's
      whole configuration as one flat case class; okay-conf gained
      `envName` (the ONE derivation, which okay-deploy now exports
      rather than repeats), `fromEnv` (a PATCH, only what is set,
      typed by the schema, a wrong value refused BY NAME) and
      `layered` (defaults → file → environment by RFC 7396). The
      deployment writes only what it overrides, the rendered files
      came out byte-identical, and a test walks every name the
      deployment renders and fails on one the config has no field
      for. Three behaviours changed: a non-numeric value is named
      rather than silently dropped, an `OKAY_CONF` that is not there
      refuses, and an empty variable means unset.
- [ ] deploy-host-verified — put the rendered systemd unit in front of
      a real `systemd-analyze verify` and the install script in front
      of a real rented box. Needs a Linux host; the unit is currently
      proven by its text, which is not the same thing.
- [x] deploy-cluster — LANDED 2026-09-07, stage 1: the `cluster`
      target renders a Helm chart from the same value — ConfigMap,
      PVC, Ingress per TLS mode, a one-replica StatefulSet for a
      database that says in the manifest what it is, and a Secret the
      chart deliberately does NOT own (a templated one would be
      blanked by the next `helm upgrade`), with `secrets.sh` carrying
      the kubectl command instead. `helm lint`/`template` are LIVE,
      not the default gate: AGENTS.md's rule for suites leaving the
      JVM won over this module's opinion of its own tool.
- [x] deploy-old-helm-retired — LANDED 2026-09-07: one model, not
      two. `Deployment.image` renders the Dockerfile (not a target's
      file: every target runs what it produces), `Run.Module` gained
      `extraBuild`/`extraCopy`, `Service` gained `metricsPath`, and
      okay-demo ported. `Deploy`/`Compose`/`Helm` and the packaged
      chart resources are gone; `Resources`/`Health`/`Copy`/`repoRoot`
      survived; specs/deploy.md records its supersession. Porting a
      REAL application found all three gaps plus a fourth — okay-demo
      was about to render OKAYCHAT_PORT, which nothing reads.
- [x] deploy-paas — LANDED 2026-09-07, stage 2: the `fly`, `render`
      and `railway` targets. A managed database is DELEGATED — the
      third answer beside host's refusal and cluster's rendering:
      the platform has one, the file cannot express it, so `setup.sh`
      carries the exact commands (Render's Blueprint is the exception
      and holds it in the file). Gated by a REAL parser per format
      (tomllib, ruby -ryaml, okay's Json, `sh -n`) rather than golden
      files, which is stage 1's lesson applied. `Need.Region` joined
      the model; fly refuses without one, okay-script included.
- [x] deploy-cloud-aws — LANDED 2026-09-07, stage 3's first cloud:
      the `aws` target, Terraform for ECS Fargate (ALB, EFS, RDS,
      ElastiCache, ACM + Route 53). A secret is NOT a Terraform
      resource, because apply writes its value into the state file in
      plaintext; the network is NOT ours, because an organisation
      already has one. Gated by `terraform init` + `validate` against
      the provider's own SCHEMA (semantic, not syntactic), plus
      `fmt -check`, plus a test that deliberately breaks it.


- [x] deploy-clouds-rest — LANDED 2026-09-07, stage 3 finished: the
      `gcp` (Cloud Run) and `azure` (Container Apps) targets, both
      passing `terraform validate` and `fmt -check` against their
      providers' own schemas on the first run. The decision is a
      REFUSAL: gcp will not mount a gcsfuse bucket for a
      `Need.Volume`, because object storage has no atomic rename and
      no locking and okay-persist's log needs both; azure renders an
      Azure Files share, which is a real filesystem, and one test
      asserts all three answers together. Nine targets from one value.
- [x] deploy-secret-schemes — LANDED 2026-09-07, stage 4:
      `Schemes.sops/awsSecrets/gcpSecrets/azureVault` in okay-conf,
      each shelling out to the VENDOR CLI (no SDK, no second
      credential chain, and the doctor already checks for exactly
      those binaries). `Schemes.all()` is now okay-script's `sslOf`
      and okay-acme's provider default. The rule that matters INVERTS
      okay-deploy's: a failure never carries the command's output,
      because a secret resolver's stdout may BE the secret. sops is
      tested end to end against a real sops and a real age key in a
      container; the three cloud managers are argument-pinned with
      /bin/echo, since no machine here has an account.

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

- [x] chunked-source-sweep — DONE 2026-09-07: `zioStreamRange`,
      `fs2StreamEmits` (pure) and `okayStaged` joined
      `StreamOpsBenchmark`, all thirteen lanes ran in one session with
      `-prof gc`, and §5 is that one table — floor 13.96, Staged 1.55
      (0.11x), Okay chunked 9.52 (0.68x), Okay elements 22.5, fs2 emits
      21.5, ZStream.range 31.4, kyo range 60.8, the per-element rows
      243 / 618 / 1364. The old caveat's ratios held. (fs2's chunked
      spelling is `emits`, not `range`: its `range` is a singleton
      chunk per element, fs2-chunked-merge-lanes.) Original: one
      same-session StreamOps run with every library's CHUNKED source
      next to the per-element lanes; today only kyo's chunked lane
      exists and the §5 table mixes sessions with a ratio-to-floor
      caveat.
- [x] shape-check-new-lanes — DONE 2026-09-07: a rule, now written
      where a lane's author reads it — docs/benchmarks.md "Lane rules"
      under "Where the numbers are honest about limits", and AGENTS.md
      beside the Jmh warnings policy — with `ReaderBenchmark`'s
      left-nested / right-nested pair as the worked example and the
      pairing rule (granularity, memoisation, source) beside it.
      Original: every new competitor lane built by
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
- [x] casts-recount — RE-COUNTED 2026-09-07, after a day of core, channel,
      STM, intent and frame lanes: 33 `asInstanceOf` in src/main across
      the modules (code lines; the round-two close said 36 and counted
      comments that mention the word too) and 27 non-`resume`
      `@unchecked`. Every site is one of the argued ones — `Same`'s
      `=:=` witness, the erased `Chunk` array (`Chunks`, `ChunkBuf`),
      `SentinelChannel`'s one cast with its argument, `Pipe`'s
      `resumeWith`/`reinject`, `Effects`' `unapply` and the F-or-G
      split, `Writer`'s `Say`, `Generate.produced`, `Delim`'s two,
      `Agent.stateAs`, `Schema`'s kernel, `Frame.valueOf` by identity,
      `Http`'s `ArraySeq` narrowing, the Java and JS interop in
      okay-java / okay-crypto. Nothing crept in; the day's new code
      (`Ring`'s single-consumer pop, `Stm`'s log, `Reading.grounded`,
      `Temporal`/`Duration`/`People` lexicons, `Slot.lookup`) has none.
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
- [x] stm-js-direct-bench — DONE 2026-09-07 (§18d, specs/stm.md
      Results): `BenchStmCross` on three platforms + `StmBenchmark`
      under JMH. A one-`Modify` transaction is the same handler
      twice (the structural fast path; JMH identical to the byte,
      9 ns / 55 B per transaction). On a Read-then-Write `direct` is
      ahead of `tl2` by 23% on Node and 19% on Native; the JVM pair
      is inside its noise. The cost is the transaction (~800 B,
      70–90 ns JVM / ~300 ns Node / ~1.5 us Native), not the handler.
      Original: the direct handler is the JS given by construction;
      price it against tl2 on Node once a JS benchmark harness exists.
- [x] stm-sync-commit-fastpath — MEASURED AND DECLINED 2026-09-07
      (§18e). Built as filed, with the first attempt inside a `Run`
      so nothing runs at construction: `async(tryNow(tx)).flatMap {
      Right(a) => pure(a); Left(log) => Async.await(park…) }` in
      both handlers; 17 STM tests green. JMH `-prof gc`:
      `directReadWrite` 398 → 451 us (+13%) and 3.64 → 4.25 MB/op
      (+152 B per transaction), `tl2ReadWrite` 298 → 318 (+6%),
      +124 B; JS flat, Native −3–6% inside its bars. The `Run` +
      thunk + `Bind` + closure + `Either` cost MORE than the `Await`'s
      registration closure, exchange cell and `Got` — which the
      Drive's synchronous-answer path handles in one CAS and a
      `getAndSet`, evidently well. Reverted in full; the lanes stay.
      What the log costs (~800 B per Read-then-Write) is untouched
      by this and is the number, if anyone wants it: the persistent
      write map updated per write, the `(TRef, Long)` tuple per read,
      the installed cell. Original: an attempt that COMMITS never
      parks, yet every non-fast-path `atomically` is staged as an
      `Async.await`; run the attempt first, answer `pure(a)` on
      commit, reserve the `Await` for `RetryNow`.
- [x] stm-log-cost — DONE 2026-09-07 (§18f): the read set as two
      parallel arrays (−55/−63 B per transaction, time flat) and the
      commit as one typed walk of the write set — no reversed copy,
      no iterators, no lookup per install — `directReadWrite` 398 →
      167.5 us (−58%), `tl2ReadWrite` 298 → 229.3 (−23%); a
      Read-then-Write is 35 ns / 439 B (`direct`), 51 ns / 695 B
      (`tl2`) over the bind now. Not taken: the small-array write set
      (the `TMap` walk is no longer the cost), the log reuse across
      retries (a retry is rare and parks). Original: a Read-then-Write
      transaction allocates ~800 B
      and costs 70–90 ns on the JVM, ~300 ns on Node, ~1.5 us on
      Native (§18d), in the `Log`: a `TMap` write set rebuilt with
      `updated` per write, a `(TRef, Long)` tuple with a boxed
      version per read in an `ArrayBuffer`, the `Slot` installed per
      written cell, `written`'s iterator and closures at commit.
      Candidates, measured one at a time on `StmBenchmark
      *ReadWrite` (`-prof gc`) with the `Modify` lanes as control: a
      small-array write set for the one-to-few-refs case (the map for
      more), reads kept as two parallel arrays (no tuple, no box), the
      log reused across an attempt's retries. Laws: every TestStm* on
      every platform; `orElse`'s branch logs (`parent`, `absorb`)
      must keep their semantics.

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

## okay core
- [x] park-interrupt-order — LANDED 2026-09-07: the rule
      scheduler-cancel-wins gave `block` on the JVM had reached one
      park site of six. `blockAccepted` (a blocking channel send) had
      it wrong in BOTH its fast path and its loop, `await(Handoff)`
      in its fast path, and all three Native sites took the answer
      outright. All six now read the interrupt first and withdraw the
      registration when they refuse. Proved by a unit suite at the
      park level (`TestParkInterruptOrder`, one copy per platform)
      after a law written at the scheduler level passed with and
      without the fix. specs/schedulers.md, "Every park site".

## Flakes observed (record → fix loop when they recur)

- **native-runner-error — a Native module errors with NO failed test,
  and the runner says nothing else** (twice on 2026-09-09, in my own
  gates: `okay.codec.TestJsonEscape` at 13:01, `okay.lex.TestBpe` at
  13:41). The signature is exact: `Error: Total 110, Failed 0,
  Errors 1, Passed 109` where the module alone runs 116, plus
  `Error during tests:` naming whichever suite was in flight, and NOT
  ONE `==> X` anywhere. The module passes alone. It is a lost test
  process; the runner reports no exception, no stack, no output.
  (READ "THE CAUSE" BELOW FIRST: for shape B "lost" is measurably the
  wrong word — nobody killed that process, it exited 0 — and the same
  test applies to shape A's logs.)

  WHAT IS RULED OUT, measured, so nobody repeats it:
  - the scalascript RAM guard — its log reads `killed=0` at both
    minutes, though it was in T1 (the box was paging both times)
  - an OS kill — the kernel's `memorystatus` log for that window has
    only idle-exit of system daemons (cfprefsd, softwareupdated);
    nothing of ours, no jetsam
  - CPU pressure alone — 13 Native modules in parallel (`sbt all
    <m>/test ...`) under 42 CPU burners, four rounds: green. Five
    modules under 28 burners: green. So concurrency by itself does
    not do it; both real occurrences had memory pressure with it.

  NOT FIXED, and honestly labelled instead: `scripts/gate.sh` runs the
  matrix, and when a failure carries exactly this signature — no
  `==> X`, a module with `Failed 0` and `Errors ≥ 1` — it re-runs
  those modules ALONE and says which they were, in both outcomes. A
  single real test failure is final and never retried. Next occurrence:
  capture the module's own section of the log and the `log show
  --predicate 'eventMessage CONTAINS "memorystatus"'` window before
  rerunning, and add the pair here.

  THIRD OCCURRENCE, a NEW SHAPE (failing-over's gate, 2026-09-09
  18:50–18:55, a cold matrix in a fresh worktree beside two sibling
  sbts): `okayLexNative / Test / executeTests` failed with
  `RPCCore$ClosedException: NativeRunnerRPC$RunTerminatedException` —
  the runner process started (`Starting process '…/okay-lex-test' on
  port '60120'`) and printed NOT ONE suite header before it was gone,
  so there is no `Errors 1` line at all, and `scripts/gate.sh` said
  "RED — a failure this script does not recognise" and did NOT
  re-run. Alone, a minute later: 11/11. The pair the entry asked for:
  the module's section of the log is the one `Starting process` line
  and nothing after it; the memorystatus window 18:47–18:56 has ZERO
  lines (no jetsam, as before). Box at the time: 5.6 GB of 7 GB swap
  used, 273 885 pageouts, a sibling sbt at 500% CPU. So the same
  family — a lost test process under memory pressure (WRONG on both
  counts for this shape; corrected under THE CAUSE below by the agent
  who wrote this paragraph) — and gate.sh
  should learn this shape too: a `(<m>Native / Test / executeTests)`
  error line carrying `RunTerminatedException` with no `==> X` in the
  log is the rerun-alone case, not a real red; `--read` on the saved
  log (scratchpad gate-failing-over-full.log of that session, or the
  next occurrence's) is the test for the change.

  THE CAUSE, half settled (native-runner-cause, 2026-09-09). The
  entry called this "a lost test process" for a day. For shape B that
  is measurably wrong, and the correction comes from the runner's own
  code rather than from the symptom:

  - `ProcessRunner` (test-runner 0.5.12) fails its promise with
    "Process … finished with non-zero value N" on any non-zero exit,
    and above 128 also logs "Test runner interrupted by fatal signal
    N"; `ComRunner` then logs "Force close …"; and the
    `RunTerminatedException` sbt finally prints CARRIES that failure
    as its cause.
  - The failing gate log has NONE of those three lines, and its
    `RunTerminatedException` has no cause at all — one `Caused by`,
    nothing under it. `NativeRunnerRPC` builds it from
    `t.failed.toOption`, so no cause means the com run SUCCEEDED,
    which `ComRunner` only does when the process exited ZERO.
  - `TestMain` exits 0 in exactly two cases, both in `NativeRPC.loop`:
    end of stream, or a message length <= 0. Nothing else in that
    binary returns 0 while a call is pending.

  MEASURED, not merely read — `scripts/native-runner-probe.java`
  drives a real okay Native test binary the way ComRunner does and
  ends the connection four ways (on okay-lex-test, 2026-09-09):

      close-socket  exit=0    printed nothing
      zero-length   exit=0    printed nothing
      sigterm       exit=143  printed nothing
      sigkill       exit=137  printed nothing

  So the two signal shapes are exactly the ones that WOULD have been
  logged and were not. Nobody killed that process. Its connection
  ended and it left, cleanly and silently, while sbt still had a call
  in flight — which is why the error is an sbt-side `ClosedException`
  and why the module has no test report at all.

  ALSO RULED OUT BY COMMAND (this occurrence):
  - the RAM guard: its log has only `SPARE` decisions in the window
    and `killed=0` at every tick, with 6.9–8.5 GB available
  - jetsam / an OS kill: the `memorystatus` window is EMPTY and there
    is no crash report for the binary — and a SIGKILL would have
    shown as 137 above
  - our own code: no `sys.exit`/`System.exit`/`halt` in any source a
    Native test binary links (the six hits are JVM-only modules)
  - the plugin's global adapter close (`onComplete` closes EVERY
    `TestAdapter`, and `TestAdapter.close()` is SILENT when its runs
    are all done — the one silent closer in the JVM): refuted by the
    log itself, since 30+ runner processes started and passed AFTER
    the failing one
  - the adapter being collected under the GC storm the log shows
    (89.8% of ten seconds in GC): `registerResource` holds every
    adapter in a strong list, so it cannot be collected — though the
    mechanism it would have needed does exist, `NioSocketImpl` having
    a `Cleaner` that closes an unreachable socket's fd

  STILL OPEN, and stated as open: WHAT ended that one connection. Two
  candidates remain, both silent by construction — the JVM end of the
  com socket going away without any of the loggable paths, and a
  non-positive length reaching `readInt` on a live connection. The
  log's own hint favours the first: that process produced no test
  output at all, and a desync needs at least one message to have been
  written. What would separate them next time is a TIMELINE, which
  the sbt log cannot give (it has no timestamps): record the runner
  processes beside the gate (`pgrep -f '<module>-test'` once a second,
  with the clock) and compare the death against the module's first
  output. Died before any output, and nothing was ever written to it:
  something closed the socket. Died after: the stream is the suspect.

  HANDLED 2026-09-09 (gate-lost-shape2): `scripts/gate.sh` knows both
  shapes now — A, a module reporting `Failed 0, Errors 1` after some
  tests ran, and B, `(<m> / Test / executeTests)` carrying
  `RunTerminatedException` with no report at all because the process
  went before it said anything. It stays conservative: a project that
  failed in NEITHER shape is named and nothing is re-run. Tested with
  `--read` over five real logs — the two of shape A, this one of
  shape B (thank you for keeping it), the `TestOfflineGate` failure
  and a green matrix — plus a doctored log where a second project
  fails in an unknown shape, which correctly refuses to retry.

- **hedge-timer-leak — FIXED 2026-09-09 (hedge-start-races), and it
  was the SMALLER half.** `Hedge.start` published after it acted, in
  two places: it forked an attempt and only then added the fiber to
  the list `settle` cancels, and it armed the hedge timer later still.
  An attempt answering inside the first window left a fiber nobody
  cancels — for a hedged request, a duplicate that outlives the answer
  and keeps working — and inside the second, a timer nobody disarms.
  Both are re-checked and undone now, by the thread that published
  them. `TestHedgeStart` drives the window rather than timing it: a
  Scheduler wrapper lets the first attempt answer in the middle of the
  second one's fork, and the test asserts the second fiber was
  cancelled and no timer stayed armed. It fails on the first count
  without the fix.

- **TestResilienceTimed "hedge: a fast first attempt never starts a
  second" — FIXED 2026-09-09 (hedge-timed-flake).** Seen at ~10:20
  under nine sbt JVMs: `starts.get` was 2, because the "fast" first
  attempt took longer than the 20 ms hedge delay and the timer fired
  as designed. Two assertions in that file said "nothing else started"
  by sleeping 60 ms on the REAL timer, which asserts the box's speed.
  Both now take a `ManualTimer` the test fires by hand: after the run
  settles, firing whatever is still armed starts nothing, because
  `start()` is guarded by `done`. Measured both ways with a first
  attempt three times slower than the delay: the old shape fails, the
  new one passes. No wall clock left in either.

- **TestManyToMany "the default channel ends for every consumer" —
  FIXED 2026-09-09 (channel-lost-part), and MY EARLIER READING OF IT
  WAS WRONG.** The entry said "a part accepted under memory pressure
  and dropped at close" and asked for a repro. Reproduced under CPU
  oversubscription (28 burners, ~1 run in 8) and instrumented, the
  channel turned out to lose nothing: `sendBlocking` refused nothing,
  the buffer ended empty and every accepted element was delivered
  (accepted = popped = delivered = seen, one thousand short of the
  16 000 sent). What actually happened is that a PRODUCER THREAD DIED
  on its very first send with `NullPointerException: tried to cast
  away nullability`, and `join` is happy with a thread that threw, so
  the law reported its whole output as elements the channel had lost.
  The throw: `AdaptiveFifo.claimPart` publishes the part COUNT before
  the SLOT, and a producer that shares an existing part — 16
  producers over a buffer whose cap is 8 — read that slot while it
  was still null. Every other reader in the class expects the null and
  comes back; the thread-local home `.nn`'d it. Fixed by waiting for
  the slot (`slotAt`), with `TestAdaptiveClaimRace` as the regression:
  it throws within three rounds without the wait, 0.07 s. The law now
  asserts that every producer finished, so a next occurrence names the
  throw instead of blaming the channel.

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

- [x] channel-impls — CLOSED 2026-09-07 (channel-entries-audit): every
      bullet below landed under another name, and the harness it asks
      for exists. RingChannel → `SentinelChannel` (d7c69167, the ring
      plus a mark in the FIFO stream), the default `Channel.apply`,
      and since receive-blocking-path-length its head moves by a store
      for a single consumer (§17g: 152 us elementwise against
      `StmChannel`'s 250). UNBOUNDED → `Segments` behind
      `Queues.strong[A].unbounded` (channel-ring-unbounded). Relaxed /
      MultiFifo → `AdaptiveFifo` behind `Queues.relaxed` / `adaptive`
      (cb51748c), measured at sixteen producers, order between
      producers documented as the price. The comparison harness,
      parameterised over the implementation: `ChannelGuaranteeBenchmark`
      (a lane per mechanism at both granularities, zio beside them)
      and `TestChannelLaws` (an `impls` table every mechanism must
      answer for). Original:
      Implementations behind the `Channel` seam
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

- [x] raft-wire-election-flake — CLOSED 2026-09-07 (raft-wire-flake): Live-tagged, budgets and a retrying cluster; see its own entry below. Original: okay.persist.TestRaftWire "killing
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
      SECOND OBSERVATION 2026-09-06 (channel-elementwise-wakeups'
      gate): same test, same assertion -- "the survivors did not
      commit after failover: Vector((1,1), (2,1))" -- under matrix
      load 25-30, then green 3/3 in isolation at load 12-13. The same
      signature as 3 September. Not the lane that saw it either: its
      only edit is `filled` from AtomicBoolean to @volatile in
      CanBlock, and this flake is three days OLDER than that edit.
      Two sightings, both load-shaped, both green in isolation -- that
      is enough to stop surveying and act: tag it Live, or give the
      failover assertion a budget that survives a loaded box.

- [x] channel-impls-correctness — CLOSED 2026-09-07 (board-hygiene): not a flake, and answered by the channel rewrite — `RingChannel` and `CasChannel` no longer exist; the ring channel is `SentinelChannel` (d7c69167), judged by `TestChannelLaws` as this entry asked. Original: bring RingChannel and CasChannel
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

- [x] ring-channel-waiters — CLOSED 2026-09-07 (channel-entries-audit)
      by two counts that refute its premise. The premise: the waiter
      protocol eats more than half the ring's win. The counts: on the
      elementwise saturated regime the consumer never parks and the
      producer parks 12–20 times per 4000 elements (§17f, a probe on
      `okaySentinelElem`), so no waiter code is on the per-element
      path; and the per-element cost that WAS there was the head CAS
      of the pop, a third of the consumer by JFR, taken for a single
      consumer by a store (§17g, 203 → 152 us). `SentinelChannel`
      reads ahead of `StmChannel` on every lane today. What the entry
      names is still literally in the code — a `Waiter` per park
      attempt, `wakeOne` walking a `List` with `last`/`init` — and
      matters only in a regime that parks at scale; the sixteen-
      producer regime already has its answer in per-part waiter
      queues (channel-per-part-waiters, one useful wakeup in k). No
      lane on the board measures a cost there, so nothing stays open.
      Original: (after channel-impls-correctness) the
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

- [x] channel-ring-unbounded — CLOSED 2026-09-07 (board-hygiene): not a flake; the unbounded ring is `Segments` behind `Channel.apply` above `MaxRing` (relaxed-queues-builder, cb51748c). Original: channel-ring gives the allocation-free
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

- [x] channel-multififo-many-producers — CLOSED 2026-09-07 (board-hygiene): not a flake; `MultiFifo` was folded into `AdaptiveFifo` behind `Queues.relaxed`/`adaptive` (cb51748c), and channel-per-part-waiters measured it at sixteen producers. Original: if a channel ever has MANY
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

## Task-oriented dialogue: the literature the operator brought (2026-09-05)

Three papers, read together on 2026-09-05. Their common answer to
"a new task must not need retraining" is to put the schema in the
INPUT rather than in the weights — which is structurally where we
already are (`Schema[I]` derives the tool declaration AND the
decoder, specs/intent-classify.md). What follows is only what is NOT
already done here, in the order it is worth doing.

CAVEAT ON ALL THREE, stated once: every reported gain comes from a
FINE-TUNED model (GPT-2, T5). What transfers is the construction of
the input, not the numbers. Each item below is a hypothesis to
measure on our own data, never a predicted result.

- [x] tod-demonstrations-from-the-log — MEASURED 2026-09-07
      (TestDemonstrationsLive + Demonstrations, offline selector with
      its own suite). Four demonstrations chosen mechanically from a
      log — one per class, scored messages excluded — lift the model
      tier 0.685 → 0.892 macro F1 and take undecodable replies 6/120
      → 0/120. With index names they recover only 0.100 → 0.376: of
      the whole gap the examples buy 35% and the names 74%, so
      demonstrations COMPOSE with names rather than replacing them.
      The selector is the mechanism the harvest programme needs (the
      log as prompt material) and it performs like hand-written
      examples. specs/intent-classify.md, "Results —
      tod-demonstrations-from-the-log".
- [x] tod-schema-diagnostics — MEASURED 2026-09-07 (TestSchemaDiagnostics,
      Live). (a) D3ST: an index-named taxonomy (C1..C4, field s1)
      collapses from 0.685 to 0.100 macro F1 and answers C1 for every
      message — the four identifiers carry essentially all of the
      prompted model's discrimination. (b) SGD-X: near synonyms cost
      0.217 macro F1 and far synonyms 0.425, with Request recall
      0.67 -> 0.07 under "Ask" and Notification F1 0.77 -> 0.00 under
      "Advisory". Verdict: a taxonomy's case names ARE the prompt, so a
      rename is a model-facing change and must carry a number; name a
      class with the plainest standard word. specs/intent-classify.md,
      "Results — tod-schema-diagnostics".
- [ ] tod-schema-guided-retrieval — Labruna, Bonetta, Magnini (RANLP
      2025, "Task-Oriented Dialogue Systems through Function
      Calling", MultiWOZ 2.3): let the model call a schema-guided
      query that fetches only the needed KB entries, instead of
      putting the whole KB in the prompt; accuracy up, tokens and
      time down, the gap widening as the KB grows. Their BASELINE is
      not ours — we never put a KB in a prompt, and the demo's
      central claim is that nothing reaches the projection except
      through a tool. What IS new for us: deriving the RETRIEVAL tool
      from the store's own `Schema` instead of hand-writing one tool
      per query shape (okay-sql's typed layer, okay-match's
      registry), so a new domain field becomes a queryable slot with
      no new tool code. Pairs with a KB-size sweep — tokens per turn
      and latency, full-KB against schema-guided — in
      docs/benchmarks.md, because at the demo's current KB size the
      effect is invisible by construction.
- [ ] tod-single-sequence — SimpleTOD (Hosseini-Asl, McCann, Wu,
      Yavuz, Socher, 2020, arxiv 2005.00796): belief state, actions
      and response as ONE delimited sequence rather than three
      models. Filed LAST, and the reason is the useful part. Its
      result is a fine-tuning result (GPT-2 on MultiWOZ) that we
      cannot reproduce without training; the 2025 paper above argues
      the opposite architecture on ground that suits us better; and a
      single delimited sequence WEAKENS the invariant the demo is
      built on, since a belief cut out of raw text has not gone
      through a tool (recoverable only by decoding it through
      `Schema` before it touches the store — intent-classify's own
      rule). What stays attractive is the SHAPE: SimpleTOD's
      inference suspends after the belief state, queries the KB,
      appends the result and resumes the SAME generation — a
      coroutine that yields exactly once, which is `Stage` over
      `Cont` and something okay expresses better than a framework
      would. Worth building only if the items above leave a reason to.
- [ ] tod-multiwoz-harness — OPTIONAL and honestly expensive: a
      loader for MultiWOZ 2.3/2.4 plus the inform/success/joint-goal
      metrics, so our dialogue lane has numbers comparable with the
      outside world instead of only with itself. Keep separate from
      the items above; it is a benchmark harness, not a feature.

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [x] ui-vocab — LANDED 2026-09-09 (see specs/frontend.md Results). Original: Box with weights/gap/pad
      (Row/Column as aliases), style tokens, Image, Input kinds,
      Scroll; the semantic level (Form, List, Table, Tabs, Modal) each
      DEFINED by its lowering; `Ui.lower(ui, vocab)`, `Ui.keys`; laws:
      diff-then-patch on every new node, keys(s)==keys(lower(s)), diff
      commutes with lowering.
- [x] ui-protocol — LANDED 2026-09-09 (specs/frontend.md Results). Original: derived Schema[Ui]/[Event]/[Patch] (JSON +
      CBOR from one definition; needs codec-vector's gaps closed),
      `hello {vocab, version}` first line and lowering per vocab in
      Wire.serve, the conformance script, docs/protocol/frontend.md
      rendered from the schemas. WireJson retires after equality.
- [x] ui-hybrid — LANDED 2026-09-09 (specs/frontend.md Results). Original: Input local by default, Form submits ONCE
      as Submitted(key, json) decoded by the form's schema, `live`
      inputs send Edited, the closed Local set (Toggle, Tab), server
      SetValue overrides a local edit, forged Submitted dropped.
- [x] ui-mobile — LANDED 2026-09-09 (specs/frontend.md "Mobile", M1): installable Live pages — viewport, level-L mobile CSS, manifest, service worker; Playwright in an iPhone emulation, offline reload; live.js queues events before the socket opens.
- [x] ui-mobile-ios — LANDED 2026-09-09 (specs/frontend.md "Mobile" M2): okay-swift/, `swift test` 3/3 over conformance.jsonl, iOS Simulator build succeeded, headless smoke against a real Live page.
- [x] ui-mobile-android — LANDED 2026-09-09 (specs/frontend.md "Mobile" M3): the SDK by brew + sdkmanager, okay-compose/app as Kotlin Multiplatform (desktop + Android from one set of composables, the socket per platform), app-debug.apk built. Not run: an emulator image is a further download.
- [x] ui-compose — LANDED 2026-09-09 (specs/frontend.md Results; okay-compose/README.md). Original: a Compose Multiplatform thin client
      (Kotlin, no okay dependency) drawing level L, passing the
      conformance script; the same server drives browser + Compose at
      once; Scala Native + GTK or Swing as the out-of-the-box leg.
- [x] ui-native-toolkits — LANDED 2026-09-09 as the Swing host (`Swing.backend/host/window`, TestSwing headless); GTK 4 on Scala Native LANDED 2026-09-09 (ui-gtk, okay-ui-gtk/, aggregated only where pkg-config finds gtk4). Cocoa stays filed: Objective-C from Scala Native is objc_msgSend all the way, and a Swift thin client over the protocol is the cheaper road. Original: GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-codec
- [x] json-escape-alloc — LANDED 2026-09-07: `Json.escape` allocated a
      String PER CHARACTER, on a path `Json.print`, `Staged.scala` and
      `RuntimeStaged.scala` all take — so every string through the
      staged doors paid it. `unquote`'s shape now: 17.0 → 6.0 ms with
      nothing to escape, 37.4 → 13.0 ms when every string escapes
      (200k strings, best of twelve). The win is in BOTH columns,
      because the old version allocated per character either way. The
      test was written FIRST and caught the rewrite using Scala's
      `StringBuilder`, whose `append(Any)` silently appended a tuple.

- [ ] codec-two-roads-audit — the json-parse-fast-road shape as a
      QUESTION rather than a fix: a module had two roads to the same
      value, 37x apart, and the DEFAULT was the slow one for long
      enough that a separate feature (py-arrow) got filed to work
      around the symptom. Where else does this repository have a fast
      path that nothing takes by default? Named suspects: the CBOR
      pair beside `Json`/`JsonValue`, and the staging seam's
      interpreter-vs-installed choice (`Codecs.current`), which is a
      runtime switch rather than a road but has the same failure mode
      — measured once, then assumed. Cheap to check, and the last
      check of this kind was worth 37x.

- [x] codec-jsonschema-refinement-enum — `JsonSchema.of` renders an
      `SIso`/`Schema.refine` as its underlying type, so a refinement's
      vocabulary (`Conf`: three words over a string) reaches a
      `response_format` contract as a plain `string`; found by
      intent-structured-output, where the contract could not carry
      the one thing the persuasion did. A refinement that enumerates
      its values (a finite `refine`, or an enum's Schema) should
      render as `"enum": [...]`. Filed, not done inside a measurement;
      worth doing when a gateway that ENFORCES its schema appears —
      the rozum one does not, for nested sums and lists. DONE
      2026-09-07: `Schema.enumeration(values, name)` — the vocabulary
      as a second parameter list on `SIso`, so the twenty patterns
      stay three-armed; only `JsonSchema.of` reads it (`enum` beside
      the underlying type); `Schema.vocabulary` for a declared
      vocabulary with one's own decoder — `Conf`, case-insensitive as
      the recorded journal needs; JVM/JS/Native tests. The prompt
      renders WITHOUT vocabularies (`JsonSchema.of(s, vocabularies =
      false)`): measured deterministic, the enum in the schema cost
      1.7 macro-F1, so the recording stands.
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
- [x] staged-runtime — landed: `okay-staging`, a JVM-only module
      nothing depends on; `RuntimeStaged.json(schema)` is the staged
      generator over the schema as a VALUE through
      `scala.quoted.staging`, cached by identity, the interpreter when
      `-Dokay.staging=off` or a generation fails (never a throw).
      Agreement over the whole node vocabulary on run-time-built
      schemas. Price on the Order: encode 233 ns vs 842 interpreted (3.6x; 1.4x of the compile-time staged 165), decode-from-AST 140 vs 683 (4.9x; 1.2x of the compile-time 113); generation 8.7 ms per schema with the compiler warm (the first in a process pays the compiler's own warm-up on top, seconds), so a warm generation is earned back after ~15,000 values (609 ns saved per encode, 543 per decode) — history.tsv staged-runtime. Not a default anywhere;
      "where else" with the condition for each is in the spec.
      specs/codecs.md, "Run-time staging".
- [x] schema-thunks-once — landed: `Schema.once`, every derived edge
      memoised (fields, cases, Option/List/Vector, wrap/refine); a
      sum's case schema was re-derived per value and never the same
      instance twice (the staged-runtime trap, at its source).
      Interpreter before/after: measured by ALLOCATION per value (-prof gc; time on a loaded box is noise, bytes are not) on a sum-shaped Owner (a Pet enum, four case values): encode 10160 -> 8144 B/op (-20%), decode-from-AST 5976 -> 4088 (-32%), CBOR encode 7312 -> 5416 (-26%); the same runs' times 1072 -> 817, 830 -> 573, 1221 -> 974 ns (wide error bars); the Order, which has no sum and a given per type, 8016 -> 7968 B/op (-0.6%, the Option/List givens' re-summon) — history.tsv schema-thunks-once. specs/codecs.md, "Schema
      thunks once".

- [x] sql-fold-profile — landed: `MeasureSqlFold` (okay-jdbc, Live).
      The row fold is 0.14 us per row at six columns and 10.5% of an
      in-memory H2 read (corrected 2026-09-07 by sql-plan-cells: the
      first numbers, 0.45 us and 24.1%, were taken on 3 warmups); `Typed.planOf` already hoists column matching
      out of the per-row loop. staged-runtime's condition for okay-sql
      (>=30%) is NOT met, and cannot be by a real driver — verdict: no
      staged codec at the database seam. specs/codecs.md, "The row
      fold's share".
- [x] sql-plan-cells — MEASURED AND DECLINED 2026-09-07 (taken up on
      the operator\'s word despite its own condition). Compiling each
      field\'s Shape into a cell decoder at plan time is SLOWER than
      the ADT walk it replaces: 0.13-0.14 us per row against
      0.11-0.12, over three paired back-to-back runs. A closure call
      per cell is a megamorphic virtual call and a tuple destructure;
      the match it replaced is a branch the JIT already predicts.
      Reverted. What stayed: the measurement instrument, whose 3
      warmups had made the ORIGINAL profile three times too high
      (0.45 us -> 0.14 us, 24.1% -> 10.5%), and TestRowDecode, a
      decode suite the module did not have. specs/codecs.md, "The
      cell decoders that were not faster".
- [x] staged-strict — landed: `RuntimeStaged.strict`, the strict-JSON
      reader generated from a schema VALUE, and `Codecs.strict` /
      `Codecs.readStrict` beside json/cbor (the Provider method is
      defaulted to the fold, so no implementation had to change).
      Agreement with JsonStrict.read refusal by refusal. Price:
      text to Order 385 ns against the interpreted strict door's 901 (2.3x) and the compile-time generated 307 (1.25x of it); circe's fused parse+decode 706 ns on the same text, so the run-time generated strict read is 1.8x faster than circe with no type known at compile time — history.tsv staged-strict. specs/codecs.md, "The strict door".
- [x] staging-seam — landed: `Codecs.json/cbor`, one pluggable door for
      a codec over a schema value (interpreter by default, every
      platform); `RuntimeStaged.cbor` + `install()`; `Staging.autoInstall`
      by name on the JVM; okay-script's container installs at boot;
      the generic doors of script, ui, persist, http, cluster, agent,
      llm, cache, mongo, conf, obs routed through it. Price:
      through the seam with the interpreter (nothing installed) encode 864 ns vs 860 direct, decode-from-AST 654 vs 598 — a volatile read and a wrapper, within the interpreter's noise; through the seam with okay-staging installed encode 235 ns and decode 164, the same as the generated codec called directly (236 / 142-164 across runs) — the seam costs nothing measurable over the codec behind it. The first seam run had the staged door at 2.7 µs: the launch switch read `sys.env` per call (fixed, see Decisions) — history.tsv staging-seam. specs/codecs.md, "The codec seam".
- [x] script-runmain-fork — landed: okayScript's `run` forks (as its
      tests already did, for the same reason) with the repo root as its
      working directory; `sbt "okayScript/runMain okay.script.Serve
      <dir> <port>"` now compiles its pages (verified: a page compiled
      and served, with a directory given relative to the root). Found by
      staging-seam's boot check; the same failure on master before.
      specs/okay-script.md, "Site — the container".
- [ ] py-arrow — frames via pyarrow (twin of r-arrow). RE-FILED
      2026-09-07 with an honest number: the measurement meant to
      justify it found that 60% of a 500k-row frame's 9.7 s round trip
      was OUR OWN `Json.parse` taking the lossless road
      (json-parse-fast-road). The same frame is now 0.94 s, of which
      the Python side is roughly half and our encode 0.3 s. Arrow
      would still take the serialization hop out, but "the JSON-frame
      road hurts" is ten times less true than when this was filed and
      no consumer has asked. Measure again before building.
- [x] json-parse-fast-road — LANDED 2026-09-07, found while measuring
      py-arrow: `Json.parse` was the LOSSLESS road (tokenize to a CST,
      then project) while `parseValue` was one strict pass — two roads
      to an equal value, differing by 79x, with the slow one as the
      default that `Codecs.readJson` and twenty-four files took.
      `parse` is the fast road now with the lossless fallback intact;
      `parseValue` is removed rather than deprecated. 500k-row frame
      round trip 9.7 s → 0.94 s. Full suite green, 2959 tests.

## okay-r (specs/r.md — R as a handler)
- [x] json-cst-batch-road — LANDED 2026-09-07, the operator's follow-on
      ("why is it slow, how is it made fast"): `Json.cst` was feeding
      the source ONE CHARACTER AT A TIME through the effect system
      (`Writer.tell` + `flatMap` per char) into two transducer stages
      and a LazyList — just moving the characters cost four times the
      entire fast value parse. `Parse.full` + `JsonParse.instrs` were
      written for each other and JSON was the LAST codec not using
      them. Tree 1129 → 334 ms (best of twelve, 9.3 MB), proven by a
      TREE-level prefix sweep. Two things measured and NOT done, with
      the numbers: skipping reparse snapshots (1.5%, noise) and
      `Scan.step`'s per-char tuple (needs an interface four codecs
      implement, and no main-source caller uses the lossless road).
      Also corrected json-parse-fast-road's own "79x" to 37x — it was
      single-shot, which at this scale prices the JIT.
- [x] json-projection-alloc — LANDED 2026-09-07, the rest of the
      lossless road: the projection answered a `Vector` from every
      node and every leaf (plus `grouped(2)` per field) and now
      appends into a builder in one pass; `unquote` built a
      StringBuilder and two substrings for every string token and now
      returns the substring directly when there is no escape. ~12% on
      that stage, ~6% end to end — A/B'd in ONE run, because the first
      cross-run reading said 2x and was GC noise. Guarded by
      TestJsonValue's existing prefix sweep.
- [ ] scan-step-allocation — NOW THE LARGEST NAMED SHARE, and priced
      2026-09-09 by lexer-buf-without-concat: the `Tuple2` is ~19% of
      lexing's ~171 B per character, second only to `S` itself (~33%).
      The consumer this entry said it wanted is now here — three lanes
      have measured this path — but note what the same work refuted:
      the concat is the SMALLEST of the three shares, so an interface
      change that removes only the tuple buys about a fifth. Was:
      `Scan.step: (S, Char) => (S,
      Vector[Token[K]])` allocates a tuple per CHARACTER, which is the
      next wall on the lossless road (~26 ms of a 9.3 MB lex). An
      additive `stepInto(s, c, out)` with a default delegating to
      `step` would leave every scanner working and let one override.
      NOT taken 2026-09-07 because no main-source caller uses the
      lossless road at all — it serves `Json.parse`'s damage fallback,
      the incremental reparse story and the tests. Wants a consumer
      before it wants an interface change.
- [x] r-subprocess — LANDED 2026-09-07, stage 0: okay-r with
      REval/RValue/RFrame, the versioned shim, the comonadic handler
      over a clean-env Rscript, conditions as data, verify, dead-
      process-throws. R's TWO absences are two cases (NULL vanishes
      from a vector, NA poisons it — R's own arithmetic is the test)
      and NA carries its TYPE. jsonlite is a NAMED prerequisite the
      handshake refuses on. Proven against a live R in a container.
      The "Durable-replay test" in this entry could not be written
      and should not have been promised — see durable-any-operation.
- [ ] durable-any-operation — `Durable.tools` wraps a `Handler[Tool]`
      and `Tool.Call` carries a `ToolCall`, so there is NO generic
      journal-any-operation. specs/r.md and specs/py.md both claimed
      a foreign-runtime step is "journalable by Durable"; both are
      corrected, and this is the item that would make it true. The
      question to answer first is what a fingerprint and a key mean
      for an arbitrary operation type — `ToolCall` gave both for
      free, and an `REval.Frame` carrying a million rows gives
      neither cheaply.
- [ ] r-rserve — stage 1: the served engine (Java client behind a
      trait; own QAP1 over Async later if named); two-engine
      acceptance
- [ ] r-arrow — frames as Arrow files/streams once the JSON-frame
      road hurts. MEASURED 2026-09-09 (r-measure-harden,
      `MeasureRFrame`, medians of five against the dockerized R 4.4.1,
      `identity` on a 3-column frame):

      | rows | payload | our encode | round trip | our decode | typed rows | OUR share |
      |---|---|---|---|---|---|---|
      | 10 000 | 0.30 MB | 6.5 ms | 1 546 ms | 7.3 ms | 2.2 ms | 0.9% |
      | 100 000 | 3.21 MB | 20.4 ms | 13 686 ms | 18.3 ms | 6.0 ms | 0.3% |

      The number says the opposite of the Python twin's. There, 60% of
      the trip was OUR parser; here our two halves are 0.3% and the
      other 99.7% is R. Nor is it the pipe: 3.21 MB in 13.7 s is
      ~230 KB/s, and a pipe does that in milliseconds — we encode and
      decode the same bytes at ~83 MB/s. So the cost is jsonlite
      walking the STRUCTURE we hand it, and the structure is the
      suspect below. Arrow would still remove it, at the price of the
      `arrow` package (native, heavy) on R's side and an Arrow reader
      on ours — a big dependency for a module whose only dependency
      today is jsonlite. Try the cheap shape change first.
- [ ] r-frame-columnar-wire — THE CANDIDATE THE MEASUREMENT FOUND, and
      it costs no dependency. `Wire.enc` tags PER CELL: an R integer is
      `{"t":"i","v":…}`, an NA is an object, an integral double is an
      object — because JSON cannot otherwise keep R's integer apart
      from its double, nor its four NAs apart. On a 100k-row frame that
      is hundreds of thousands of tiny objects for jsonlite to build,
      and jsonlite's fast path is exactly the one it cannot take.
      A frame is COLUMNAR and a column is homogeneous, so the type tag
      belongs to the COLUMN, not the cell: `{"name": …, "type": "i",
      "values": [1,2,3], "na": [7, 19]}` — one tag, one plain array
      (jsonlite's C path), and the absences as an index list. Values
      stay exact, the four NAs stay four, and nothing about the
      no-source rule changes. Expected to move most of the 99.7%;
      MEASURE with `MeasureRFrame` before and after, since that is the
      lane's own lesson. Do this before r-arrow — and if it lands the
      win, r-arrow may never be worth its dependency.

## okay-persist (specs/persist.md — staged design; stage 0 landed)
- [x] persist-raft — RaftStore: consensus as one more control-log
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
      1b LANDED 2026-09-07: `okay.persist.RaftStore` — a `Store` over
      the wire node (an append proposed as one log entry, applied on
      every node at commit to its local store; reads from the local
      store, so nothing uncommitted is ever served; a follower's
      append throws `NotLeader(leader)`, a lost majority
      `NotCommitted`) — and `RaftWire.Stable` (term and vote saved
      inside the lock before any send; a file replaced by rename, or
      memory). `TestStable` in the gate, `TestRaftStore` Live beside
      `TestRaftWire`. FORWARDING LANDED 2026-09-07 (persist-raft-
      forward): `RaftMsg.Propose`/`Proposed`, a follower's append
      carried to the leader and applied everywhere; `NotLeader` only
      when no leader is known. SIM HARNESS LANDED 2026-09-07
      (raft-sim-fuzz): `TestRaftSim`, a discrete-event simulator over
      the pure core — five nodes, random timeouts, reordering, 10%
      loss, a minority cut and healed — safety asserted after every
      event on 40 seeds, acked proposals never lost, convergence and
      a late ack on a lossless stretch; replayable by seed. STAGE 2a
      LANDED 2026-09-07 (raft-membership): single-server membership
      changes (thesis §4.1) — a configuration entry in the log, in
      force on append, one change at a time, a removed leader steps
      down at commit, a removed node stops campaigning;
      `reconfigure(cluster)` on the wire node and the store; swept
      by the simulator (a sixth node joins, the leader leaves, 40
      seeds clean). The sweep forced two of the paper's rules the
      core had skipped: the blank no-op at the start of a term (§8)
      and conflict-only truncation in AppendEntries (§5.3) — a real
      safety bug under reordering, fixed. STAGE 2b LANDED 2026-09-07
      (raft-compaction): `Raft.compact` (the engine's bytes, the
      core's index arithmetic), `InstallSnapshot` for a follower
      whose next entry is compacted away, `restored` as the engine's
      cue; `Node.compact`/`onRestore` on the wire; the simulator's
      nodes run state machines they snapshot and compact to — 541
      snapshots, 81 installs on 32 of 40 seeds, safety asserted on
      what the machines saw. STAGE 2c LANDED 2026-09-07
      (raft-store-snapshot): `RaftStore.snapshot()` — the local
      store's full history as the image, refused by name once
      retention has dropped history (offsets must survive a
      restore); restore appends the image's records past the local
      `end`, a gap is `damaged`; `snapshotEvery`; the wire's commit
      callbacks moved inside the node's lock (a real ordering race
      between connections). PRE-VOTE LANDED 2026-09-07
      (raft-prevote), measured: a `PreCandidate` asks at its next
      term without adopting it, a voter grants only on the log AND
      no leader heard within an election timeout (the caller's word:
      `handle(..., leaderFresh)`); terms over the forty partition
      seeds 163 → 67, over the membership seeds 174 → 93, more
      proposals acked. LEFTOVERS DECIDED 2026-09-07 (raft-leftovers):
      the joiner's catch-up phase declined by measurement (a joiner
      is current 25..165 ms after it is added, median 71, on 40 of
      40 seeds, one to three heartbeats, while the cluster commits
      0..1 entries — a learner phase shortens a window already
      shorter than a heartbeat's commits; returns when a snapshot
      takes longer to transfer than an election timeout); chunked
      InstallSnapshot declined until an image near the 2 GB frame
      exists (flow control, not correctness); the commit-wait as an
      `Ack` level declined by the contract (an offset exists only
      once applied, i.e. committed, so every level waits for the
      commit; the timeout is the one honest knob). BOX CLOSED: stages
      0-2c and pre-vote landed, the rest decided on evidence.

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
- [x] http-post-body-audit — DONE 2026-09-09. Netty and the JDK server
      DO read the body (Jetty's `posted` was the only defect and
      mcp-push fixed it) — but nothing asserted it on any backend but
      Jetty, so the audit's answer is a law, not a fix:
      `Acceptance.rest` runs against all three servers in
      TestBackends, proven able to fail. The entry's "NIO" was a
      misnomer: `Nio.scala` is raw TCP, not an HTTP server.

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
- [x] Once landed: migrate `okay-subscription`'s two maps and
      `okay-live`'s `Hub`/`Registry` onto it, and reconsider whether
      either module (or okay-demo itself) should become crossProject
      at that point — no JS/Native consumer is named yet, so this is
      NOT urgent; filed so the decision is made once, deliberately,
      not by accretion the next time this exact tradeoff recurs.
      PARTIAL 2026-09-03: `okay-subscription` migrated, a pure swap
      (9/9 existing tests unchanged). DONE 2026-09-07 (live-tdict):
      `okay-live`'s `Registry` over `TDict.computeIfAbsent`, `Hub`
      over `TList` — a pure swap, the module's last
      `java.util.concurrent` gone, tests unchanged. The crossProject
      question, decided: neither module moves — no JS/Native consumer
      is named, and a crossProject with one platform is a promise
      nobody asked for; the swap leaves the door open at the cost of
      one build.sbt line when someone does.

## okay-script (specs/okay-script.md) — markdown ```scala fenced blocks as Scala source
- [x] okay-script-site — LANDED 2026-09-06 (c0b37da2): the container,
      "a new JSP" complete. `Site(root).routes` serves a directory of
      `.md` pages over okay-http/okay-jetty (`/a/b` → `a/b.md`,
      `index.md`, `[param].md`, static files); `okay.script.api`
      (Web with form/cookies/body/params, Response with status/
      headers/redirect/cookie, Session, Error, include, forward) is
      DELEGATED to the host classloader servlet-API style — and so is
      `scala.*`, because delegating the API alone fails the JVM's
      loader-constraint check on `Option`/`Map` in its signatures
      (found on the first run). ```scala declare (JSP `<%! %>`),
      error.md / errorPage:, contentType: front-matter, TTL sessions.
      Per-thread stdout capture (Capture) replaces the JVM-global
      System.setOut, so concurrent requests are correct. Example
      okay-script/examples/site (a store with a session cart).
- [x] okay-script-multipart — LANDED 2026-09-06: multipart/form-data
      uploads reach a page as `Web.parts` / `Web.file(name)`
      (`api.Part`: name, filename, contentType, bytes); a multipart
      request's non-file fields land in `Web.form` too. Byte-level,
      binary-safe parser (`okay.script.Multipart`), no dependency;
      damage yields no parts rather than a 500. specs/okay-script.md
      "Uploads".
- [x] okay-script-cookie-flags — LANDED 2026-09-07: the session cookie
      had no `Secure` and no `SameSite`, and `Response.cookie` could
      express neither — found by answering "what goes to the proxy",
      and not something a proxy can add. `SameSite=Lax` by default;
      `secure` decided by the container (`secureCookies`, the Site's
      own TLS, or a trusted `X-Forwarded-Proto`), `OKAY_FORWARDED=1`
      for Serve. The first draft read the scheme from `Request.url`,
      which carries a path — the server knows what the request cannot
      say. specs/okay-script.md "Cookie flags".
- [x] okay-script-measured — LANDED 2026-09-07 (operator ask, first of
      four): the first numbers for a runtime-compiled page —
      ~150 ms to compile, 0.062 ms to answer (x2500), 870 ms for the
      first page of a process (dotc's warmup), metadata free, a
      static 304 at half a 200, 87 KiB per compiled page, ~100k
      renders/s saturating at four threads. Not JMH, and why.
      docs/benchmarks.md §19.
- [x] okay-script-warm — LANDED 2026-09-07 (second of four):
      `Site.warm()` compiles every page at boot (variants and
      fragments too, `i18n/` skipped) and names the broken ones with
      their errors; `Serve` warms before binding and goes on serving
      (a broken page answers its error page). `Site.stats` in
      `Store.Stats`' shape with JSON and Prometheus renderings as
      pure mappings, and `opsRoutes` (/healthz /stats /metrics) that
      is opt-in — `OKAY_OPS=1` for Serve, `orElse` for a caller, and
      a page of the same name still wins. The counter is
      `pageRequests`, not `renders`: the test counted 4 where the
      name promised 5. specs/okay-script.md "Warm and stats".
- [x] okay-script-image — LANDED 2026-09-07 (third of four):
      `ScriptDeploy.spec` + `okay-script/deploy` (Dockerfile,
      compose, Helm), drift-tested; the image runs `okay.script.Serve`
      over `/app/pages` (the example rides along so it runs out of
      the box; mount your own over it). `Serve` reads
      `OKAY_PAGES`/`OKAY_PORT` when given no command line. OKAY_DATA
      deliberately not baked in — /app is root's, the process is
      `okay`. Proven by running the jar: 13 pages compiled at boot in
      3.7 s, pages and /metrics answering.
      specs/okay-script.md "The image".
- [x] okay-script-guide — LANDED 2026-09-07 (last of four):
      docs/okay-script-guide.md, an empty directory to a running shop
      in ten steps — routing by directory, sessions, forms (plain and
      Schema-typed), a live page, two languages, a locked page and
      the `main` that gives a Site its verifier, what a page costs,
      and running it for real. Every command executed before it was
      written down; linked from docs/README "Start here".
- [x] okay-script-cache — LANDED 2026-09-07 (operator ask): static
      files always carry an ETag (size+mtime, no read) and
      Last-Modified and answer 304; pages opt in with `cache:
      <seconds>`, ETag of the body, and go `private` automatically
      when `secure:`, when a cookie is set, or when the session
      cookie rode in; only a plain 200 to GET/HEAD is cached. A 304
      saves the transfer, not the render — the counter test refuted
      the first draft's claim otherwise. specs/okay-script.md
      "Caching".
- [x] script-tls — LANDED 2026-09-07 (operator ask): HTTPS for a Site
      on the one transport seam — `Tls.serverContext` (okay-tls),
      `Jetty.serve(..., ssl)` (okay-jetty, no new dependency),
      `Site.serve(port, ssl)`, `OKAY_TLS_CERT`+`OKAY_TLS_KEY` for
      Serve; half a pair refuses by name. Live over openssl.
      specs/okay-script.md "HTTPS", specs/tls.md Results.
- [x] script-https-default — LANDED 2026-09-07 (operator ask: https out
      of the box, with a proxy and without, minimally). Four switches:
      `OKAY_TLS=self` (a self-signed PKCS#12 generated once by the
      JDK's own keytool, openssl as fallback, fingerprint printed with
      the warning it deserves), `OKAY_HSTS=<seconds>` (secure
      responses only, off by default — it pins a browser), 
      `OKAY_HTTPS_ONLY=1` (301 before routing, `Host` or a 400), and
      `OKAY_HTTP_PORT=<n>` (a second server whose only route is that
      redirect). This closes the redirect-port half of the old entry.
      specs/okay-script.md "HTTPS out of the box".
- [x] script-real-certs — LANDED 2026-09-07 (operator ask): a CA-issued
      certificate as it actually arrives. `Tls.privateKey` reads the
      algorithm from the PKCS#8 DER (RSA/EC/Ed25519/DSA) — the old
      code assumed RSA and refused every EC key certbot writes; a
      `fullchain.pem` is now PROVEN to be presented as a chain; and
      `Tls.reloading` re-reads cert and key when they change
      (`OKAY_TLS_RELOAD=<seconds>`), so a renewal needs no restart
      and a half-written one is not adopted. This closes the reload
      half of the old entry. specs/tls.md.
- [x] okay-acme — LANDED 2026-09-07 (operator ask): an RFC 8555 client,
      narrow on purpose (HTTP-01, one order, staging by default, an
      account key kept beside the certificate), wired into Serve as
      `OKAY_ACME=<email>` + `OKAY_ACME_DOMAINS`; the challenge rides
      the plaintext port ahead of the https redirect, the issued pair
      is read through `Tls.reloading` so renewals need no restart.
      Tested against a fake CA in-process. specs/acme.md.
- [x] acme-pebble — LANDED 2026-09-07, and it EARNED its keep on the
      first run: Pebble refused us with `badNonce`. `freshNonce`
      returned the Replay-Nonce of the HEAD it had just made while
      `send` had cached the same value from the same response, so the
      next POST spent it twice; our own double accepted a replayed
      nonce, a real CA does not. Fixed (a nonce is taken once) plus
      the §6.5 retry every client has. The test runs Pebble in docker,
      trusts the CA it generates per run through a test-scope Http,
      and asserts a real chain from an issuer that is not us.
- [x] acme-revoke — LANDED 2026-09-07 (operator ask): RFC 8555 §7.6.
      `Acme.revoke(cfg, http, reason)` posts the LEAF's DER signed by
      the account that ordered it, with an RFC 5280 reason as a named
      value; a second revoke answers the CA's own `alreadyRevoked`.
      `okay.acme.Revoke` is the operator's way to run it, over the
      directory Serve writes and the switches it runs with. Proven
      against Pebble.
- [x] acme-eab — LANDED 2026-09-07: `Config.eab` / `OKAY_ACME_EAB=
      <kid>:<key>` carries the inner HS256 JWS over our own account
      JWK that a commercial CA requires before it opens an account.
      Proven against a Pebble configured to require it. Fixed a
      fixture flake on the way: the Pebble tests shared a name and
      fixed ports, so one test could reach the PREVIOUS container and
      get a badNonce that looked like the client bug acme-pebble had
      just fixed — one instance per test now, ports from the OS.
- [x] acme-ari — LANDED 2026-09-07: `Acme.renewalWindow` reads the CA's
      suggested window and `ensure` honours it BESIDE `renewBefore`,
      never instead — either brings a renewal on, and a CA that
      publishes nothing or nonsense cannot stop one. The certID is
      the leaf's AKI keyIdentifier and serial read out of its own DER
      by hand (the JDK exposes the AKI only as raw extension bytes).
      Proven against Pebble's renewalInfo and, for the decision rule,
      a stub CA both ways.
- [x] acme-dns01 — LANDED 2026-09-07: an `Acme.Dns` seam (put/remove a
      TXT record, and say how long this provider propagates), dns-01
      used when one is given, and a wildcard refused BEFORE an order
      is placed when one is not. No provider ships, deliberately.
      Proven end to end: pebble-challtestsrv as both the resolver
      Pebble asks and the provider the test writes to, issuing
      `*.okay.example`.
- [x] acme-dns-providers — LANDED 2026-09-07: `Providers.cloudflare`,
      `.desec` and `.route53` — THREE, so that none is the favourite,
      and a deployment's own `Dns` stays as first-class. Credentials
      are `Secret`s, refusals are the provider's own sentence,
      propagation and endpoint are theirs and overridable, a delete
      names what it removes. Route 53 signs with the repo's own SigV4
      (okay-blob) rather than a second copy of the algorithm. Shape
      tests against a stub; the flow around them is Pebble's.
- [ ] script-tls: ALPN/HTTP2, OCSP stapling, cipher policy — still the
      proxy's, and named as such in the spec. A Site behind Caddy/nginx/an ingress needs
      three things from the operator: pass Upgrade for EVERY path
      (a live page's socket is on the page's own path),
      `OKAY_FORWARDED=1`, and to treat `X-Forwarded-For` as a claim.
- [x] okay-script-i18n — LANDED 2026-09-07 (operator ask): `Site(
      languages = ...)`; the request's language from `?lang=` (kept in
      the OKAYLANG cookie), the cookie, `Accept-Language` or the
      first; `page.<lang>.md` variants for routing, includes,
      forwards, login/error pages and the Live socket, inheriting the
      base page's front-matter (`secure:` holds for a translation);
      `t(key, args*)` from `i18n/<lang>.yaml` with fallback to the
      first language; `OKAY_LANGS` for Serve; the example in
      Ukrainian. specs/okay-script.md "Languages".
- [x] okay-script-serve — LANDED 2026-09-07 (operator ask: "where is
      the one line written?"): `Site.serve(port)` = `Jetty.serve(port)
      (routes)(ws, push)`, okay-jetty now a main dependency; the stock
      entry point `okay.script.Serve <pages-dir> [port]`, `OKAY_DATA=`
      for a persistent FileStore behind sessions and the application
      scope. specs/okay-script.md "Serving".
- [x] okay-script-application — LANDED 2026-09-07: JSP's `application`
      scope (`Application.current`: attributes shared by every page,
      typed through a Schema's JSON, `memory`/`persisted(store)`),
      `Site(issue = ...)` + `api.signIn(subject, scopes)` so a login
      page never holds an issuer, and the admin example on top:
      login.md (Password.verify → signIn), admin.md (`secure: admin`,
      `Live.form[Product]` + a plain `Forms` post into the catalog),
      index/product reading the catalog from Application.
      specs/okay-script.md "Application scope".
- [x] okay-script-forms — LANDED 2026-09-07 (operator ask): typed
      forms from a `Schema`, okay-ui's `Form` on both roads. Plain:
      `Forms.html[A]`/`Forms.read[A](Web.current.form, checks*)` — a
      `<form method=post>` with the fields' dotted keys as names,
      read back through okay-ui's own edit site (lists grown to the
      posted indices, a sum's case knob re-rendered until stable,
      empty text = absent, unposted checkbox = false), then errors,
      decode, checks. Live: `Live.form[A](submit, checks*)`. Example
      checkout. specs/okay-script.md "Typed forms".
- [x] okay-script-secure — LANDED 2026-09-07: declarative page security,
      web.xml's constraint. `secure: <scope>`/`any` and `loginPage:`
      in front-matter; `Site(verify = Some(...))` checks a bearer
      token from the header or the session attribute `okay.token`
      (`api.login(token)`/`logout()`); 302 to the login page with
      `?next=` when one exists, else okay-security's 401/403 ladder;
      no verifier → 500. Forwards checked, includes not; a secure
      page's Live socket checked from its cookie. `Principal.current`.
      specs/okay-script.md "Declarative security".
- [x] okay-script-live — LANDED 2026-09-06 (operator ask): okay-ui as
      the front-end layer. `api.Live(init)(view)(update)` declared at
      object level, `${mount("id", app)}` in prose: SSR of the first
      tree (React.elem → HTML) + script; `Site.ws` runs okay-ui's
      `Wire.serve` over the page's own WebSocket (`?__live=<id>`);
      `/__okay/live.js` is a dependency-free WireJson patch consumer
      served by the container. Classloader: `okay.*` shared with the
      host, and — found by the lifecycle test's ClassCastException on
      jetty's Server — any class on BOTH the page's classpath and the
      host's is the host's. specs/okay-script.md "Live pages".
- [x] okay-script-live: reconnect with state — a socket is one session
      from `init`; a browser that reconnects starts over. okay-ui's
      ui-durable (journal + refold on okay-persist) is the mechanism;
      key by session cookie. DONE 2026-09-07 (script-live-resume) for
      the RECONNECT: `Live[S]` keeps the state each session cookie
      last reached (a `TDict[String, S]` inside the app, no cast),
      `session(key)` starts from it and remembers on `Closed`;
      `Site.ws` reads the cookie, `mount` opens the session so the
      page sets one. In memory for a plain `Live`. DURABLE 2026-09-07
      (script-live-durable): `Live.durable(...)(using Schema[S])`
      keeps the state as a session attribute (`okay.live.<id>`, CBOR
      in base64) through the bound `Sessions.Handle`, so
      `Sessions.persisted`/`shared` make a restart and a second node
      resume too, swept by the session TTL; not a journal + refold —
      one value per session written on close is what a session
      attribute is, and nobody has asked for replay in between.
- [x] okay-script-live: server-pushed updates — a Live app only
      changes on a client event; a source merged in (`Ui.run`'s
      `external`) is what a ticking clock or a shared poll needs.
      DONE 2026-09-07 (script-live-push): `Live(init)(view)(update,
      push = Source[Event])` — the server's own events, a fresh
      instance per session, the same capability rule as the
      browser's; `Jetty.serve(port)(routes)(ws, push)` — an optional
      `push: PartialFunction[Request, Source[Frame]]` the transport
      feeds into the socket's input channel beside the client's, so
      `Wire.serve` is still used verbatim and `ws` keeps its callers;
      `Site.push` beside `Site.ws`. In-JVM and over Jetty: two
      patches arrive with nobody pressing.
- [x] okay-script-cluster-sessions — LANDED 2026-09-06 (operator ask):
      `Sessions.shared(topic)` — the persisted engine over a
      `Replicated` coordinator (leader node) or a `RemoteStore` topic
      over the wire (every other node), index kept current by tailing
      the topic, own offsets skipped. Found by the test: a touch was
      a full-state write, so a stale-cookie read on A racing an
      invalidate on B resurrected the session in log order — a
      logout that did not stick; a touch is now an 8-byte
      update-if-present record. specs/okay-script.md "Clustered
      sessions".
- [x] okay-script-persistent-sessions — LANDED 2026-09-06: `Sessions`
      is a trait; `Sessions.memory` (default, unchanged) and
      `Sessions.persisted(store)` over an okay-persist keyed compacted
      topic (whole state per write, tombstone on invalidate, rebuilt
      on open) so a Site's carts survive a restart. Expiry stays the
      sweep's job on the caller's clock — the first draft dropped
      entries in the rebuild by wall clock, a synthetic-clock test
      caught it. specs/okay-script.md "Persistent sessions".
- [x] okay-script core — LANDED 2026-09-03: `blocks`/`run`, one .md
      file = one compilation unit (blocks concatenate in document
      order, later blocks see earlier ones' val/def), driven through
      dotty.tools.dotc IN-PROCESS (no scala-cli subprocess, no custom
      language/interpreter). Success = compiles + runs without
      throwing; stdout captured. Investigated ../scalascript first —
      unrelated (a full custom markdown-as-syntax language), nothing
      reusable found, recorded as a negative result in the spec.
- [x-declined→reopened as script-cli's `check`] okay-script: sbt-test / CI integration — a task walking
      `specs/*.md` (or a configured dir), failing the build on the
      first `!ok` Result. Deliberately not built with the core
      (operator: "библиотека/API, без интеграции в sbt test пока").
      DECLINED 2026-09-07: the operator asked what it was for. It was
      the mdoc-era doc smoke test, from before the JSP turn; the
      specs here quote APIs as pseudo-code, so a blind walk fails on
      the first one and nobody marked opt-in blocks. `ScalaScript.
      check` exists for a caller that wants a document held to its
      output; a task over it is twenty lines when a document earns it.
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

## channel-sentinel-default — DONE 2026-09-04 (d7c69167)

Closed by `SentinelChannel`: the end-marker travels in the channel's
own element slot (`A | Mark`, one documented cast to read it back),
the layer owns `close` so the sentinel cannot be overtaken, and
`Channel.apply` defaults to it — ring under `MaxRing`, `Segments`
above, `StmChannel` only for capacity < 2. The entry sat open for a
day after landing; found by the 2026-09-06 audit. Original entry
follows, as the record of why the layer was chosen over the invariant.

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

## channel-bulk-send — DONE 2026-09-06 (feed-offer-first), without the primitive

Closed the other way round from this morning: the caller existed —
`Channel.buffer`'s feed, one `send` per element, 4000 handshakes for
4000 elements — and it did not need `sendManyNow`. `offer` is
synchronous and O(1); the feed now offers in a loop while the ring
takes and parks with one `send` only on the refused element, which
is `sendBlocking`'s own OFFER-FIRST rule applied one level up.

**2 279 774 → 1 411 988 B/op on `elem_effectCallback`, −38%.** `Slot`
and `Async$Await` left the allocation profile's top twelve; the four
`Channel` closure classes collapsed to one. Three laws through a ring
of 2 against thousands of elements: none lost, order kept, a producer
parked on a refusal released by close.

`sendManyNow` stays as landed in d13cfd72 — `private[okay]`, two laws,
and now with the reason it has no caller stated twice: representation
amortises the chunk feed, and offer-first amortises the element feed.

## reopened earlier the same day — the caller exists, and it is the feed

Closed this morning as "no production caller, by measurement". The
measurement that reopens it (channel-batch-floor, counted per side):
on the elementwise channel lane the CONSUMER makes 64 awaits for 4000
elements, and the PRODUCER makes 4000 — `Channel.buffer`'s feed sends
`c.send(it.next())` one element at a time, and each is an
`Async.Await`, a `Slot`, an acceptance callback and the interpreter
steps around them. Every per-element handshake on that lane is on the
send side.

`sendManyNow` is exactly the primitive for a producer holding a batch
of elements for an element channel, and a feed over a strict
collection (`St.iterator`) holds all of them. d13cfd72's caveat still
binds: bulk send LOSES 1.43x against a consumer that is not draining,
because a full ring fails every bulk scan. Here the consumer drains at
62 of 64. So the shape to build is `feed` offering runs through
`sendManyNow` while there is room and falling back to `send` when the
ring is full — and to MEASURE it on both consumer shapes before
believing it, since that is the trade the caveat names.

Expected size, so it is judged honestly: the producer's 4000
handshakes are the `Slot` (43 samples), `Async$Await` (31), the
`Channel` closures (185) and the `Platform` lambda (30) in the
allocation profile — roughly a third of the 672 bytes per element, and
all of it on the producer's fibre.

## as closed this morning — DONE 2026-09-05 (d13cfd72)

Closed by `Ring.pushMany` + `Channel.sendManyNow` (`private[okay]`,
two laws, each with two threads contending for the same claim). The
primitive was the smaller half of the result; two findings outrank it.

`feedChunked` never needed it — it amortizes by REPRESENTATION, putting
whole chunks into a `Channel[Chunk[A]]`, so the absence of a production
caller is the measured answer, not an omission. And the bulk send is
66.9us against a draining consumer (1.71x past `zioChunked`) but 280.4
against an elementwise one — a 1.43x LOSS, because a full ring makes
every bulk attempt fail its scan and fall back anyway. Batch both ends
or neither. Original entry follows.

`Ring.popMany` batched the consumer's head CAS; `push` still takes the
tail one element at a time, so a chunked SEND (`Channel.mergeChunked`,
`feedChunked`) pays a tail CAS per element the way the receive side
used to pay a head CAS. Symmetric fix: claim a run of writable slots
with one `compareAndSet` on the tail, then publish each stamp. Wants
the same contending-producers law the bulk receive got.

## channel-callback-allocation — HALF DONE 2026-09-05 (d13cfd72)

The send half is closed: `Accepted` plus `CanBlock.blockAccepted` end
the boxing of the acceptance answer, so `boxToBoolean` is off the
send path.

What remains is the RECEIVE half, and it remains BY DECISION, not by
neglect: `receiveBlocking` returns `Option[A]` and `End` is
`Either[Throwable, Option[A]]`, so the `Right`+`Some` pair is in the
return TYPE, not in the implementation. Removing it means the
dedicated SAM below — an abstract primitive on `Channel` and every
implementation with it. Do not reopen this as "two allocations per
element"; it is one allocation pair on one side, priced against a
public signature. Original entry follows.

Leaf samples on the elementwise lane: `boxToBoolean` 8% (Function1 is
not specialized on Boolean, so every `sendAsync` callback boxes) and
`Right.apply` 3.4% plus the `Some` beside it (`End =
Either[Throwable, Option[A]]` wraps each element twice). Chunking
makes both per-batch, which is why they were left; they still stand on
the elementwise path. A dedicated SAM with `onValue`/`onEnd`/`onError`
removes both without a cast, but it changes an abstract primitive on
`Channel` and every implementation with it.
- [x] intent-examples-in-language — the candidate this lane deliberately
      did not confound into itself: the example MESSAGES stayed English
      throughout, so the native-names arm moved one variable. Translating
      the five few-shot examples is untried, and examples OF A CLASS are
      the one lever that has consistently paid here — unlike every prose
      addition, which has now cost four times running. MEASURED
      2026-09-07 (`TestExamplesInLanguage`, seven languages, English
      names, the same decoder): the five examples in the message's
      language against the same five in English — better on five of
      seven (pl 0.792 → 1.000, uk +0.071, ru +0.066, fr +0.040, de
      +0.030), one message of thirty worse on es and ja, +0.049 on
      average; the lever holds, and its sign is the reading (thirty
      messages resolve to 3.3 points each). The translations ship as
      `IntentFixture.meetingExamplesIn(lang)` for a caller that knows
      the language; the author's, second-author limitation applies.
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
- [x] intent-rule-induction — MEASURED 2026-09-07, not a replacement
      at this corpus size: `Induced` (RIPPER-lite — literals are words,
      adjacent pairs and either at the start; grown by FOIL gain on two
      thirds of the training half, pruned on the third, kept on the
      whole half at a support and a precision floor; deterministic; a
      `Trained` is rules a person can read) beside `Patterns.meeting`
      on the same held-out rows: hand-written fire 53.3% at 90.6%;
      induced at floor 0.8 fire 51.7% at 67.7%, at floor 0.9 (the
      default) 11.7% at 85.7%; support 3 or 4 is worse on both. At
      sixty rows induction buys coverage or precision, not both — the
      corpus is the limit, as intent-static-embeddings and the learning
      curve found for the other tiers; the tier stays, with the grid
      in the suite, for the corpus that grows. Original: patterns are
      88.6-90.9% accurate where
      they fire and fire on only 58.3% of messages, and the cues are
      hand-written. Induce them instead (RIPPER-style: grow a rule,
      prune it against held-out data, repeat) so coverage grows with
      the corpus rather than with someone's patience. Keeps the zero-
      network property, which nothing else above 60% has.
- [x] intent-tfidf-word-linear — DONE 2026-09-07: `WordTfIdf` (a
      vocabulary and IDF fitted on the training half, the vector into
      `Probe`), `TestWordTfIdf` in the default gate beside
      `TestCharGrams`. Same split, same session: **61.7% against
      chargrams' 65.0%** on English (the 60.0% below is the older
      fixture) — so the n-gram tier's number is about having a linear
      model, not about characters, to within three points. Per
      language (15 rows each, thin): tf-idf en 53 / fr 67 / de 27 /
      es 27 / ru 40 / ja 27; chargrams en 40 / fr 47 / de 40 / es 20 /
      ru 53 / ja 60 — characters win exactly where words are not the
      unit (no spaces in ja, inflection in ru), words win on fr.
      51 us per message, a 66 ms fit, 303 words. Original: the
      classical baseline nobody ran:
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
      GATED 2026-09-07, the gate measured: "only worth it if the
      server is the problem" — intent-4b-with-more-data found both
      embedders flat from 32 examples with the same slope, and every
      no-network tier since (chargrams 65, TF-IDF 61.7, the static
      table at 68.3 with triples and PCA) meets the same ceiling from
      a different road: the limit is register and context, not the
      server. A trained subword head would be a fourth road to it.
      Opens if a fixture at least twice this size shows a late slope.
- [ ] intent-grammar-parse — intent by GRAMMAR over `okay-lex` and
      `okay-parse`, the way `Temporal` does dates: deterministic,
      explainable, and refusing rather than guessing. Expensive in
      rules, and the honest reason to want it is a domain where a wrong
      answer is worse than no answer.
      GATED 2026-09-07: no consumer has named a domain where a wrong
      answer is worse than no answer; the model tier at 0.909 macro
      F1 with zero undecodable replies, the slot parsers (when,
      duration, people, amount) already refusing rather than guessing
      where a wrong value would act. Opens with that consumer.
- [ ] intent-crf-slots — sequence labelling for the frame's SLOTS
      (who, when, where) rather than its class. `Temporal` fills one
      slot with a parser; the general case is a tagger, and a CRF is
      the classical one. Only after the class problem is settled.
      GATED 2026-09-07: "only after the class problem is settled" —
      it is settled for the model tier (0.909, deterministic, the
      decoder reading every reply) and the four parsed slots cover
      what the meeting frame asks; the slots still open (who, places)
      are named entities, which is what a tagger is for, and which no
      frame in the fixture yet asks a question about. Opens with a
      frame that does.
- [x] intent-active-learning — labels are the bottleneck everywhere
      above, so choose the next ones to label by uncertainty rather
      than by order. Directly compounds with `intent-label-distillation`
      (the model labels, a human confirms the uncertain ones), and
      needs the calibrated confidence `intent-no-model` is building.
      SIMULATED 2026-09-07 (`TestActiveLearning`, the fixture's own
      labels, both mirror splits): uncertainty (the probe's smallest
      margin) against random, +2.4 / +1.0 probe points over the run
      and 80% reached at 28 labels against random's 36 and order's
      52; the classical dip at 32, recovered by 36. Order is a straw
      man on a class-grouped fixture, and the straw says: balance the
      classes first. The ordering needs no calibration — a threshold
      would; it belongs with the review queue (Harvest signal 6), one
      line over `Probe.score(_).margin`.
- [ ] intent-ensemble-weights — `NoModel` blends the probe with the
      pattern tier using ONE fitted weight from a six-point grid,
      because sixty rows cannot support a fitted second-level model.
      When the corpus grows (see distillation), replace the grid with a
      real stacking model and measure whether it beats the blend.
      GATED, and the gate is now measured (2026-09-07): the corpus did
      not grow honestly — intent-distil-dose found the distilled rows'
      gain to be one split's, intent-distil-static found them worth
      nothing to the static table, intent-distil-diversity found them
      a third as diverse as the fixture — so a second-level model
      trained on them would learn the generator's register. Opens when
      the human fixture passes ~200 rows (the review queue is the
      source); the six-point grid stays until then.

## channel-chunk-batch-size — REFUTED TWICE 2026-09-06: the consumer already batches at 62 of 64

Taken as channel-batch-floor on the finding that `receiveMany(64)`
hands back ONE element — "4022 handshakes for 4000 elements". Built:
a watermark on the four push-side wakes, a dwell timer as the latency
bound, `drained(atLeast, dwellMillis)`, five laws, and a probe that
priced the dwell (plain 0.2ms; floor16 with dwell 1/5/20ms: 2.0 / 8.5
/ 28ms — `Timer.after` on Loom adds ~1ms plus 40% over the asked
sleep). Then the handshakes were counted PER SIDE, with a counting
`Handler[Async]` wrapped around the consumer's `runWith` only:

| read | awaits (N=4000) | elements per await |
|---|---|---|
| `drained` (default) | 64 | 62.5 |
| `drained(16, 1)` | 64 | 62.5 |
| `drained(64, 1)` | 64 | 62.5 |

**The default already takes a full batch.** There is no ping-pong and
nothing for a floor to do; the whole lane was reverted, laws and all.
The "4022" was a GLOBAL counter in `CanBlock.block` that summed the
producer's 4000 sends with the consumer's 64 receives (4064 − 64 =
4000, one per element sent) and was read as the consumer's. See
`channel-elementwise-wakeups` for the correction and
`channel-bulk-send` for where those 4000 actually live.

What survives from the original entry below is nothing about batch
SIZE. Its numbers predate two rounds of channel work and its central
claim is now measured false; keep it only as the record of the idea.
The one durable artefact is the method: count per side, never with a
static counter both fibres can reach.

## the original entry — PREMISE REFUTED, the idea survives

Audited 2026-09-06. The entry opens by naming "the one lane we lose to
`zio.Queue` — `zioStrongChunk` at 128.0". We do not lose it: d13cfd72
re-measured both sides on one granularity axis and the layered chunked
lane reads **115.9 against zioStrongChunk's 124.0**. Whoever takes
this must not take it to close a gap that is already closed.

What survives is the MECHANISM, which is worth having on its own
terms: a ring wakes a receiver on every push, so average elements per
bulk receive are 43.5 where `StmChannel`'s transaction hands over
363.6. The watermark direction is still the cheap one, and is now
about throughput headroom rather than a deficit. Original entry
follows; its comparison numbers predate the send fast path and the
chunked feed, so re-measure before quoting any of them.

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

## channel-elementwise-wakeups — CORRECTION 2026-09-06 (later the same day): the 4022 was both sides

The count below — `fast=4022 slow=42 parks=42`, read as "the consumer
calls `block` once per element" — came from a STATIC counter in
`CanBlock.block`, which both fibres reach. Counted per side with a
`Handler[Async]` wrapped around the consumer's `runWith` alone, the
consumer performs **64 awaits for 4000 elements** — a full batch every
time. The other 4000 are the PRODUCER's: `Channel.buffer`'s feed does
`c.send(it.next())` once per element, and every `send` is an
`Async.Await` through `block`. 4064 − 64 = 4000, one per element sent.

So the conclusions here that rested on the consumer stand — it does
not park, the watermark was never the lever, the 672 bytes are the
representation — but the per-element HANDSHAKE cost is real and it is
on the send side. That is `channel-bulk-send`'s territory, reopened
with this evidence. The `AtomicBoolean` removal stands too: it saved
one object per handshake on whichever side made it.

## the entry as first written — MEASURED 2026-09-06: the premise is wrong, and the cost is the representation

Taken, counted, and the entry's own framing does not survive.

**The consumer does not park.** Counters in `CanBlock.block` on this
lane's shape, N=4000: `fast=4022  slow=42  parks=42`. Once per element
`block` runs, and 99% of the time `register` has already completed —
the element was there, there was nothing to wait for. So "one unpark
per element on the consumer's critical path" is not what happens, and
the entry's second proposal, waking on a WATERMARK, is not the lever
here: there are 42 wakeups to save, not 4000.

That also retires the reading of the `-prof stack` profile that
promoted this entry. Three quarters of thread time is parked, but it
is the producer fibre and idle scheduler threads — not the consumer.
Counting the consumer directly is what settled it; the sampler could
not.

**What the handshake did cost was an allocation, and one of them is
gone.** `Slot` and `BoolSlot` held `filled` as an `AtomicBoolean`
although it is only ever SET and READ — never compare-and-set — so a
plain `@volatile var` has exactly the same memory semantics and one
fewer object per handshake. Verified by counting bytes, which is the
one measurement a loaded box cannot distort:

| variant | B/op (two runs) | GC count |
|---|---|---|
| `AtomicBoolean` | 2 689 282 / 2 689 255 | 25 / 24 |
| `@volatile` | 2 624 432 / 2 624 424 | 19 / 20 |

64 840 B/op against 4064 x 16 = 65 024 predicted — 0.3% off — and a
fifth fewer collections. **The timing effect was NOT measured**: an
alternating A/B ran while a sibling's job took the box from load 8.5
to 48, and its numbers (240 -> 1749 -> 2191 -> 6607, error bars to
±9438) are void and are recorded here only so nobody mistakes them for
a result. The change lands on the allocation count and on being
semantically identical, not on a speed claim.

**The real number this lane found is 672 bytes per element.** 2.62 MB
per operation over 4000 elements, and the 16 bytes above are 2.4% of
it. A JFR allocation profile says where the rest goes — and it is not
one hole:

| class | samples |
|---|---|
| `Free$Bind` | 161 |
| `Channel` lambdas (four distinct classes) | 185 |
| `Right` + `Tuple2` (uncons's answer) | 140 |
| `Free$Inject` / `Free$Pure` | 59 / 52 |
| `Slot` | 43 |
| `Async$Await` / `Drain` / `Writer$Say` | 31 / 27 / 21 |

Free nodes are 272 samples and closures about 248. That is the
REPRESENTATION: a fragment of program is built and discarded for every
element. There is no single allocation to remove, which is the same
conclusion `channel-per-element-effect-cost` reached from the other
side, and the way not to pay it is not to go per element —
`bufferChunked` already pays it once per 256 elements and reads 19.66.

**Left open, with the trade named.** `Drain` batches by holding a
chunk and calling `receiveMany(64)`, yet `block` runs once per
element: the batch comes back with ONE element, because producer and
consumer ping-pong and the ring never accumulates. Making the batch
real means holding a woken receiver for a dwell or a watermark —
throughput bought with latency, which is `channel-chunk-batch-size`,
and it should be taken there with that trade stated, not here.

## receive-blocking-path-length — DONE 2026-09-06 (§17g): the head CAS was 35%; a single-consumer ring takes it, −25% elementwise

Profiled first (JFR): the head compare-and-swap in `Ring.pop` is 35%
of the elementwise consumer, the pop half of it. Landed
`Queues.strong[A].bounded(n, singleConsumer = true)` — `Ring`'s flag,
`pop`/`popMany` moving the head by a release store — and the actor's
default mailbox built with it. `okaySentinelElem` 202.9 → 152.1 us at
the same bytes; chunked and both actor regimes unchanged within their
bars. Laws: the new flavour answers for ten, the
contending-consumers law is recorded as not claimed. The `Handoff` per
call (8%) and the slot read (14%, the element itself) are what
remains; not taken. Original entry:

### as filed — the 3.7x between elementwise and chunked is path, not parks and not bytes

Filed 2026-09-06 by `channel-elementwise-wakeups`' closing count.
`okaySentinelElem` 205.9 us and `okaySentinelChunk` 55.0 allocate the
same 77–80 bytes per element and neither side parks (§17f), so the
150 us between them — ~37 ns per element — is what one
`receiveBlocking` does over a chunk's per-element share: a `Handoff`
made per call (`CanBlock.handoff()`), `receiveInto`'s scan with its
shared reads (`ended`, `reached`, `endPending`, the senders' queue
head), the `Mark` match, and `await`'s check of `filled`. Candidates,
to measure not assume: a per-thread `Handoff` reused across calls
(one thread blocks on at most one at a time; JVM/Native only, JS has
no `CanBlock`); the shared reads folded into one; nothing that adds
a read of the producer's line. The A/B lane is `okaySentinelElem`
against `okaySentinelChunk` under `-prof gc`, and `TestChannelLaws`
is the law.

## the original entry — MEASURED AND CLOSED 2026-09-06: the wakeup per element is not there

Counted, on the entry's own shape (`okaySentinelElem`: N=4000,
cap=1024, a virtual-thread producer, the consumer on
`receiveBlocking`), 100 runs after 200 warmup, three processes:
**sender wakeups per element 0.000–0.001, sender parks 0.003–0.005,
receiver parks 0.000.** Twelve to twenty producer parks per four
thousand elements, and the consumer never parks at all. There is no
unpark on the consumer's critical path to amortise; a watermark wake
policy would remove nothing. The harness re-taken the same day (first
time since the chunked feed, `-prof gc`, two forks): `okaySentinelElem`
205.9 ±17.5 us / 307 668 B against `okaySentinelChunk` 55.0 / 318 318
B — the SAME 77–80 bytes per element on both, so the 3.7x is path
length per `receiveBlocking`, not allocation and not parking;
`okayStrongElem` (StmChannel) 250.1, so the "SentinelChannel behind
StmChannel" line below is stale too, and the 208.9 → 268.7 regression
does not stand (§17f). What is left of the elementwise cost is the
~37 ns a `receiveBlocking` spends over a chunk's share — a `Handoff`
made per call, the scan, the shared reads — and that is a different
entry if anyone wants it (`receive-blocking-path-length`). The
three-quarters-parked profile that promoted this entry was taken on
another lane at another time; it did not survive a count. Original
entry follows.

### as promoted — OPEN, the PRIMARY lane

Promoted 2026-09-06 by free-cont-stack, which went looking for the
per-element cost in the interpreter and found it here instead. A
`-prof stack` of the elementwise lane puts 58.3% of thread time in
WAITING (86% of that `Unsafe.park`) and another 16.8% in
TIMED_WAITING, all park — three quarters parked — against 24.9%
RUNNABLE, of which the entire effect machinery is about 10 points.
`channel-per-element-effect-cost` closed as an interpreter lane and
points here; read it for the counts and the caveats.

Audited 2026-09-06: still genuinely open — one unpark per element is
on the consumer's critical path and nothing since has addressed it.
But every number below predates the chunked feed (9fe22fdc), and they
come from the guarantee/granularity harness, NOT from the idiomatic
one where the elementwise lane reads 209.3 today. Do not cross the two
sets, and do not assume the 208.9 -> 268.7 regression still stands:
re-run this entry's own harness before deciding the size of the prize.

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
- [x] intent-instruction-prefix — a short "Classify the intent of this
      message: " prefix measured +1.6 (probe) and +3.3 (centroid), and
      the spread across four framings was 6.6 points. Both are at or
      near the noise floor on 60 messages: re-measure on the grown
      fixture before making it the default, and keep the finding that
      LONG instructions cost (81.7% for the e5-style one). RE-MEASURED
      2026-09-07 on the 120-message fixture, both mirror splits, the
      bare split-to-split spread as the bar (`TestPrefixGrown`): the
      bare probe is 10.0 points apart between halves; the classify
      prefix is +1.7 / +0.0 (probe) and −1.7 / +3.3 (centroid); the
      e5-style one +10.0 / −5.0 and +5.0 / −15.0 — a coin, not a cost.
      Nothing clears the bar; no default moves. The finding is the
      bar: sixty test messages bound every single-split claim at about
      ten points, which is the fixture's size, not the embedding's.
- [x] intent-static-trigrams-and-pca — two obvious extensions of the
      static table, both filed rather than guessed: adjacent TRIPLES as
      well as pairs (pairs were worth 11.6 points, and the same
      argument applies once more with diminishing returns and a bigger
      table), and `model2vec`'s PCA step to cut 1024 dimensions to
      256 — 1303 units already cost 5.2MB as float32, and a production
      vocabulary of 30k would be 120MB. MEASURED AND LANDED
      2026-09-07: one run, same split, baseline re-measured beside —
      triples +5.0 probe / +11.7 centroid; PCA 256 keeps 91.5% of
      the variance at a quarter of the bytes and GAINS five probe
      points (a denoising); together 68.3% / 66.7% at 2.1 MB against
      61.7% / 53.3% at 5.3 MB. `Static.units3`, `Static.fitPca` /
      `projected` / `variance`; `TestStaticMore` (Live) holds the
      table. 30k units ship at 30 MB (256) or 15 MB (128).
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
- [x] intent-4b-with-more-data — the 4B embedder is worse at 60
      examples because 2560 dimensions need more of them, which is a
      prediction rather than a defeat: re-run the learning curve on
      BOTH embedders and find where the lines cross. If the 4B
      overtakes past some n, it is the right vectoriser for a
      distilled corpus even though it is the wrong one today.
      MEASURED 2026-09-07 (`TestLearningCurveBoth`, both mirror
      splits, n = 8..60, Conditions on every row): the lines do not
      cross — at no n is the 4B ahead on both splits, its means sit
      level with or under the 0.6B's (at 60: probe 78.3 vs 80.0,
      centroid 75.8 vs 78.3), and both curves flatten from 32 with
      the same slope. The prediction is not supported inside this
      fixture; a crossing past 60 is a claim it cannot make. The 0.6B
      stays the vectoriser, for a distilled corpus too.
- [x] intent-distil-more — 320 generated messages bought chargrams 6.7
      points and the curve was still climbing when the fixture ran out.
      The generator is resumable, so this is machine time rather than
      work: raise the target, re-filter, and find where the gain stops.
      Also worth trying on the STATIC table, which was not fed here
      because its vocabulary would have to be re-embedded — a second
      pass over the teacher rather than a change of method. DECIDED
      2026-09-07: the static half MEASURED (intent-distil-static,
      `TestStaticDistilled`, both splits) — the corpus as vocabulary
      0.0, as weights +1.7 (one split), as 20 training rows −1.7 /
      +1.7, a table 1.7x larger for nothing: a static table's ceiling
      is context, and a corpus supplies units; the generation half
      DECLINED on intent-distil-dose's finding that the dose's gain
      was one split's — more of it is more of that.
- [x] intent-distil-diversity — the distilled corpus alone scores TEN
      POINTS below a human fixture a third its size, which says its
      distribution is narrow rather than its labels wrong. Prompting
      for a persona, a register or a length before each batch is the
      cheap thing to try; measuring the corpus's own diversity (say,
      distinct trigram ratio against the human fixture's) is the honest
      way to tell whether it worked. MEASURED 2026-09-07
      (`TestDistilDiversity`, offline, equal samples of 120, twenty
      draws): distinct-3 69.1% (all) / 64.3% (self-consistent) against
      the fixture's 98.6%; vocabulary 295 vs 457; openers per message
      41% / 34% vs 85% (`could you` x21, `your access` x13 in 182
      rows); the two vocabularies overlap under half. The filter
      keeps the formulaic. The persona half is not built: a later
      corpus has its number to beat (distinct-3 and openers within
      ten points of the fixture on equal samples), and the dose and
      static lanes have priced what missing it buys.
- [x] intent-distil-dose — a little distilled data is worth ten points
      to the centroid (80.0% -> 90.0% at +40 rows) and more is worse,
      monotonically, down to 78.3% at +320. The optimum was found by
      accident between two arms; find it properly, and find out whether
      the self-consistency filter (which kept 182 of 320 and was NOT
      applied in that run) moves it. Also worth asking whether the
      right knob is a dose at all or a WEIGHT — distilled rows counted
      at less than one in the fit, which a centroid can express and a
      grid over doses cannot. MEASURED 2026-09-07 (`TestDistilDose`,
      both mirror splits, grid 0..320, filtered and not, weights):
      the optimum was a split artefact — test-even reproduces 78.3 →
      93.3 at dose 40–50, test-odd goes 78.3 → 70.0 at the same dose
      and no dose ever lifts it; the probe never gains. The filter
      (182 of 320, its verdicts now data beside the corpus) moves the
      peak to 20 and softens the overdose, not the verdict. The one
      cell ahead on both splits is the filtered pool at weight 0.10
      (+1.7 / +8.4). No default moves.
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

## channel-sender-livelock — DONE 2026-09-06 (this lane)

`Ring.hasRoom` answers from the stamp of the position the tail is
about to claim, exactly as this entry prescribed, and it landed with
the relaxed queues (cb51748c). But it was only WIRED on the default
lane: `SentinelChannel` asks `hasRoomAt(route)`, while
`AbruptChannel:101` still subtracted `ring.size < ring.capacity`. The
audit found the fix half-applied and finished it — the weak channel
now asks `ring.hasRoom` too.

The general lesson, this being the fifth defect of one family: adding
the right primitive is not the fix; every caller of the wrong one is.
Grep for the SHAPE (`size <`, `isEmpty`) after landing its
replacement. Original entry follows.

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
- [x] intent-split-other — MEASURED AND DECLINED 2026-09-05. Carving
      the bin into Social/Support/Errand takes `Other` recall from
      46.7% to 6.7% (composite 26.7%) and all three new classes score
      F1 0.00, because the training half has 15 `Other` rows and three
      ways leaves 4-6 each. A two-way carve also collapses (13.3%), so
      it is row count and not where the line is drawn. All three
      remedies are now measured and the status quo wins. Original
      entry follows.
- [x] intent-split-other (original) — NOW WITH A NUMBER (2026-09-05): `Other`
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
- [x] frame-rebind — DONE 2026-09-07: `Frame.rebind(rebuilt*)` answers
      a `Rebound` — the frame re-bound by NAME to the rebuilt
      descriptors, each stored answer re-read from its text by the new
      parser; `rederived` lists every value that came out different
      (name, text, before, after), `lost` the names the new descriptor
      could not read (their words kept in `unread`) or no rebuilt slot
      carries; `clean` when neither. Laws in `TestFrame`; `TestRebind`
      in okay-intent shows the hazard as filed — "next Tuesday" against
      a reference day a week later is a different date, and the move
      is reported, not made silently. Original: after a restart a
      caller rebuilds its `Slot`
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
## the autonomy programme (specs/intent-autonomy.md — the seams, the literature, the staged plan; measurements in specs/intent-classify.md)
Operator's direction: aim this line at working WITHOUT models. The
metric, the measured dead ends and the order are in the spec section;
these are the lanes.
- [x] intent-autonomy-report — LANDED 2026-09-07 (MeasureAutonomy,
      offline, in the ordinary suite): the autonomy rate and the
      handed-over share, per tier and per abstention floor, plus the
      promise table a caller reads. The finding: "80.0% at full
      coverage" was the worst of the available promises — at margin
      0.5 the same tiers answer 73.3% of traffic at 88.6% precision
      and hand over 26.7%. The per-class law is asserted here too.
      specs/intent-classify.md, "The autonomy programme".
- [ ] intent-annotate-log — the model reads the LOGS and proposes
      labels for REAL messages (operator's direction; specs/intent-classify.md,
      "The harvest programme"). Not the distillation that failed: the
      model writes nothing, the messages are real traffic, only the
      label is proposed — the practice the literature supports
      (arxiv 2406.17633, 2503.17336). A row is kept only if the
      reading grounds in the message, conf >= Medium, k samples agree,
      and no deterministic tier contradicts at high margin; provenance
      per row (model, prompt fingerprint, date, filters passed).
      Criterion: 100+ kept rows, and a refit on them moves the
      autonomy rate without breaking the per-class law.
- [ ] intent-coannotate-queue — CoAnnotating (arxiv 2310.15638):
      route by UNCERTAINTY, so a person arbitrates only what the
      filters could not settle, ranked by tier disagreement. Our
      active-learning lane already measured the shape (28 labels
      against 36 random for the same gain). Criterion: human effort
      per point of autonomy, not accuracy alone.
- [x] intent-label-model — MEASURED AND DECLINED 2026-09-07
      (MeasureLabelModel, offline). Six offline labelers combined by
      agreement-estimated weights (Dawid-Skene / Snorkel's idea),
      against the cascade. Correlated labelers inflate each other
      (prefix ranked 0.833 at 58.3% real precision); dropping them
      fixes the weights (cues first at 0.914) and one split looked
      good; eight splits took it back — 72.2% precision at 82.9%
      coverage against the cascade's 77.5% at 89.6%, worst class 0.42
      against 0.56, ahead on 1 split of 8. Nothing ships; the
      estimator lives in the test sources. Reopen with many more
      INDEPENDENT labelers or with dependency modelling.
      specs/intent-classify.md, "Results — intent-label-model".
- [ ] intent-discover-classes — Other is several real classes nobody
      named; cluster what lands there and name the clusters with the
      model (Dial-In LLM, ACL 2025.emnlp-main.300, >95% agreement with
      human judgement on 100k real calls; NILC, arxiv 2511.05913,
      WSDM 2026), then a person accepts or rejects each proposal.
      Criterion: Other's recall after the split, and how many
      proposals survive review.
- [ ] intent-noise-aware-refit — if harvested labels prove noisy
      enough to bind, the noise-aware refinement the literature
      reports (arxiv 2505.19675, ~7% recovered). GATED on a
      measurement showing noise is the limit.
- [ ] intent-label-queue — which rows to ask a person about: small
      margin plus disagreement between tiers (the shape the
      active-learning lane measured at 28 labels against 36 for the
      same gain). Ships as a queue the admin flow can drain.
- [x] intent-refit-gate — LANDED 2026-09-07: `Refit.propose` fits a
      candidate, scores it and the incumbent on the same held-out
      rows, and answers Accepted/Refused with every class before and
      after; the law (no class below F1 0.50) and the slide rule (no
      class down more than 0.10 against the incumbent) both enforced,
      `MakeModel` writes only on Accepted and `--force` says so out
      loud. The shipped corpus passes and reproduces the artifact byte
      for byte. specs/intent-classify.md, "Results — intent-refit-gate".
- [ ] intent-induce-on-harvest — re-run cue induction whenever the
      corpus grows and ship the induced cues beside the hand-written
      ones. Cues need nothing at run time, so their coverage is pure
      autonomy: 85.7% precision at 11.7% coverage on 60 rows today,
      against hand-written 90.6% at 53.3%.
- [x] intent-offline-slots — MEASURED AND PARTLY FIXED 2026-09-07
      (MeasureSlotCoverage + TestTemporalForms, offline). The offline
      extractors filled `when` on 24.2% of the fixture; the printed
      miss list showed four real shapes, two of which are correct
      refusals (a bare time with no day, a range like "this week").
      The other two were word forms the tokeniser could not see — a
      possessive ("tomorrow\'s meeting") and a plural weekday
      ("Thursdays") — now handled: `when` 24.2% -> 29.2%, recall
      against the hint denominator 66% -> 80%. NO sequence labeller:
      the gap was four word-shapes, not a learning problem.
      specs/intent-classify.md, "Results — intent-offline-slots".
- [x] intent-slot-denominators — MEASURED 2026-09-07, NOTHING TO FIX.
      `duration` fires on 7 of 120 and `people` on 1; the hints
      suggest 19 and 5, and every one of the sixteen misses is a
      substring false alarm ("min" in *reminder*), the same word
      meaning something else ("take the minutes"), or a vague phrase
      with no number ("a quick chat"). The two refusals a widening
      would break first are now asserted (TestSlotRefusals). The
      fixture simply does not carry these slots.
      specs/intent-classify.md, "Results — intent-slot-denominators".
- [ ] intent-per-language-models — one artifact per language once
      rows exist; the shipped one is English-only and scores chance
      (23-30%) elsewhere. GATED on intent-language-fixture-growth.

- [x] intent-offline-other — MEASURED AND DECLINED 2026-09-07
      (TestOfflineGate, offline, no network). The offline analogue of
      the model path's binary gate: a second CharGrams model on the
      in/out task before the four-way tier. By argmax it never fires
      (15 out-of-domain rows against 45 collapse the fit to the
      majority); balanced 15/15 it fires and the tier dies (binary
      accuracy 50%, AUC 0.615). The representation DOES see it —
      p(OUT) ranks at AUC 0.843 — but over eight random splits a
      threshold buys 0.13 of Other recall for 0.4 points of total and
      wins on 3 splits of 8. Nothing ships; the binding constraint is
      15 training rows, not the algorithm. specs/intent-classify.md,
      "Results — intent-offline-other".
- [ ] intent-other-more-rows — SHARPENED 2026-09-07 by
      intent-offline-other, which turned this from "more data would
      presumably help" into a measured blocker: an out-of-domain
      detector over the same rows already RANKS at AUC 0.843, and
      every decision rule built on it is starved by 15 training rows
      (by argmax it cannot fire at all; balanced it destroys the
      tier). 40-60 real out-of-domain English rows — the operator's,
      or harvested from the service's own traffic into
      okay-chat/corpus/harvested.json — and TestOfflineGate re-runs
      unchanged to settle it. Still needs human rows: the distillation
      lanes measured generated ones to be worth nothing (they carry
      the generator's register).
- [x] intent-english-corpus-twins — LANDED 2026-09-05. Three
      near-twin pairs rewritten, the guard asserted on `labelled`, the
      artifact regenerated and every published number re-measured:
      composite 76.7% -> 75.0%, near half 86.7% -> 83.3%, far half
      unmoved. Original entry follows.
- [x] intent-english-corpus-twins (original) — `TestFixtureHygiene` found three
      near-twin pairs in `labelled`, the English corpus the shipped
      model is fitted on and every published number is measured
      against ("Suggestion: we meet on Monday at 9" against "Suggest
      we cancel Monday and meet Wednesday instead"; two Requests that
      both say "send me the agenda"; two Notifications about a room
      change). Fixing them changes the shipped artifact and every
      number quoted from it, so it is its own lane with its own
      re-publish rather than a tidy-up.
- [x] intent-span-runaway — DONE 2026-09-07: `Reading.grounded
      (message)`, the decoder-side guard — a span is kept only if its
      text is in the message (grounding) and its stretch is not
      already covered by a span kept before it (distinctness, the
      check the twenty-span run needed: every text was a real
      substring). Matching on lowercased, whitespace-flattened text,
      first occurrence, the model's order kept. Four laws in
      `TestClassify` (the twenty-span cycle collapses to two, an
      ungrounded span drops, a clean reading is the identity, overlap
      is judged on stretches); the live suite guards before it counts
      and prints what the guard dropped. `decide` unchanged. Original:
      nothing bounds the number of spans a
      `Reading` may carry. One live run answered a nine-word message
      with TWENTY, cycling Notification+Request+Other+Proposal five
      times; it did not recur on a second run. GROUNDING would not
      catch it — the repeated texts were real substrings of the
      message — so the check that would is DISTINCTNESS: no two spans
      covering the same stretch. Worth pairing with the grounding rule
      (every span's text must be in the message, which held 12/12
      across two runs) as decoder-side guards rather than prompt
      wording.
- [x] mail-loopback-tls — LANDED 2026-09-05. Needed `Tls.server`, the
      missing mirror of `Tls.client` for upgrading an already-accepted
      socket mid-protocol; with it, the client's whole STARTTLS path
      runs against a real handshake. Original entry follows.
- [x] mail-loopback-tls (original) — okay-mail's STARTTLS path is covered by the
      pure protocol tests and NOT over a real socket: the loopback leg
      runs in the clear, because a TLS server needs a certificate and
      a key and this module has no such fixture. okay-tls's own tests
      have one; borrowing that shape would close it.
- [ ] mail-consumer-adoption — the consumer who asked for okay-mail
      replaces `Identity.console` with it. Not my lane to do, but the
      one that tells whether the seam is right: their `deliver` is
      `(Channel, String, String) => Unit` and `Mail.Send` has to plug
      in without anything else changing, which was their stated
      requirement.
- [x] intent-window-by-dim — MEASURED 2026-09-07 (`TestWindowByDim`,
      specs Results): at 4096 the narrower windows keep every class
      clean on the composite (Other F1 0.52, on the floor; the shipped
      (3,5) @4096 has 0.64), and (2,3) @4096 is the best configuration
      on the table — 80.0% behind the cues against the shipped 75.0,
      71.7% under a typo against 63.3. Under the typo `Other` falls
      under the floor in EVERY configuration, the shipped one included
      (0.42): that is the class's fifteen rows (intent-other-more-rows),
      not the window. No default moved; the size-for-points decision
      is filed below. Original: the n-gram window against the per-class
      law at each hash width; a 2–4 window survives a typo at both
      widths and at the shipped width takes `Other` from recall 0.47 to
      0.33 while the total rises.
- [x] intent-shipped-model-4096 — the shipped model at (2,3) @4096:
      +5.0 points behind the cues (80.0 vs 75.0%), +8.4 under a typo
      (71.7 vs 63.3), `Other` F1 0.52 clean (the floor holds), at four
      times the artifact — `Fit` chose 1024 for a quarter of the size
      when the cost was two points. The owner's call: if taken, refit
      through `MakeModel` with `dim = 4096, low = 2, high = 3`, re-take
      every number `Models.scala`'s doc comment and `TestModels` /
      `TestSecondAuthor` pin (61.7% alone, 75.0%, the register-shift
      table, per language), and move `Fit.grams`' defaults with it so
      the artifact stays what the generator produces. TAKEN
      2026-09-07 (the operator's call): refitted, `Fit.grams` defaults
      4096 / 2–3, every pin re-taken — alone 61.7 → 68.3%, full
      coverage 75.0 → 80.0%, typo 63.3 → 71.7%, far half 66.7 → 70.0%,
      per class Proposal 0.85, Request 0.88, Notification 0.87, Other
      0.52 (recall 0.47 → 0.40: the bigger model does not buy `Other`;
      rows do). Found on the way: a class file caps a string constant
      at 64KB and the 171KB artifact would not compile — `MakeModel`
      emits it in 60KB pieces joined at load.
- [x] intent-typo-robustness — MEASURED 2026-09-07, default kept:
      per window at both widths (`TestTypoRobustness`, default gate),
      (3,5) 65.0 → 55.0% @4096 and 61.7 → 53.3 @1024 under one
      transposition; (2,4) 65.0 → 63.3 / 61.7 → 65.0; (2,3) 68.3 →
      66.7 / 58.3 → 66.7; the TF-IDF control 61.7 → 56.7. A smaller n
      buys the typo back — and the shipped model refitted at (2,4) or
      (2,3) loses `Other` (recall 0.47 → 0.33, F1 under the 0.50
      per-class floor) while its totals rise, so the default stays
      (3,5) and the reason is written on it. Filed
      intent-window-by-dim; the rows are intent-other-more-rows.
      Original: character n-grams are supposed to
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
- [x] intent-structured-output — every lane bought its answer's SHAPE
      by persuasion: a rendered example, written rules, field order.
      OpenAI-compatible gateways take `response_format` with a JSON
      schema, which makes the shape a property of DECODING rather than
      of asking nicely. Never tried once. If the gateway supports it,
      it may close the decode question outright and take accuracy with
      it — one experiment, not a research programme, and it is not a
      data problem.
      MEASURED 2026-09-07 (`TestStructuredOutput`, five arms over 120
      messages, `Meeting`): `OpenAi.request` gained `responseFormat`
      and `OpenAi.jsonSchema`. Where the persuasion already works the
      contract changes nothing — the same 0.909, the same replies — at
      36% more latency; where it is removed, zero of 360 replies
      decode, and they violate the schema itself (`why` missing, a
      string for a sum): the rozum gateway's `response_format` is a
      hint for flat objects, not a grammar for this shape. Also: the
      derived JSON schema renders a refinement (`Conf`) as a plain
      string, so the contract could not carry the vocabulary even if
      enforced — an `enum` for refinements in `JsonSchema` is filed.
      No default moves; the door stays for a gateway that enforces.
- [x] intent-multi-intent-measured — LANDED 2026-09-05. Twelve
      two-intent messages in the fixture; the shipped path answers
      with one label by construction (matched either intent 10/12, and
      the cue tier's runner-up carries the second 5/12 where the
      router discards it); the model tier returns two spans 6/12, the
      right pair 5/12, the right pair in order 4/12, every span
      grounded in the message 12/12. The claim is a property of ONE
      tier and now says so. Original entry follows.
- [x] intent-multi-intent-measured (original) — spans have been in the type since
      the first lane and were argued for as the thing a flat list
      cannot express, and the fixture contains NOT ONE message with two
      intents. The mechanism has never been exercised. Either measure
      it or stop claiming it.
- [x] intent-jmh-row — LANDED 2026-09-05. A JMH row per tier, plus
      the load cost nobody had ever timed. The quoted microseconds
      were 50-70x too high because a cold loop in a test measures the
      JIT: probe 76 -> 1.7, centroid 90 -> 1.3, chargrams 92 -> 13.7,
      the fit 404ms -> 40.1ms. Load of the shipped model: 58.9us.
      Original entry follows.
- [x] intent-jmh-row (original) — this line quotes microseconds everywhere (76us
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
- [x] frame-language-with-grammatical-gender — SETTLED 2026-09-07
      (frame-language-tag-fallback): the caller keys by the tag it
      owns (`pl-formal-f`) and `Slot.lookup` finds a wording along the
      tag — the tag, each shorter prefix at a `-`, the fallback;
      `question`, `show`, `options` and `speaks` all go through it, so
      `untranslated` is honest for a tag. The library models nothing
      about gender or register. Laws in `TestFrame`; docs/modules/
      okay-frame.md says the rule. Original: the migrating consumer
      raised it and could not test it: `Slot.ask` is keyed by a
      language CODE, and a language whose question differs by the
      grammatical gender of the ADDRESSEE (Polish Pan/Pani, and the
      formal registers around it) needs more than a code — or needs
      the caller to key by "pl-formal-f" and own the choice. Their
      four languages dodge it because Polish there addresses
      informally. Worth settling before the Map-keyed language is
      called done for all languages.
- [x] intent-temporal-multilingual — DONE 2026-09-07: `Temporal.
      Multilingual`, a lexicon per language (fr, de, es, ru, uk, pl;
      ja as a string scan) — weekday, month and relative-day words as
      token PREFIXES so inflections and German compounds match, the
      qualifier before or after the weekday, `N jours` / `vor N Tagen`
      / `через N дня`, the next-week pair, `15h` / `15 Uhr` / `15時`,
      `M月D日`. English is tried first and unchanged. The law
      (`TestTemporalMultilingual`): every dated row of the parallel
      fixture reads the same `When` in every language as in English;
      the relative and counted forms per language against one Friday.
      Original: (absorbs the older
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
- [ ] intent-extract-more-slots — people DONE 2026-09-07
      (intent-extract-people): `People.parse`/`find` — a count beside a
      people-word (`four people`, `six of us`, `a team of 5`, `vier
      Personen`, `dla czterech osób`, `4人用`), the Slavic collective
      numerals counting by themselves (`на четверых`, `на чотирьох`),
      1..1000, `None` otherwise; `Slots.people`; the fixture's
      book-room row counts four in all eight languages; number words
      shared with `Duration` through `Numbers`. Left here: named
      entities (who) and places — neither is a parser. Durations DONE
      2026-09-07
      (intent-extract-duration): `Duration.parse`/`find` (minutes; a
      number and a unit, `1h30`, `90m`, the spoken fractions, number
      words, `and a half`; total and deterministic like `Temporal`)
      and `Slots.duration`, asked in `when`'s six languages, showing
      `1h30`/`2h`/`45min` back. English phrases; the other languages'
      number-and-unit words are filed as
      intent-duration-multilingual. Amounts DONE 2026-09-07
      (intent-extract-amount): `Amount.parse`/`find` — a number
      beside a symbol, a code or an unambiguous currency name in
      the eight languages, separators told apart by the digits that
      follow, composed number words, the nearest number wins;
      `Amount(value, currency)` with an ISO code; `Slots.amount`.
      Still open here: named entities (who) and places — neither a
      parser. Original: only
      `when` and whole-message text have extractors. Named entities
      (who), durations, places and amounts are the obvious next ones,
      and each is a `Slot.extract` rather than a design.
- [x] intent-duration-multilingual — DONE 2026-09-07: `Duration.
      Multilingual`, a lexicon per language (fr, de, es, ru, uk, pl;
      ja as a string scan) — unit words as token prefixes, number
      words with their genders and the one-and-a-half words
      (`anderthalb`, `полтора`, `półtorej`), a ten and a unit as one
      number (`сорок пять`, `cuarenta y cinco`), the fraction phrases,
      `et demie` / `y media` after the hours and `с половиной` / `i
      pół` before them, `N時間半`. The fixture is in the suite — eight
      meanings, eight wordings each, dictionary facts — and the law is
      `Temporal`'s: one meaning, one value. Original: `Duration` reads
      English; the parallel fixture has no duration rows, so the law
      that settled `Temporal`'s seven languages has nothing to hold on
      to yet.
- [x] intent-taxon-wired-to-tiers — LANDED 2026-09-05. Every `Trained`
      carries the `Taxon` it was fitted against; `train` infers,
      `against(taxon, rows)` declares and refuses a label outside it,
      `silent` names what a declared taxonomy holds and the rows never
      taught. `NoModel.fit` refuses cues and a probe that speak
      different names, which was a live silent-degradation bug. The
      taxonomy is deliberately not persisted. Original entry follows.
- [x] intent-taxon-wired-to-tiers (original) — request 1 asked for one taxonomy
      both tiers read, and what landed is one taxonomy that NEITHER
      tier reads: `Classify` takes a `Schema[I]`, `Patterns` takes
      cues, `Centroid` takes whatever labels it was fitted on, and a
      caller checks `taxonomy.has` by hand afterwards. The value is
      right and the wiring is absent — `Taxon` should be what a tier is
      fitted or built against, so a mismatch is a compile or fit error
      rather than a silent disagreement.

## channel-per-part-waiters — DONE 2026-09-05

Senders wait per part now, and the buffer reports `lastRoute` so a
freed slot wakes a sender that can use it. 111546us to 485 at sixteen
producers, a 230x fix; the bounded relaxed channel is now 5.3x past a
single ring and 5.5x past `zio.Queue`, and scales the right way.


## channel-per-element-effect-cost — CLOSED as an interpreter lane 2026-09-06, redirected

Taken as free-cont-stack on the hypothesis this entry invites: that
the 26% in `runFree` and the 13% in Free allocation are the left-nested
re-association, `Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g))`,
which rebuilds a node and a closure every step, and that an explicit
continuation stack removes it.

**Counted before writing any of it, and the hypothesis is dead.** A
probe in `runFree`, this lane's own shape, N=4000:

| case | elementwise | chunk-native |
|---|---|---|
| `rotate` — the re-association | **64** | 4 |
| `Bind(Pure, f)` | 4001 | 17 |
| `Bind(Inject, f)` | 8020 | 36 |
| `Pure` | 2 | 2 |

The rotation is 64 steps out of 12085 — half a percent. The other
12021 take the two fast branches, which allocate nothing today. A
continuation stack would optimise 0.5% of the walk and add a cons cell
to the 99.5%. Not written.

What the counts DO show is the real structure — three interpreter
steps and two effect injections per element — and they explain the
chunked lane in one line: **59 steps against 12085** for the same 4000
elements, which is why it reads 19.66 against 209.3.

**Then the step count itself was measured, and it is not the lever
either.** One of the two injections per element is the CALLBACK:
`runForeach` takes `A => Unit ! Async`, so a plain side effect is
lifted through `Async.Run`. Walking the same source with a plain
function removes that injection, its `Bind`, and a third of the steps
(`PerElementStepBenchmark`, N=4000, quiet):

| lane | us/op |
|---|---|
| `elem_effectCallback` (today's surface) | 209.313 ±1.504 |
| `elem_plainCallback` | 199.621 ±3.308 |

**4.6%.** A third of the interpreter steps is worth five percent, so
the interpreter is not where the lane's time goes. (That lane is a
measurement, not a proposed API — `Source.runForeach`'s documentation
states the lifting rule deliberately. And it would not be a fair pair
against ZIO, whose `runForeach` callback is an effect too.)

**A fresh profile says where it does go.** The profile this entry
rests on predates `bufferChunked` and the linear-view feed. Re-taken
today (`-prof stack`, 1 fork, elementwise lane):

```
58.3% WAITING (86.1% of it Unsafe.park)   24.9% RUNNABLE   16.8% TIMED_WAITING (all park)
  within RUNNABLE:  go$4 3.9%   resume 3.7%   runFree 2.5%   receiveMany 0.8%   popMany 0.3%
```

`runFree` is **2.5% of wall time, not 26%**, and the whole effect
machinery is around 10%. Three quarters of the time the thread is
PARKED. Caveats stated: one fork, ten seconds, and JMH filtered half
the runnable frames — so read the ordering, not the decimals.

So the last gap on this lane is not the interpreter and not the queue.
It is the wakeup handshake, which already has an entry:
**`channel-elementwise-wakeups`** — one unpark per element on the
consumer's critical path. That is where this work goes next, and it
now has evidence rather than a hunch. The remaining interpreter idea
(make a per-element `Writer` step cheaper) is not refuted, but it is
priced: everything above says the ceiling is around a tenth of the
lane.

## the original entry, kept for the reasoning — WITHDRAWAL REVERSED, gap narrowed

Audited 2026-09-06. This entry withdrew the chunk-native read as
measured-and-worse (447.9 against elementwise 264.5) on the finding
that there was nothing to chunk: the producer delivered 1.67 elements
per `receiveMany`, because `Channel.buffer` fed the channel at one
`Free` step per element.

The entry then named its own fix — "give `Channel.buffer` a
chunk-native feed so the producer emits arrays" — and that fix LANDED
(9fe22fdc, `bufferChunked` + the linear-view feed). With the producer
able to run ahead, the withdrawn lane is the fastest one we have:

| consumer shape | producer | us/op |
|---|---|---|
| chunked read (`drained` over chunks) | `Channel.buffer`, per-element | 447.9 — withdrawn |
| chunked read (`drained` over chunks) | `bufferChunked(64, 256)` | **19.66 ±0.17** |
| elementwise (`okayChannelForeach_elem_runForeach`) | `Channel.buffer(1024)` | 209.3 ±3.5 (was 264.5) |
| `zioChannelForeach_chunk_runForeach` | — | 133.0 ±16.4 (was 139.1) |

Read those first two rows carefully: they are NOT one lane
re-measured. Same consumer shape, different producer — which is the
whole finding. A consumer-side batch is worthless while the producer
is the bottleneck and worth 23x once it is not, so the withdrawal was
correct about its own moment and wrong as a verdict. Generalise that
before withdrawing anything else measured behind a slow producer.
The elementwise row IS one lane re-measured, same harness, same name.

What is genuinely left: the ELEMENTWISE path at 209.3, where 62% is
still effect machinery (`runFree` 26%, `Async` handler 15%, `Free`
allocation 13%, `resume` 8%) against 8% in the channel. Making a
per-element `Writer` step cheaper is the remaining work, and it is a
Cont/interpreter lane, not a channel one.

NOT SETTLED HERE: whether 19.66-vs-133.0 is a fair pair at all — one
is our chunk-native surface, the other ZIO's chunked one. That is
precisely what `benchmark-fairness-audit` is measuring; take its
verdict, not this table's, before quoting a ratio anywhere. Original
entry follows.

Reading a buffered channel one element at a time: **264.5us against
`ZStream.fromQueue`'s 139.1**, the one row in the idiomatic table
where zio genuinely leads after five mismatched pairings were fixed.

Profiled (`okayChannelForeach_elem_runForeach`): **62% effect
machinery** — `runFree` 26%, the `Async` handler 15%, allocating
`Free` nodes 13%, `resume` 8% — against **8% in the channel itself**.
The queue is not the cost; the per-element program step is.

**A chunk-native read was tried and WITHDRAWN, measured.** The idea
was to batch the PROGRAM the way `popMany` batched the queue: a
`drainedChunks: Source[Chunk[A]]` whose tree steps once per batch. It
measured **447.9 against the elementwise 264.5** — worse, and the
reason is the batch size: on this path the producer delivers an
average of **1.67 elements** per `receiveMany` (max 64). There is
nothing to chunk. `Channel.buffer` feeds the channel through the
effect system at one `Free` step per element, so the consumer never
falls behind and the buffer never fills.

That is the same finding as `channel-send-fastpath`, one layer up:
batch size is set by how far the PRODUCER can run ahead, and here it
cannot, because its own per-element cost is the interpreter.

So the work, if it is taken: make a per-element `Writer` step cheaper,
or give `Channel.buffer` a chunk-native feed so the producer emits
arrays. The second is likely the smaller change — `Channel[Chunk[A]]`
already exists and `mergeChunked` already uses it — and it would make
a chunked read worth having, which it is not today.


## feed-staged-loop — measured and declined

After `feed-linear-view`, the obvious next step was to stage the feed:
loop with `offer` while the buffer has room and build a program only
where waiting happens, so the remaining `send` + `flatMap` per element
would go too.

Measured, it does not pay:

| lane | linear view | + staged loop |
|---|---|---|
| chunk-native | **19.66 ±0.17** | 20.47 ±0.54 |
| elementwise | **209.3 ±3.5** | 241.3 ±3.2 |

Neutral on the chunked path and 15% WORSE on the elementwise one, and
the reason retires the idea rather than inviting a second attempt.

On the chunked path there was nothing left to stage: `feedBatched`
already sends per CHUNK, so the whole run built sixteen node pairs,
not two per element. The per-element traffic had already been removed
by the linear view.

On the elementwise path the consumer is slow, so the buffer is
saturated nearly always — and then the loop costs a FAILED `offer` and
an `Option` per element and still builds the program to park. Strictly
more work than the plain `send` it replaced. The loop only pays while
there is room, and there is no room.

**Staging removes the building of a program; it cannot remove the
waiting.** What is left in the feed after the linear view IS the
parking, and that is the work the program exists to describe.

If anything revives this, it is the adaptive feed (see
`channel-per-element-effect-cost`): a producer that notices it is
saturated and stops trying to offer. That is a different lane, driven
by state rather than by shape.

## fs2-chunked-merge-lanes — the fs2 rows in §6 and ChunkFlush are the singleton spelling only

**CLOSED 2026-09-06** (fs2-chunked-merge-lanes): re-paired at N=500 and N=2000; fs2 chunk-native 84 / 109, slightly ahead of ZIO in §6b. See CHANGELOG.

Filed by benchmark-fairness-audit (2026-09-06). `ChunkFlushBenchmark`'s
`fs2Chunked` lane chunks AFTER the merge (`a.merge(b).chunkN(k)`), so
the merge itself still sees singletons; measured at N=2000, chunking
BEFORE it (`Stream.emits(range).chunkN(256).unchunks` a side) reads
281 against 35 700. §6's table is N=500 and was not re-paired: add
the chunked-before lane to `MergeBenchmark` and `ChunkFlushBenchmark`
at their own N and put the row beside the singleton one — do not scale
the N=2000 number. Same for `fs2.Stream.range` anywhere it feeds a
competitor lane: it is per-element by construction (3.10.2,
Stream.scala:3981), and `emits`/`chunkN` is the chunked spelling.

## close-the-gaps — refuted attempts, so nobody tries them blind

- `Chunks.elements` as a single cursor over the chunk walk (no
  `Iterator.flatMap`): 23.3 → 22.5, inside noise. Boxing through
  `Iterator[A]` is the per-element cost; the win is the chunk-native
  path, which exists. Reverted 2026-09-06.
- JVM `Fiber.joinEither` on `CompletableFuture.get()`: 19.6 → 22.3,
  worse in all three rounds — `get()` spins before parking. Reverted
  2026-09-06. If the join is ever revisited, the thing to try is a
  fast path that reads the future's completed value without
  registering a callback, NOT a different park.
- `runForeach` — DONE (runforeach-one-walk, 2026-09-06): 159.7 → 99.9,
  0.63x. The channel lanes did not move: their per-element cost is the
  Async operation `.drained` forwards per element, not the walk.

## actor-receive-offer-first — the mirror of feed-offer-first, on the loop that reads one message at a time

Measured 2026-09-06 (`actorTell`, §17): 4000 tells through an actor
read 295.9us against 198.5 through the bare mailbox shape, 1.49x, and
the JFR says why per message: `Slot` 142, `Right` 95, `Some` 89, the
`Platform` lambda 87, the `Await` anon 87. The actor loop calls
`receiveBlocking()` once per message — deliberately, so supervision
knows which message was the poisonous one — and each call runs the
full handshake even when the message is already there, which under
load it always is. `feed-offer-first` removed exactly this on the send
side (−38%). The receive side needs a synchronous poll the loop can
try first: a `receiveNow(): Option[A] | end` on `Channel`, answered
from the ring without a `Slot`, an `Await` or the `Right(Some(_))`
pair, falling back to `receiveBlocking()` only when empty. That also
finishes `channel-callback-allocation`'s receive half for this caller.
Laws: the one-message-at-a-time property must survive (a poll takes
ONE), close/fail/end must be seen through the poll, and the ordering
against parked receivers must hold. Expected: most of the 0.49x.

## actor-ask-timer — DONE 2026-09-06 (§17e): the wait as one operation, 4.4 KB → 1.5 KB per ask

The timer's thread went in `small-wins` (5.5 → 4.4 KB); the rest was
`race` itself — a fiber per side for a contest between two callbacks.
`Reply.await` is now one `Async.await` over `box.receiveAsync` and
`Timer.after`, first wins, other cancelled; `ask` needs no
`Scheduler`. `actorAsk` 2332 → 1557 us and 880 429 → 308 646 B/op
(−33% / −65%). Of the two shapes filed below, "arm the timer only if
the reply is already there" is moot for a sequential ask (the actor
has not run yet when the send returns) and the single-slot `Reply`
stays open at a few hundred bytes of upside. Original entry:

### as filed — 13 microseconds and 5.5 KB per ask

`Reply` is a `Channel[R](2)` and `await` is `Async.race(box.receive,
Async.sleep(within))`: a virtual-thread timer armed for every ask,
whether the answer takes a microsecond or never comes. Measured
(`actorAsk`, §17): 13.0us and 5.5 KB per round trip; the KB is the
timer's stack chunk. Two cheaper shapes, to measure not assume: arm
the timer only if the reply is not already in the box after the send
(the common case under a responsive actor), or a single-slot box
instead of a full channel. Matters only for ask-heavy callers.

## reactive-bridge-profile — DONE 2026-09-06: 5.67x → 2.63x, −51% time, −41% bytes (§17b)

Counted per side first: the reader made 4001 awaits for 4000 elements
(one handshake each, where `drained` takes 62), and the pump walked
the source through a memoising `toLazyList` — a cell, a `State$Cons`
and a thunk per element. Two fixes, each alone neutral or worse, and
together **312.9 → 152.5 us, 2 951 724 → 1 746 516 B/op**: the pump on
the linear view runs ahead, so the reader's chunks (up to 64 per
handshake, demand still following consumption) fill instead of
parking. The third withdrawal-reversal of the day and the rule it
leaves: measure a consumer-side batch only after the producer that
fills it is fast. Laws 49/49 (TestReactive + TCK) at every step. What
remains is representation: `Writer` per element, `uncons`'s three
objects on the pump, an atomic decrement of demand, boxing, the ring.

## as filed that morning — 5.67x and 738 bytes per element, unprofiled

The largest ratio measured on 2026-09-06 (`reactiveRound` vs
`plainSource`, §17): a round trip through `Flow.Publisher` and back
through `Flow.Subscriber` costs 312.9us and 2.95 MB against 55.2us and
0.86 MB. The shape has two channels of 256 and two half-window demand
batches per window. Before anything is changed: a JFR class breakdown
of `reactiveRound`, and a count per side of the awaits on each of the
two channels — the method that settled the channel lanes today. The
guess this entry refuses to make is which of the two channels, or the
demand accounting, is the 738 bytes.

## behavior-state-boxing — a primitive state boxes on every step

`Behavior[S, M] = (S, M) => S ! Async` over `S = Long` allocates a
`java.lang.Long` per message (141 of ~900 allocation samples on
`actorTell`). Not a library defect — a generic `S` cannot be
specialised without a second trait — but worth one line in the actor
docs, now written: give a behaviour an `AnyRef` state. Closed by the
doc line; kept here so the sample count has a home.

## actor-stop-strands — DONE 2026-09-07 (small-wins): a supervised Stop drains and discards, and `stopped` comes true

The loop now calls `discardRest()` after `fail` + `close` under
`Supervise.Stop` and `Escalate`: it reads the closed mailbox to its
end -- which is the failure, since the channel was failed -- and drops
what it reads. The messages were accepted and will never be handled;
that was already true, silently. Now `finished` means what it says,
`ActorRef.stopped` turns true, and the law in `TestPoisonLaws` asserts
it again (19/19 across the actor module). The `stop()` doc carries the
qualifier: a supervised stop drains and DISCARDS; a caller who needed
those messages handled wanted `Resume` or `Restart`. Original entry
follows.

## as filed — Supervise.Stop closes with accepted messages inside, and `stopped` is never true

Found 2026-09-06 writing a law for actor-receive-offer-first. On a
poisonous message under `Supervise.Stop` (and `Escalate`) the loop
does `mailbox.fail(e); mailbox.close(); running = false` — and stops
READING. Messages already accepted behind the poisonous one stay in
the mailbox, nobody drains them, and `ActorRef.stopped`, which is
`mailbox.finished` — "every accepted element handed over" — can never
become true. This is not the poll-first loop's doing: the old loop did
exactly the same. It is a standing semantics, and it is at odds with
the module's own note that `stop()` DRAINS under the strong contract.
Two honest shapes: drain-and-discard the rest before closing (so
`stopped` means what it says and the stranded messages are
acknowledged as dropped), or document that a supervised stop strands
and `stopped` is not the thing to wait on. Either way the actor docs'
"stop, DRAINING" line needs a qualifier. Not taken in the lane that
found it; the law there waits on the closed mailbox instead.

## actor-receive-offer-first — MEASURED AND DECLINED 2026-09-06: +19% in the regime that matters

Built: `receiveNow(): Poll[A]` on `Channel` (default `Empty`,
`SentinelChannel` answering from the ring, refusing while a receiver is
parked as `offer` refuses while a sender is), and the actor loop
polling before it parks. Eight laws held. The A/B against the old loop
in both regimes (§17a): **+19% time with an empty mailbox** (−14%
bytes), −17%/~0 time and −35% bytes with a full one. A failed poll is
a second read of the producer's cache line, ~17ns × 4000 = the 67us
lost; a responsive actor's mailbox is empty most of the time. Reverted
in full — no `receiveNow`, no `Poll`, the loop as before. Kept: the
`actorTellBacklog` lane, so both regimes are always measured, and the
poison laws (`TestPoisonLaws`), which held for the old loop and had
never been written.

## actor-receive-fused — the shape that wins in both regimes, not yet built

Why offer-first loses on receive when it won on send: a failed `offer`
reads the producer's OWN line (the ring tail it is about to write, no
contention); a failed poll reads the line the OTHER side writes. So
the try must not be a second scan. Fold it into `receiveAsync`'s first
scan — a variant that RETURNS `Got(a)` when its first pop hits and
only otherwise enqueues, rechecks and answers through the callback —
so a hit is one scan and one small object, and a miss is exactly the
old path. And the handshake a miss pays is a `Slot` plus a callback
lambda: make the callback BE the slot (one object with `value`,
`filled`, `waiter` and `apply`), which halves `CanBlock.block`'s
allocation for every caller, not only the actor. Expected: the full
regime's −35% bytes kept, the empty regime at parity or better.
Measure both with `actorTell` and `actorTellBacklog`; the A/B method
is in §17a.

## actor-receive-fused — DONE 2026-09-06 (§17c): the receive-side handshake, one object, no second scan

`Handoff[A]` in core — the callback that is its own slot — with
`CanBlock.handoff()`/`await(h)` on JVM (park/unpark) and Native
(monitor), `Channel.receiveInto(h): Boolean` (default: register and
answer false, correct before fast; `SentinelChannel`: the first scan
with an early return on a hit), and `receiveBlocking` rebuilt on them.
Old against new, both regimes: **bytes −5.2% (empty mailbox) and
−12.9% (full), time at parity in both** — the regression that sank
the poll-first loop does not occur, because the try is the scan the
handshake was going to do. Every `receiveBlocking` caller in the
library gets it. This also finishes `channel-callback-allocation`'s
receive half for the blocking form: the `Right(Some(_))` pair is gone
from the hit path, and the `Option[A]` return costs its one `Some`.
The `receive` PROGRAM (`Async.Await` + callback) is untouched and
still pays the pair; that is the remaining half, priced against the
`End` type as before. Laws: `TestHandoff` (7), `TestChannelLaws`,
`TestPoisonLaws`; Native compiles.

Process note kept here because it cost an hour: the A/B chain that
produced these numbers also WIPED the six uncommitted source edits —
a quoted `$FILES` list made its backup `cp` fail while the
`git show master:f > f` overwrite succeeded. The measurement was
valid (NEW ran before the swap); the tree was rebuilt from the
transcript and re-verified. Rule, now in memory: commit before any
script rewrites tracked files; restore with `git checkout`, never
`cp`.

## source-unfold-tuple — DECLINED by design 2026-09-07

`Source.unfold` costs 13% over `Source.range` on its lane (section 6c),
and the cost is the `Some((a, s2))` per step. That pair is what the
CALLER's `f: S => Option[(A, S)]` returns -- the `LazyList.unfold`
signature, which is the point of offering `unfold` at all. `range` is
faster only because it is not generic: it knows its step is `i + 1`
and tells `i` with no state to carry. Removing the tuple means a
second step type (`Step[S, A]` with `Emit(a, s)`/`Done`) that every
caller would have to learn for a 13% that only shows on a stream that
does nothing else. Not taken; a caller with a hot unfold writes the
specialised source, as `range` does.

## drain-copy-per-element — DECLINED by design 2026-09-07

`Drain` is a case class and `Stream[Drain, Async].uncons` answers
`Some((d.held(d.at), d.copy(at = d.at + 1)))`: three objects per
element -- `Some`, the pair, the advanced cursor -- 42 of ~900
allocation samples on the elementwise channel lane. The cursor is the
library's re-observation law: `uncons` on the same `Drain` twice must
answer the same element, so the advance MUST be a fresh value, and a
mutable index would make a `Drain` a linear resource in disguise. The
`Option[(A, S)]` pair is `Stream.uncons`'s contract for every stream.
Neither is a hole; both are the representation. Recorded so the next
profile does not re-file them.

## actor-ask-timer — DONE 2026-09-07 (small-wins, §17d): 5.5 → 4.4 KB per ask

The JVM `Timer` no longer starts a virtual thread per arm: one
scheduled executor holds the delay as a task, and the callback gets a
virtual thread only when it fires. A fast `ask` cancels before that,
so it allocates the task and nothing else — `actorAsk` 1 108 493 →
879 176 B/op, −20.7%; time 2594 → 2150 ±770 on a bursty box. What is
left per ask is the `Reply` channel (a ring of two), the send and the
race; a single-slot box instead of a channel is the remaining shape,
not taken here. All actor laws 19/19, including the timeout ones.

## drained-chunked-door — DONE 2026-09-07 (small-wins, §17d)

§6c recorded that `.drained.chunked()` re-chunks what `Drain` already
batched and reads 318.7 against 209 elementwise, and named it a
footgun. It cannot be forbidden by type — a `Source[A]` does not say
what it is made of — so the fix is the right door: `Channel#
drainedChunks: Source[Chunk[A]]`, each `receiveMany` batch told as one
chunk, nothing re-done. 79 469 B/op against 1 413 443 for the same
channel read one at a time, 17.8x less; the scaladoc names `.chunked()`
on a drained source as the wrong door.

## json-fast-read — DONE 2026-09-07: `Json.readStrict`, the second door

The operator's call ("let there be a choice"). `Json.read` stays the
lossless road — scanner, CST with every trivia token, projection,
fold — and keeps its three promises: byte-for-byte losslessness,
damage-as-data, a half-arrived document that still decodes.
`Json.readStrict` goes characters → `Schema` with no tree: the strict
recursive descent of `JsonValue.Parser` (an index, a slice for a plain
string, `parseDouble` on a number's slice) driving `Cbor.get`'s walk —
products by field name with the lossless decoder's own rules (unknown
fields ignored; absent → declared default → None-if-optional →
refusal), sums as the one-entry object `Json.encode` writes, options
as `null`, lists/vectors, iso, bytes as base64, chars as one-character
strings. It refuses — `Left` — anything it is not sure of, exactly as
`JsonValue.parse` answers `None`.

**The law, as a test:** `readStrict(write(a)) == read(write(a))` over
a corpus with every schema shape, whitespace everywhere the grammar
allows, unknown fields, defaults; and on a truncated or damaged
document the strict door is `Left` where the lossless one still
projects. 88/88 across the codec module.

**Measured (2 forks, bars tight):** `textToOrderStrict` **743.9 ±14.2
ns / 5 048 B/op** against circe's 813.1 ±69.6 / 3 416 and the lossless
road's 14 264 ±125 / 127 724 — 0.92x circe in time, 1.48x its bytes;
19.2x faster and 25.3x less allocation than `Json.read`. The strict
Schema walk costs about 3.3x the bare value parse (227 ns / 2.2 KB): the field map, the
erased parts and `make` are most of it, and a STAGED strict decoder —
the macro `Staged` already generates for a `Json` value and for CBOR
bytes — is the shape that would take it to circe's bytes. Filed as a
next lane if wanted, not taken here.

## bench-cross — DONE 2026-09-07 (§18): the same four shapes on JVM, JS and Native

`BenchCross` in src/test/scala-cross: a Live-tagged munit suite, four
shapes through `Async.runAsync`, thirty warmups, median of twenty and
minimum, platform from `java.vm.name`. Run per platform with the
build's `--exclude-tags=Live` REPLACED by an include (`set every Test /
testOptions := ...`), as the `integrationTest` alias does; an include
after `--` runs nothing. First numbers in §18 and the ledger. JS and
Native stable across two runs; the JVM column is a ruler against JMH,
not JMH.

## native-interpreter-allocation — DONE 2026-09-06: the collector is not it; the count is six objects per bind

§18: `bindChain`, N nested flatMaps with no channel, reads 497–517 us
on Native against 164–230 on JS and the JVM's warm ~190 — 2–3x, stable
across two runs. Nothing platform-specific is in that code path; the
difference is the allocator paying for the same `Free` nodes and
closures. Before anything is changed: a Native allocation profile of
that lane (Scala Native's GC has `-Dscalanative.gc.stats`-style
counters), and a check whether the interpreter's per-step objects can
be fewer on every platform — which `free-cont-stack` measured as not
the case for the re-association, but did not measure for the
closures. Native is also where `channelChunks` beats `channelElem` by
the most (2.1–4.6x): whatever is done here, the chunks door is the
Native reader's first move already.

**Measured (docs/benchmarks.md §18a).** The counter is immix's
`GC_STATS_FILE` (one row per collection), and `GC_INITIAL_HEAP_SIZE`
takes the collector out of the picture. Default heap: 503 / 498 us,
ten collections in the whole process, 11.4 ms of collector time.
Heap 2G: ZERO collections, and the lane reads 570 / 559 us — slower,
because every allocation now touches memory the process never wrote.
So the collector is not the cost; the mutator's allocation path is,
and it scales with objects allocated. The JVM reference for the same
program (`PerElementStepBenchmark.bind_runWith`, `-prof gc`) is 27.5 us
and 540,984 B/op: 135 bytes per bind, about six objects — `Inject`,
`Run`, the `() => i` thunk, `Bind`, the `x => go(...)` closure, and
by the byte count a boxed `Long` for `x`. Native pays roughly 21 ns
per object-and-step for those where the JVM's TLAB and escape analysis
pay near nothing. The lever is fewer objects per bind, on every
platform, filed as `free-bind-node-count`. Note also that BenchCross's
JVM column (189–483) is 5–12x the JMH figure for the same shape: 30
warmups of 4000 binds is not warm; read that column against JMH, as
its header says.

## free-bind-node-count — DONE 2026-09-06: the injection is 56% of the Native bind; a fourth node declined, a direct loop filed

`native-interpreter-allocation` counted 135 bytes and about six
objects per bind on the JVM, and showed that on Native that count IS
the cost (2G heap, zero collections, no faster). The candidates:
`Inject(Run(() => a))` is three objects for one operation — a Free
node that carries the thunk directly would be one; the `Bind` and its
closure are the program and stay; the boxed `Long` argument is the
generic `Function1` in `flatMap`. Measure on `bind_runWith` (JVM
bytes/op) and on Native's `bindChain` before and after each; the
Native number is the one that moves. Not a rewrite of `Free`: its
`fold` is the interpreter and every handler matches on three cases.

**Measured (§18b).** `pureChain` — the chain over `okay.pure(i)`,
no injection — reads 232.9 / 228.1 us on Native against `bindChain`'s
532.6 / 493.9 in the same process; under JMH 19.4 us / 380,984 B
against 27.6 / 540,984. The injection is 40 of 135 bytes and 30% of
the JVM bind but 56% of the Native one. The terminals differ:
`runWith` on `Free` is already a direct loop (`runFree`, Effects.scala
— `H.handle(e)` a plain call), so its 8 us is the objects and one
virtual call; `runAsync`, which BenchCross runs everywhere, is
`Drive.apply` re-entering `fold` per operation with a polymorphic
handler value, a closure per step and the answer back through `k` —
12.6 us on the JVM (`bind_runAsync` 40.2 vs `bind_runWith` 27.6).
A fourth `Free` case is declined by count: 118 sites outside
Free.scala match on the three cases directly. What is open is the
loop, not the node: `async-direct-loop`.

## async-direct-loop — DONE 2026-09-06: a quarter off the bind on Native (−26%), JS (−20% by minimum) and the JVM's `runAsync` (−27%); §18c

Landed: `Drive.apply` as a `while` over `Free`'s cases with the
operation dispatched to `op` (next program, or null when parked).
Native `bindChain` 534 → 396 us, JS 160 → 128 by minimum, JMH
`bind_runAsync` 40.2 → 29.2 us and one 16-byte closure per bind gone;
controls (`pureChain` Native, `bind_runWith`) unchanged. `runAsync`
is 2.3 us over `runWith` now; what remains of Native's injection cost
(163 of 396 us) is the three objects, the `op` call and `f()`, and
the next lever there is `free-bind-node-count`'s declined node — not
worth 118 match sites for it. Original entry:

### as filed

`free-bind-node-count` put a number on `runAsync`'s round-trip:
`Drive.apply` calls `fold` afresh for every operation with a
`[X] => F[X] => (X => Free) => Unit` value, `h(a)` builds the
`k => …` closure per step, the answer returns through `k`, and the
loop re-enters `fold` from the top — 12.6 us of `bind_runAsync`'s
40.2 on the JVM, and an unknown share of Native's 300 us of injection
(§18b), on a platform that inlines none of it. `runFree`
(Effects.scala) and `Stm`'s runner (Stm.scala:271, `case
Bind(Effect(e), k) => loop(k(perform(e, log)))`) are the precedent:
a direct match over `Free`'s cases with the effect's operations
inlined, no handler value. Write `Drive.apply` that way — the
left-nested rotation and the `Bind(Pure, f)` step as in `fold`, `Run`
inlined, `Await` keeping its exchange cell exactly — and measure
`bind_runAsync` (JMH, `-prof gc`) and `bindChain` on Native and JS
before and after; `bind_runWith` is the JVM ceiling and `pureChain`
the floor. Laws: every Async suite; cancellation at the next
operation and the callback-during-registration exchange do not move.
Not a change to `Free`, `fold`, `runFree`, or any other handler.

## raft-wire-election-flake — DONE 2026-09-07: Live-tagged, and made robust where it now runs

The operator's rule, stated again 2026-09-07: every flake moves to
the integration tests. `TestRaftWire` is tagged `Live` -- out of the
default gate, run by `sbt integrationTest` -- after failing three full
gates in two days under matrix load while passing 3/3 in isolation
every time. Two fixes went in with the tag, for the run it now has:
the cluster is built through a retrying door (`freePort` closes its
socket and the node binds later; the 2026-09-06 gate lost that race
to a BindException) that closes half-built nodes before trying again,
and the waits around the protocol are budgets a loaded box can meet
(election 15s, settle 6s, commit 15s; the nodes' own 20/200/50ms
timings are the law and are untouched). Verified: excluded from the
gate, and 5/5 under the integration flag with four CPU burners
running.

The same day, the same rule, a second suite: `TestSupervision` ("the
default is Stop -- a failure ends the actor") missed its 5s wait for
`stopped` once under a full matrix at load 8, after 2/2 other full
gates; 20/20 in isolation under four burners. Tagged `Live`, wait
budget 30s; `TestPoisonLaws` keeps `stopped`-after-poison in the gate.

Audit note on this section: its remaining `[ ]` items --
channel-impls, channel-impls-correctness, ring-channel-waiters,
channel-ring-unbounded, channel-multififo-many-producers -- are not
flakes, and the channel rewrite of 2026-09-05/06 answered three of
them under other names: the ring channel is `SentinelChannel` (the
default), the unbounded ring is `Segments`, many-producer FIFO is
`AdaptiveFifo` behind `Queues.relaxed`/`adaptive`. They are left in
place for a reader of this section's history; a later audit may close
them against those commits.

## json-strict-staged — DONE 2026-09-07: `Staged.strict[A]`, 2.45x circe, and a correction

The `Staged` macro's third target: `StrictJsonCodec[A]` from
`Staged.strict[A]`, generated over `JsonStrict.Reader` the way the
CBOR codec is generated over `Cbor.In` — primitives call the reader's
own `number`/`string`/`bool`, products go field by field into slots by
name with unknown fields SKIPPED (JSON's rule, not CBOR's refusal) and
absences filled as the fold fills them, sums by the one-entry object,
recursion and Mirror-less types falling back to the interpreted walk.
Laws: staged == interpreted strict == lossless over a corpus, plus the
refusals; 94/94 in the codec module, on three platforms (2485 in the
gate). `JsonStrict.Reader` became public for it: a package-private
member reached from a quote is an "unstable inline accessor" in the
caller's compilation unit — "access from wrong staging level".

**Measured (2 forks):** `textToOrderStrictStaged` **323.1 ±15.6 ns /
2 320 B/op** — 2.45x faster than circe (792.8 / 3 416), 32% less
allocation, 2.36x faster than the interpreted `readStrict` (764.1 /
4 968), and 112 bytes over the bare value parse (225.8 / 2 208): it
reads at the cost of scanning.

**The correction.** `textToOrderStaged` — `Json.parseValue` plus
`Staged.json[Order].decode` of the tree — reads 348.4 ns / 2 680 B in
the same run: 2.3x circe, and it has been in `CodecBenchmark` since
staged-codecs. It was never on the price list, and the list's
sentence "need raw speed? use circe" stood beside it. `json-fast-read`
measured its interpreted door at 0.92x circe and called that the
choice; the existing staged road was already better, and that lane
did not say so. The price list now carries all three doors and their
prices; the fastest is this one.
## adaptive-p-x-c-deadlock — DONE 2026-09-07: the woken senders were on the wrong part

Fourth face of the adaptive-buffer family (adversarial-lanes,
2026-09-07), after dense part indices, the three-state seal and the
snapshot `wakeAll` were in. `AdversarialBenchmark.manyToMany_okay`
4×4 under the ADAPTIVE default: warmup iteration 3 never returned;
the fork sat at 0 % CPU for 2 h 39 m with four consumers parked in
`receiveBlocking`, one producer parked in `sendBlocking`
(`blockAccepted`), the benchmark thread at `ps.foreach(_.join())`.
Dump kept by the lane (`adaptive-4x4-deadlock.jstack`). Not reproduced
by `TestManyToMany` (6 runs × 5 shapes × 4 buffers) nor by
`TestChannelLaws` × 6; it needs thousands of rounds. The shape to
suspect: a sender parked on a full part whose emptying pop woke a
different route's queue — `wakeSender()` wakes the senders of
`ring.lastRoute`, the part last POPPED, which is not the parked
sender's part when consumers rotate across parts. RESULT: the suspicion in the paragraph above was right, and the
mechanism is one line. `AdaptiveFifo.lastRoute` returned the SCAN
CURSOR, and the cursor is a single shared cell: with several consumers
rotating over the parts it names whatever part was scanned last by
anyone, so `wakeSender()` woke the senders of a part that had not
freed a slot, while the sender parked on the part that HAD stayed
asleep. Reproduced in three minutes by a probe at the JMH fork's own
shape (`ProbeAdaptivePxC`, four consumers and four producers, small
per-part capacity): round 5 363, four consumers parked in
`receiveBlocking`, one producer in `sendBlocking` — the JMH dump's
shape exactly. A thread's own last route replaces the cursor (every
`wakeSender()` runs on the thread that just popped, so it is exact),
and 20 000 rounds pass with no stall. Law in `TestManyToMany`: 400
rounds at capacity 4.

The probe's own lesson, worth keeping: `Thread.getAllStackTraces` does
NOT include virtual threads, so a watchdog built on it prints nothing
about a fiber deadlock. `jcmd <pid> Thread.dump_to_file -format=json`
does, and the fork to dump is `ForkMain`, not the sbt launcher.

## own-scheduler-jvm — DONE 2026-09-07 as `schedulers-family`: the scheduler exists, and it beats both policies

Measured (drive-scheduler-jvm, 2026-09-07, §4b): 10 000 small
fork/joins cost 187 ns per task on a raw `ForkJoinPool` (external
and internal push alike) and 79 ns on kyo's scheduler; okay's
`Schedulers.drive` sits 25 % over the pool, Loom 15 % over that.
Nothing in okay closes that from above the pool — fusing fiber, task
and promise into one object (`DriveTask`) bought 4 %. What would:
worker threads okay owns, a deque per worker, a parking policy that
does not signal a worker per submission (kyo: adaptive, one wake per
idle worker, batching of external pushes), `DriveTask` as the unit
scheduled. Price it as a programme: a scheduler is a scheduler.
Expected: fork/join 10k from 2310 toward ~1000; disqualifying: a
prototype with owned workers still above 1500 means the cost is in
`Drive`'s walk, not the pool.

RESULT (landed 9ede2549): `Schedulers.own` reads 750 us per 10 000
fork/joins at 30 ns a fiber and 3 645 at 2.5 us, against kyo's 880 and
27 097 — the prediction was met on the first column and the second
column turned out to matter more, because the two policies (keep the
burst home / spread it) are each right on one shape and each 7x wrong
on the other. The scheduler picks between them itself by measuring its
own last sixteen tasks; the two presets pin it either way. The builder
(`Schedulers.own.workers(4).forLongTasks.build`), the stuck-check that
makes a blocking fiber survivable (`Schedulers.adaptive`), `Running`
with `close()`, nine laws over six members and `docs/schedulers.md`
came with it. Everything found on the way was in the WAKING, not the
queues — see `own-deque` below for the three defects and the one
unmeasured piece.

## own-deque — DONE 2026-09-07, and the cause was the WAKE, not the queue

Measured (schedulers-family, 2026-09-07, §4b): `own` is the fastest
lane on real work when forked from outside (2 430 us against the JDK
pool's 3 729 and kyo's 25 379) and the best external lane at tiny work
(1 249 against 2 206), but forked from INSIDE a worker it reads 14 054
where the JDK pool reads 2 954. The helper rule is not the problem —
it activates workers and they do steal. The queue is: each worker owns
a `ConcurrentLinkedQueue`, so the owner and every thief contend on one
head, and at 2.5 us per task that contention is the ceiling (10 000
tasks x ~1 us of contended poll = the 14 ms measured).

The fix is the standard one and the reason `ForkJoinPool` does not
have this problem: a Chase-Lev deque per worker — the owner pushes and
pops at one end with plain writes and a fence, thieves take from the
other end with a CAS, so the common case has no contention at all.
`DriveTask` is already the unit; only the container changes.

RESULT: the deque landed and moved nothing — the disqualifying
evidence fired. Counters then named the real defect in one run: the
"active prefix" unparked only the worker at its edge, so workers that
had parked earlier never woke (ONE activation, two workers, 10 000
tasks). With the prefix gone (wake any sleeper, steal from everyone)
and the helper rule given its second clause (spread only when tasks
average more than `spreadAboveNanos`), `okayOwnInside` reads 744 us at
work=100 and 3 327 at work=10000 against kyo's 779 and 25 419 — 4.5 %
faster on kyo's own ground and 7.6x on real work, one tight run.

A third defect surfaced in the same lane: per-worker inboxes with a
random victim woke a sleeping worker for nearly every external
submission (1 249 -> 5 822 us). One shared submission queue, signalled
only when nobody is awake, fixed it (1 554 / 2 902).

Left open: the Chase-Lev deque's OWN contribution is unmeasured, since
it landed while the prefix defect masked everything. An A/B against a
per-worker CLQ would say; it is cheap and nobody needs the answer yet.

## adaptive-as-default — decided 2026-09-07: NO, and here is the number that would change it

`Queues.strong.adaptive` wins many-to-many (0.63 of our ring, 0.93 of
ZIO at 4x4) and is now correct under contention. It is still not
`Channel.apply`'s default, for one measured reason: at ONE producer it
costs about 15 % over a plain ring (144 against 122 in a tight run),
and one producer is what a channel usually has. Get that under ~5 %
and the default should flip; the obvious remaining suspects are the
interface call to the part (the ring is reached through `Buffer`,
where a plain `Channel` reaches a `Ring` directly and monomorphically)
and the `Chunks` path each side takes. A thread-local cache of the
producer's own buffer was tried and bought 18 % -> ~15 %, which is
inside the noise this box can measure and is recorded as no gain.

## adaptive-one-producer — CLOSED 2026-09-07 as the buffer's price, after five refuted causes

`Queues.strong.adaptive` beats the ring everywhere except at ONE
producer, where it reads about 1.2x a plain ring (measured repeatedly:
149/122, 147/126, 143/121). That gap is the whole of what keeps
`Channel.apply` on a ring, so it is worth naming properly.

REFUTED, each by its own measurement (solo-part, 2026-09-07):
- the part LOOKUP (`open` read, array read, bounds check): a
  thread-local holding the producer's own buffer, and then a `solo`
  field for the single-part case, together moved 149 -> 141 -> 143.
  Inside the noise of this box — and the `solo` field was REVERTED
  for a better reason than that: routing a push straight at part 0
  skips `claimPart`, so a second producer never gets a part and the
  buffer quietly stops adapting. `TestAdaptiveFifo` caught it in the
  gate. Whatever the fast path for one part turns out to be, it must
  still make every producer claim.
- the THREAD-LOCAL on the consumer side (`lastRoute` per pop): taken
  off the single-part path entirely. No measurable change.
- the extra LAYER OF CALL (channel -> Buffer -> Ring): a `Forwarding`
  buffer that does nothing but delegate reads 116.6 against the ring's
  121.2 — the layer is free, the JIT inlines it. This one was the
  disqualifying evidence written into the claim, and it fired.

WHAT THE EVIDENCE NOW SAYS: allocation. `-prof gc` at one producer,
same work: ring 780 KB/op, adaptive 1 446 KB/op — about 83 bytes per
element more — and the stack profile shows
`DirectMethodHandle$Holder.newInvokeSpecial` /
`Invokers$Holder.linkToTargetMethod` under `receiveManyAsync` in the
adaptive lane and nowhere else. Something on the chunked receive path
stops being inlined or scalar-replaced when the buffer is an
`AdaptiveFifo`, and starts allocating per element.

THE ALLOCATION LEAD WENT NOWHERE. JFR's `ObjectAllocationSample` on
both lanes names only the benchmark's own boxing (`Long.valueOf` in
the producer lambda) and the chunk array in `receiveManyAsync`, in the
same proportions; and JFR's own instrumentation moves the two lanes
past each other (adaptive 125 against the ring's 132 with the recorder
on), which is itself the finding: the gap lives in inlining, not in
allocation, and a profiler that changes inlining cannot see it.

TWO MORE REFUTATIONS, and this is where the box closes:
- the GROWTH MACHINERY: a partitioned buffer with ONE part that
  cannot grow (`onePart_chunk`, kept as a diagnostic lane) reads
  1.30x the ring — as much as the growable one. So the price is the
  layer's fixed per-element work, not the claim, the open count or
  the scan.
- the two THREAD-LOCAL lookups that fixed work consists of
  (`route()` per send, `lastRoute` per pop): replacing them for the
  first producer with a thread-identity compare against a volatile
  field made it WORSE — 146 -> 168, 1.49x the ring. A volatile read
  on this machine costs more than the `ThreadLocal.get` it replaced.

CLOSED with the price stated rather than paid down: `Queues.strong
.adaptive` costs 1.15-1.3x a plain ring at ONE producer and is 5.2x
and 24x faster at four and sixteen, and twice as fast with several
consumers. `Channel.apply` stays a ring; a channel that expects more
than one producer should ask for the buffer. Reopen only with a
cycles-level profile (async-profiler or perfasm, neither of which
this machine has), and read the five refutations above first.

## consumer-stash — the second half of the operator's MPMC design, and the contract that stands in its way

Stage 1 landed (`consumer-claim`): a part is drained under an
exclusive per-part claim released BEFORE anything is processed, and
each consumer starts its scan somewhere else instead of walking one
shared cursor. Four producers, four consumers, elementwise: 1 039 ->
893 us, and 0.39x a plain ring. Real, and smaller than predicted
(600), which says most of what a consumer pays is not the buffer's
head at all: an elementwise `receiveBlocking` takes ONE element per
call through the channel's waiter machinery, and a claim over a drain
never touches that path.

STAGE 2 is the operator's own words — consumers should not hold each
other up with what they took — and it means a per-consumer STASH: one
claimed drain refills a private batch that later elementwise receives
are served from without touching shared state.

The problem to solve first, and it is a contract, not a mechanism: a
stash is a place elements can die. `Queues.strong` promises that an
accepted element is delivered and that close drains what is buffered;
elements sitting in a consumer's private batch when that consumer
stops are neither delivered nor drainable. Three ways out, in the
order they should be tried:
  1. the stash IS a part — a consumer takes ownership of a part
     rather than copying out of it, so anything it leaves is still in
     the array for the next scan. No new place to die, and the claim
     already exists.
  2. a registry of stashes the channel drains on close, which is the
     honest version of a private batch and the most code.
  3. a stash bounded to one receive call, which is what
     `receiveMany` already is — and which the numbers say is not
     where the elementwise cost lives.

Do not build 2 before measuring 1: the difference between them is
whether a consumer may hold elements no one else can reach, and that
is the whole of the strong contract.

## jiffy-hole-scan — MEASURED and DECLINED 2026-09-07: a real latency hazard with no measurable throughput cost

The operator pointed at Jiffy (Adas & Friedman, arXiv 2010.14189v2),
a wait-free MPSC queue: buffers in a linked list, enqueue by one
fetch-and-add, two bits of state per entry, no atomic in dequeue, and
— the part that matters to us — a dequeue that SCANS FORWARD when the
head position is claimed but not yet published.

Reading it against our code corrected two things I had written down
before reading carefully:

- our unbounded buffer ALREADY enqueues Jiffy's way. `Segments.push`
  is `tail.getAndIncrement()` and a publish; `pushMany` claims a whole
  run with one `getAndAdd`. There is no CAS loop to remove.
- FAA for the BOUNDED ring is not an improvement waiting to happen,
  it is a mistake: `Ring.push` CASes the position precisely because a
  bounded claim can be REFUSED (the lap ahead may be unread), and an
  FAA that cannot be given back would claim a slot nothing can free.
  Vyukov's CAS is there for that reason; Jiffy can use FAA because it
  is unbounded.

WHAT IS REAL, and it is a latency defect rather than a throughput
idea. `Segments.pop` reads the head position, and if the stamp there
is not yet published it reports EMPTY:

```
else
  // either nothing has been claimed here, or a claim is in flight
  // and not yet published: both mean "nothing ready"
  empty = true
```

Those two cases are not the same. A producer descheduled between its
`getAndIncrement` and its publish leaves a HOLE, and every element
published after it — thousands, on a busy channel — is invisible to
the consumer, which parks. It is woken when the hole is filled, so
this is not a deadlock; it is a throughput cliff whose depth is how
long a producer stays off-CPU in that two-instruction window. On
virtual threads, which the runtime may unmount anywhere, that window
is not theoretical.

Jiffy's answer: scan forward for a set entry, take it, and rescan to
confirm nothing earlier became set meanwhile. Our per-producer
ordering survives it, which is the thing to check first and it checks
out: a producer publishes its previous element before it claims the
next, so if a later element of the same producer is visible, its
earlier one is too.

THE COST, stated honestly because it is why this is filed and not
done: `Segments` is MPMC, and taking an element PAST the head means
two consumers must not take the same one. That needs a per-slot
handled/taken state — Jiffy's two bits — with the head advancing
lazily over handled entries, which is a redesign of the consumer side
of `Segments`, not an edit. `popMany` in particular already carries a
scar from getting this wrong once (its comment records a consumer
killed by an exception nobody saw).

MEASURED, both halves:

- **The cliff is as sharp as the code says.** A probe hook held one
  hole open while 5 000 more elements were pushed and published: the
  consumer saw **0 of 5 001**, and all 5 001 the moment the hole
  filled. Nothing is lost — the strong contract holds — but everything
  behind a claimed-and-unpublished slot is invisible for as long as
  that producer is off-CPU.
- **It costs no throughput at the rates this runtime produces.** The
  unbounded buffer (the only one with holes) beats the bounded ring at
  every producer count: 131 against 142 at one, 472 against 803 at
  four, 673 against 1 485 at sixteen. The disqualifying evidence for
  the redesign was the unbounded lane trailing by more than 2x; the
  opposite happened.

DECLINED, and the reason is the shape of the risk rather than its
size: what a hole costs is TAIL LATENCY under a descheduled producer,
and a chunked-throughput benchmark cannot see that. The two-bit state
and the lazily advancing head are a redesign of an MPMC consumer that
already carries one scar; they are not worth it for a hazard that
does not show up in throughput. Reopen with a LATENCY measurement (a
percentile of receive-to-publish delay under many virtual-thread
producers), not with this one.

The probe hook itself was removed rather than kept: a mutable field
read on every push is not free — 1.15x at one producer and 1.39x at
four against master without it — so a diagnostic that costs the hot
path does not live in main code. Re-add it temporarily if the latency
measurement is ever built.

## queue-swap — CLOSED 2026-09-09, refuted by measurement

The entry proposed moving the ring -> partitioned swap out of the
`Growing` wrapper and into `SentinelChannel`, which would hold the
buffer in a volatile field and REPLACE it, so there would be exactly
one layer on each side of the swap instead of two. Six steps were
filed. **The plan is zero-sum and the lane did not build it.**

WHY. The channel lanes cannot price a 9% effect at one producer:
`oneRing_chunk` read 147.8 / 202.8 / 153.8 across three identical
rounds, and in one of them `forwarded_chunk` — a buffer that does
nothing but forward — read WORSE than `growing_chunk`, which is
impossible as a cost. So the lane built the instrument the question
needed: `BufferPushBenchmark`, one thread filling a 1 024 buffer and
draining it on a pre-boxed element, everything else common to every
row. Two independent runs of three rounds, bars under 1%:

| what it is | us per 1 024 push+pop | vs the ring |
|---|---|---|
| the ring | 9.036 | 1.000x |
| + a wrapper layer that only forwards | 9.057 | **1.002x** |
| + a `@volatile` buffer field, no trigger | 9.053 | **1.002x** |
| + the counting trigger (this is `growing`) | 9.913 | 1.097x |
| a partitioned buffer routing by thread from the first push | 10.058 | 1.113x |
| + an identity compare instead of the counter | 10.479 | 1.160x |

Read the first three rows together and the entry answers itself. Step
one turns `SentinelChannel.ring` from a `private val` into a
`@volatile private var` — row three, 1.002x. The thing that buys is
removing the wrapper — row two, 1.002x. **The plan deletes something
free and adds something free**, and the 9.7% it never touches stays
exactly where it was.

WHAT THE 9.7% IS, and why no rearrangement removes it: the price of
asking WHO IS PUSHING on every push. It is informational, not
structural. Three designs were measured and the shipped one is the
cheapest:

- the counting sample (shipped): a plain store to a padded counter
  every push, the thread compare behind an every-64th branch. 1.097x.
- routing by thread from the first push, which is what a lazily
  partitioned `AdaptiveFifo` does: 1.113x. This also prices what
  adopting the ring as part 0 buys — **1.5%**, not the 6.4x
  `docs/queues.md` claimed from parts measured before
  growing-part-sizing. That table is withdrawn.
- an identity compare instead of the counter (`GrowingCheap`):
  1.160x, **6% worse**. A volatile load plus `Thread.currentThread()`
  on every push lose to a plain store to a line this thread already
  owns.

The lane predicted, in its claim and before any of this ran, that the
gap would be ~8% and would be `sample()` rather than the layer. Half
right: it is `sample()`, and it is 9.7% rather than 8% — but the
first channel run said the opposite (layer 8.7%, trigger 4.6%) and
was believed for an hour. What settled it was building an instrument
whose bars were smaller than the effect, which is the lesson worth
keeping from this entry.

WHAT LANDED instead of the six steps:

- [x] `BufferPushBenchmark` — the instrument, kept, because the
      channel lanes demonstrably cannot resolve this class of
      question and the next person to ask will need it.
- [x] `GrowingCheap` and `GrowingNoSample` — the two diagnostic
      buffers, kept with their verdicts in their comments, so the
      shipped trigger reads as a measured choice rather than the
      first thing tried.
- [x] the corrections in `docs/queues.md` and `Growing`'s own class
      comment, both of which told the next reader that the layer was
      the problem.
- [x] the audit (`read-once`, 2026-09-07) stays landed and is good on
      its own terms: six methods now read the buffer once per
      operation.

WHAT IS STILL OPEN, honestly small: nothing in this design is known
to be improvable. Whoever wants the ring's 9.036 and knows there is
one producer can ask for it — `Queues.strong[A].fifo(capacity)` —
and that escape hatch already shipped. A cheaper trigger would have
to learn who is pushing without reading thread identity and without
writing a counter, and no such mechanism has been proposed. Reopen
this with one, not with a rearrangement of layers.

## resilience-timed-flake — a wall-clock assertion in the default gate

DONE 2026-09-09 (resilience-http): the second route. The mechanism was
the opposite of "too slow": under load the two calls were more than
20 ms apart, the REAL clock had refilled the bucket, and the second
call never parked — elapsed read 0. The limiter now takes a frozen
clock in that test, so the park is owed whatever the box is doing,
and `stats.delayed` is the assertion; the elapsed check is gone.

`okay.resilience.TestResilienceTimed`, "limiter: with a wait budget
the caller parks for the refill instead of being refused", asserts

    (System.nanoTime() - t0) / 1_000_000 >= 15

and FAILED in a full `sbt test` on 2026-09-09 while five sbt processes
from four sibling lanes were running. Measured immediately after, in
isolation, on the same tree: **three runs, three passes**. So it is
load-sensitive, not broken — and it took a green lane's gate red with
it, which is how it was found.

AGENTS.md is explicit about this class: "POLICY: no flaky tests in the
default gate — only in `integrationTest`. Anything whose outcome
depends on timing this repository cannot control ... a landing's gate
must not depend on external timing."

Owned by the `resilience` lane, which was active when this was found,
so it is filed rather than fixed. Two honest routes:

- move the timing assertions to `integrationTest`, which is what the
  policy prescribes; or
- keep them in the gate and make them robust — assert ORDER (the
  second call completed after the refill) rather than elapsed
  milliseconds, since the thing being tested is that the caller
  parked, not how long it parked for. `l.stats.delayed == 1` on the
  next line already says the former and does not depend on the clock.

The second is better if it can be had: it tests the actual claim.

## coordination-free — a clock, an identity, and state that merges

The operator's direction (2026-09-09): ULID/UUIDv7, CRDTs, and
"tokens or tickets". One direction — acting without a coordinator.
Spec: specs/coordination-free.md. Library first; Okay!Chat is the
application and gets its own entry once there is something to apply.

WHAT THE TREE HAS, grepped:
- ULID/UUIDv7: nothing. `UUID` is only a COLUMN TYPE (SqlType.Uuid,
  jdbc + r2dbc); `randomUUID` (v4, random) is called ad hoc in four
  places — McpHttp sessions, Smtp message-ids, ACME tests.
- CRDT: nothing. The one `lww` in the tree is a local function in
  okay-cache's TestView.
- okay-cluster is not a cluster: one `Acceptance` object for the
  cross-platform test.
- tokens: okay-security is real and central (Jwt, Es256, OAuth2,
  Oidc). No attenuation, no offline delegation.

- [x] 0 — the spec.
- [x] 1 — `Hlc` and `Uid` in core, cross-platform. LANDED
      2026-09-09: 15 laws on JVM/JS/Native, 2 threaded ones on JVM. A
      hybrid logical clock is what BOTH a monotonic id and an LWW
      register need, so it is built once. ULID and UUIDv7 are the
      same 128 bits — 48-bit millis then entropy — differing in six
      bits of version/variant and in spelling, so they are one type
      with two renderings.
- [x] 2 — `okay-crdt` LANDED 2026-09-09: `Crdt[A]` and its laws (commutative,
      associative, idempotent) as a reusable check, THEN the
      instances — GCounter, PNCounter, LwwRegister over `Hlc`, GSet,
      OrSet. A merge that is not idempotent makes the type a lie, so
      the laws land first.
- [x] 3 — the seam LANDED 2026-09-09 (`Crdt.folding`); Schema for the wire LANDED 2026-09-09 (crdt-wire): a CRDT is a fold, and okay-cache's `View` already
      takes one. Merge over okay-persist; `Schema` so a replica ships
      as data.
- [x] 4 — capability tokens. LANDED 2026-09-09.  DECIDED: the operator chose
      BOTH readings, capabilities first. Macaroon-shaped attenuation:
      each caveat signed with the previous signature as its key, so
      anyone can narrow a token and nobody can widen one, and a
      verifier needs neither a registry nor the issuer. This is what
      `okay-security` has no way to express — it signs and verifies,
      centrally, and a JWT cannot be narrowed by its holder.

## leases — CLOSED 2026-09-09: every part of it was already built

Filed as the second half of the operator's "tokens or tickets", to be
taken after capabilities. Taken, and there is nothing to build: the
whole design exists in okay-persist, and exists with better reasoning
than the entry proposed.

WHAT I CLAIMED WAS THE POINT, and where it already lives:

- **the fencing token** — `Replicated.Leader(partition, epoch)`, with
  an epoch-checked `append` that is "the only road records enter by",
  a refusal RECORDED as `Op.FencedAppend` rather than swallowed, and
  `promote` to advance it (catch the successor up, bump the epoch,
  switch the leader, record `Op.Promoted`).
- **the TTL lease** — `Election(control, node, leaseMillis = 5000,
  skewMillis = 1000, clock = ...)`. `heartbeat()` renews every
  partition this node holds; `vacant(partition)` permits a takeover
  only after `until` PLUS the declared skew allowance.
- **automatic takeover** — the election is a FOLD of a totally
  ordered control topic: "the first `Take` at an epoch wins that
  epoch on every node's fold — total order is the arbiter, so there
  are no votes, no terms of our own, no new wire messages". The
  winner calls the same `promote` an operator would, and the deposed
  leader is fenced.

AND IT SPLITS THE CONCERNS BETTER THAN THE ENTRY DID. Election.scala:
"Leases decide LIVENESS only ... Safety stays where stage 2 put it:
epochs fence, the high-water mark bounds visibility; if every clock
lies at once, the worst outcome is a fenced append and an ops event,
not forked history."

That is the exact distinction I had to correct MYSELF on earlier the
same day, in specs/coordination-free.md: I wrote that a capability's
`until` wants an `Hlc`, and it does not — a deadline is the
verifier's clock's business, and a logical clock orders events
without making one trustworthy. This entry made the same mistake
("`Hlc` is the obvious source" for the fencing token). The shipped
design uses neither: epochs from a total order for SAFETY, a wall
clock with a declared skew allowance for LIVENESS only. Clocks are
never trusted for correctness, which is why it is right.

MY PREDICTION IN THE CLAIM WAS WRONG, and in the useful direction. I
predicted "a narrow yes — a lease is worth having only as a way to
MOVE THE EPOCH automatically, i.e. as a policy over `promote`". That
policy is exactly what `Election` is, and it was written before I
proposed it.

The claim registered "no" as a permitted answer and named the reason
to refuse: "two fencing mechanisms is worse than one, and a lease
that grants its own token while `append` checks a different one is a
bug with two names". Building anything here would have done precisely
that. Second arc today to close as already-solved, after queue-swap.

- [ ] the only thing genuinely absent is NUMBERED QUEUES — a ticket
      per waiter, served in order — and the entry filed that as a
      question rather than work. It stays a question: nothing in the
      tree asks for one, and `Channel` already serves waiters in
      order within a partition.

## actor-stop-drain — FIXED 2026-09-09: stop waited by the clock and gave up

`okay.actor.TestChildren`, "law: a parent's stop waits for its
children to DRAIN", read 299 of 300 in a full 90-module run. Not a
race — a DECISION:

    val deadline = System.currentTimeMillis() + 5000
    while !mailbox.finished && System.currentTimeMillis() < deadline
      do Thread.`yield`()

`stopBlocking` waited up to five seconds for the drain and then
returned SILENTLY, so the caller believed the law one comment above
it — "drained, not merely closed … a parent waits for them" — had
held.

MEASURED with a repro that needs no load at all: a handler taking
20 ms over 300 messages is six seconds of honest work, one second
past the deadline.

| | before | after |
|---|---|---|
| handled | **219 of 300** | **300 of 300** |
| elapsed | 5.038 s — the deadline exactly | 6.87 s — the work exactly |

Eighty-one accepted messages dropped without a word. With a fast
handler the deadline almost sufficed, which is why it had only ever
appeared as "299 of 300, sometimes, under load" — and the busy spin
fed the failure: the busier the machine, the more of the budget the
waiting itself burned.

THE FIX: the actor already has an exact readiness signal — the moment
its own loop EXITS — and it was being thrown away, since `sch.fork`
discarded the handle. The loop now closes a `done` channel as it
leaves, and `stop` awaits that. No deadline, no spin, and no
blocking: `stop` composes with `flatMap` and `Async.await` instead.

The blocking shape was also unusable on JS, where `CanBlock` does not
exist — the compiler said so the moment the wait stopped being a
`Thread.yield` loop. Five seconds there could only be spent, never
used.

- [x] reproduce deliberately — done, and deterministically.
- [x] read `stop`'s drain against `Channel`'s close — the answer was
      simpler than the suspected sentinel race.
- [x] the law was RIGHT and the code was wrong.
- [x] CORRECTION (same day, before starting it): I filed "a JS test
      for `stop` would be worth having" and it is NOT ACHIEVABLE as
      filed. `Actor.spawn` requires `CanBlock`, and the `CanBlock`
      given exists only on JVM and Native — `src/main/scala-js` has
      none, deliberately ("There is no CanBlock on JS, so a blocking
      join is a compile error, not a frozen loop"). So no actor can be
      SPAWNED on JS at all: the module cross-builds and is unusable
      there. My commit message's "JS is exactly the platform the old
      code could never have worked on" is true and misleading —
      nothing works there, because there is nothing to work.

## actor-on-js — okay-actor cross-builds for a platform it cannot run on

Found while trying to write the JS test above (2026-09-09). `okay-actor`
is `crossProject(JVMPlatform, JSPlatform, NativePlatform)`, but its
loop reads with `mailbox.receiveBlocking()` and `Actor.spawn` asks for
`CanBlock`, which JS does not have. The JS artifact therefore compiles
and can never spawn an actor.

That is a real question, not a defect, and it has two honest answers:

- [ ] ~~Say so.~~ Not taken: the operator asked for the actors to
      WORK on JS, which is the second answer.
- [x] **Make the loop asynchronous** — DONE 2026-09-09. — `receiveAsync` and a drive
      rather than `receiveBlocking` — so an actor runs on the event
      loop, which is what the JS `Scheduler` already is ("the event
      loop IS the scheduler").

WHAT IT TOOK. The loop stopped being a `while` over blocking reads and
became a program: `mailbox.receive.flatMap` instead of
`receiveBlocking`, `Async.attempt(b(state, m))` instead of a
try/catch around `runWith`, and recursion through `flatMap` instead of
a mutable `var state`. Supervision carried over word for word —
the failed message is still gone and never retried, Resume keeps the
state, Restart takes a fresh one, Stop and Escalate fail the mailbox
and drain what nobody will read.

`CanBlock` then had no user left in the module, so both `spawn`s
dropped it — and with it `import okay.given`, which turned out to have
been there for `CanBlock` alone.

RESULT: 20 tests on the JVM, and **5 on JS and 5 on Native**, which is
the first time anything in this module has EXECUTED on those
platforms rather than merely compiled. The new laws live in
`src/test/scala`, shared, so if the loop ever goes back to blocking
the file stops compiling for JS — a better guard than a comment.

The `build.sbt` comment that explained why the laws were JVM-only
("the laws need a Scheduler to fork with, and that is platform work")
was true of the blocking loop and false now; corrected in the same
commit. A build comment promising what the code no longer does is the
same defect class as a test named for what it stopped checking.

## bulk-plan-warnings — DONE 2026-09-09 by `gate-warnings` (76478fa1)

Closed by a sibling lane that swept twelve warnings in four files —
a superset of the eight filed here — rather than by this entry.
VERIFIED before closing it: `clean` + `Test/compile` over the whole
build on master, zero `[warn]` lines. The entry stays for its
measurement, which is the argument for the rule.

`bulk-plan` / `bulk-rewrite` (origin, 2026-09-09) added eight compiler
warnings to a repository whose gate had none. Measured either side of
the merge: a full gate an hour earlier read **91 modules, 3 471 tests,
0 warnings**; the same gate with those three commits merged reads
**8**.

All eight are in the two files those commits add:

    okay-spark/src/test/scala/okay/spark/TestWroclawStages.scala
      39:48, 42:40, 45:32, 55:50   [E176] unused value
      58:35                        [E175]
      5:26, 8:19                   [E198] unused symbol
    src/test/scala-jvm/TestPlan.scala
      3:32                         [E198] unused symbol

Not a subtlety: `[E176] unused value` is what a discarded
`StringBuilder.append` or a discarded `intercept` produces, and the
fix is `val _ =`. Two lanes today (`coordination-free`, `capability`)
hit exactly these and fixed them before landing, because the gate is
kept clean here — `okay-gate-checks-warnings` says a warm gate that
hides warnings is not a gate.

Owned by whoever holds the bulk-plan lane; filed rather than fixed
because that lane was active. It is small — eight lines — and it
matters because a gate with eight warnings in it stops being a signal:
the ninth arrives unnoticed.

## gate-honesty — the gate's false failures, measured

The full gate cried wolf four times on 2026-09-09, costing four
separate investigations in one session. Every one measured:

| suite | in the gate | alone |
|---|---|---|
| `okay.kafka.TestKafkaInterop` | 237 s, timeout | 0.084 s |
| `okay.intent.TestCutStops` | 39 s, timeout | 0.059 s |
| `okay.persist.TestStable` | `BindException` | 0.007 s |
| `okay.ui` TestHybrid / TestSwing | 2 comparison failures + 2 timeouts | 84/84 |

**TWO ROWS OF THAT TABLE NAME THE WRONG SUITE** (gate-starved-suites,
2026-09-09 evening). The 237 s was `okay.demo.TestRepoAgent`'s, not
kafka's — a sibling lane had already measured it that morning and said
so in its own commit: "TestRepoAgent indexes the whole repository and
ran 203-237 s under four sbts against its 120 s budget, green alone —
tagged Live" (479c510b, gate-hygiene, 12:19). The same commit fixed
the intent row's real owners, `TestOfflineGate` and
`TestTypoRobustness` ("4 s alone, 48-61 s under load", given a
measured 180 s deadline). The kafka attribution is certain, the
number matches exactly; the intent one is likely rather than proven.

The irony is the entry's own lesson: a gate log read under pressure
was turned into a table of conclusions, and half of it named the
wrong thing.

The tax is that every lane pays isolated re-runs to tell a real
failure from noise. The real cost runs the other way: this session
nearly dismissed a GENUINE defect (`actor-stop-drain`, 219 of 300)
as load, by analogy with these.

**The okay.ui pair were NOT flakes, and that is the finding.** Both
came from `Thread.sleep(50)` used as a synchronisation point:

- `TestHybrid.client` slept 50 ms to let the server's tree land
  before feeding user events. Under a loaded gate that is not
  enough, so the first keystroke arrived before the client knew
  "name" belonged to a Form — and crossed the wire as an ordinary
  `Edited`, failing the ZERO-times law with one EXTRA event.
- `TestSwing` slept 50 ms before asserting on asynchronously
  collected events.

Fixed by waiting for the thing each sleep was waiting FOR — a render
count and an event count — bounded at 10 s and LOUD on expiry. A
test that hangs and then names what it waited for is debuggable; one
that sleeps 50 ms and fails an unrelated assertion is not.

**HOW IT WAS PROVED, including the attempt that failed.** A load
harness (28 spinners on 14 cores) did NOT reproduce it: the old
version passed under it, so a green run of the fixed version proved
nothing. What worked was isolating the variable instead of recreating
the conditions — setting the sleep to ZERO, which failed with the
same shape and more of it: one extra event in the gate, three at zero
wait. Dose-dependent, same defect.

When the CONDITIONS cannot be reproduced, reproduce the MECHANISM.

### Closed 2026-09-09 (gate-starved-suites)

- [x] the three timeouts are NOT starved, and were never slow. A full
      gate under 28 CPU burners on a box already at load 20 — load
      average 101, three times the "four sbt processes" the original
      reading came from — is GREEN at 3596 results, with
      `TestKafkaInterop` at 0.029 s, `TestCutStops` at 0.029 s and
      TestSwing's law at 1.658 s. The genuinely slow tests under that
      load are compute-bound and well inside their budgets (53.2 s
      "stack safety: 1M produced values", 37.5 s "does the gain
      survive resampling", 20.3 s "a typo's cost per tier"), and none
      of the four is near them. The three real load-sensitive suites
      were found and fixed by gate-hygiene (479c510b) the same
      morning. Nothing to widen, nothing to move: the table was
      wrong, and it is corrected above.
- [x] `okay.persist.TestStable` no longer pre-binds. It asked the OS
      for a free port, CLOSED it, and handed the number to
      `RaftWire.Node` to bind again — a real TOCTOU by inspection,
      and one this repo has met before: `TestRaftWire` and
      `TestRaftStore` both carry a documented three-try retry for it
      ("the BindException that failed a gate on 2026-09-06"). Those
      two must pre-bind, since their nodes are told each other's
      ports before any starts. `TestStable` has no peers and never
      reads its own port, so it takes port 0 and lets the OS assign
      at bind time: no window at all.

      THE COLLISION DID NOT REPRODUCE, in six models, and that is
      recorded rather than buried — neighbours grabbing ephemeral
      ports (0 in 20 000), eight threads running the idiom at once (0
      in 32 000), 717 615 ports churned alongside (0 in 30 000), a
      0-500 ms gap between the close and the bind with 11.2 M ports
      churned during the widest (0 in 300 each), neighbours that HOLD
      what they take (0 in 300 each), and four separate JVMs racing
      it (0 in 1 600). macOS does not appear to re-offer a
      just-released ephemeral port while its cursor is near it,
      within a process or across four. So the window is real in the
      code and very hard to hit on this OS — which fits a failure
      seen twice in months. The change is "the step that buys nothing
      is gone", not "the flake is fixed"; and since the original gate
      log is gone and the 237 s above was misattributed, whether that
      BindException was even this suite's is worth doubting.

## merge-chunked-order — DONE (24a1cc38): the buffer, not the merge

Found by gate-honesty's own gate (2026-09-09) as `okay.TestStream`
returning source `a` as `1..16, 49, 50, 17..48`, and filed with two
readings of the contract. The first was right: per-source order IS
the law. It is stated now — in `Source.merge`, in `Channel`'s
growing-default note, in `Growing`, and in docs/queues.md — and the
test asserted no more than it should.

The defect was NOT in the chunked path, which is only where it was
caught. `Channel.apply`'s default buffer since 2026-09-08 is
`growing`: a ring that, when a second producer appears, becomes an
`AdaptiveFifo` adopting that ring as part 0. Every producer was then
handed a part of its own while its earlier elements were still in
part 0, and parts drain independently. Reproduced with no merge, no
chunks and no streams — two plain threads into `Channel[Int](4)`, 73
rounds in 300 — so the fibres-migrating guess was wrong. The plain
elementwise merge was breaking 21 times in 500 at its own default
capacity, with nothing to say so.

Fixed by reading the adopted part FIRST while it holds anything, and
by making it nobody's home. Zero in 2000 rounds afterwards, on every
probe. Costs nothing this benchmark can see (`ManyProducersBenchmark`,
control-validated rounds only).

The two gaps that let it ship are closed with it: `growing` — the
DEFAULT — had never been added to `TestChannelLaws`' list of
mechanisms, and that suite's order law used ONE producer, the single
width at which this buffer never partitions at all.

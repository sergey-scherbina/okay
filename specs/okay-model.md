# okay-model: what okay needs for the Model Decision Runtime

Status: plan, written before code · 2026-09-26 · the same file lives in
okay (`specs/okay-model.md`) and in okay-watch (`specs/okay-model.md`).

## Overview

okay-watch's `specs/mdr.md` (the Model Decision Runtime, "MDR") asks for
an analyst's R or Python model run UNCHANGED in production pipelines, with
every decision recorded so it can be found, explained and reproduced
bit-for-bit. Its §14 draws a licensing seam: the RUNTIME is open, the
regulated extras (journal, registry, monitoring, review, reports, console)
are candidate-commercial, and no open module imports a commercial one.

This spec is the OPEN half, which belongs in okay: what okay already has
that MDR stands on, what okay-watch built first that moves here, and what
okay must still gain — module by module, with behaviour boxes, stages and
the measurements each stage owes. The commercial half stays in okay-watch
(`specs/mdr.md` §4, §14) and plugs into the hooks this spec defines.

The rule for what goes where: a MECHANISM any okay user could want (a
worker pool of R processes, an Arrow chunk with exact types, a
hash-chained log, a forked pool sharing a model's memory) is okay's; a
POLICY of the regulated product (what a decision record must hold, how
long an input is kept, who may approve a model) is okay-watch's.

## What okay already has (2026-09-26)

| need (MDR §) | where it is in okay | state |
| --- | --- | --- |
| R and Python as handlers behind one wire (§6.1) | okay-foreign (`ForeignWorker`, `ForeignEval`, the shims), okay-r (`RSubprocess.worker`) | built; one wire for Python, R, TS, Go, Rust, Haskell |
| a chunk as ONE Arrow table, exact types (§6.4) | okay-arrow (`Column`, `OkayArrow`), `ForeignWorker.frameTable(…, exact = true)` | built; dictionaries since `arrow-dictionary` (stage 8, 2026-09-26) |
| held objects (the model loaded once per worker) | `ForeignEval.Call(…, held = true)`, `PyValue.Ref` | built |
| inline modules shipped with the host | `Foreign.module`, `R.module` | built |
| a partitioned batch engine with retries and resume | okay-cluster (`Job`, `Flow`, `Cluster.run`), okay-foreign-cluster (map stages in R/Python) | built |
| append-only durable logs, compaction | okay-persist (`Store`, `Topic`, `FileStore`, `compact`) | built |
| Kafka in and out | okay-kafka (`KafkaInterop`, `KafkaStore`) | built |
| Spark interop | okay-spark (`SparkBulk`, `SparkSchema`) | built |
| plugins with versioned ports | okay-kernel | built |
| secrets as references | okay-conf (`Secret`, `Secrets`) | built |
| metrics | okay-ops, okay-obs | built |
| durable workflows (for human review) | okay-workflow | built |
| an HTTP server and router | okay-http, okay-jetty | built |

## What okay-watch built first, and moves here

okay-watch's `scoring/` (2026-09-25/26) proved the runtime end to end —
R and Python pools, the contract, Arrow at the boundary, the forked pool,
shadow, three batch paths, the benchmarks (okay-watch `bench/scoring.md`).
Its OPEN parts move into okay as below; what stays in okay-watch is the
journal's decision schema, retention policy, queries, backtest, report and
the screen.

| okay-watch today | moves to | as |
| --- | --- | --- |
| `Contract`, `Kind`, `Rejected`, `Cell`, `Incoming` | okay-model | the contract and its typed refusals |
| `ArrowTables` (a chunk as a table, levels as a dictionary; a table as cells) | okay-model | `ContractTables` |
| `Model`, `Model.Entry`, `Language` | okay-model | the package's manifest (§5 of MDR), superseding `<model>.json` |
| `Engine`, `ArrowEngine`, `RWorker`, `PyWorker`, `Pool`, `Failure` | okay-model-runtime | the pool and its two backends |
| `ForkServer`, `zygote.py` | okay-foreign | `ForkedWorkers`, generic (not model-specific) |
| `Pipeline` (source → pool → sinks, shadow) | okay-model-runtime | `Deployment` (routing and sinks as hooks) |
| `Source` (recorded file, Kafka) | okay-model-runtime | sources over okay-kafka and files |
| `Batch.lines`, `ClusterScoring` | okay-model-runtime | the batch path on okay-cluster |
| `SparkScoring` | okay-model-spark | the batch path on Spark |
| `Journal`'s chain and `Verify`, `Sealer` | okay-persist | `Chain`, `Sealed` — the MECHANISMS only |
| `Journal`'s decision records, retention, queries, `Backtest`, `Report`, `ScoringTool` | stay in okay-watch | `mdr-journal`, `mdr-console` |

## Modules

### okay-model (open, JVM; Scala.js where it costs nothing)

The package, the contract and the ONE scoring API — no processes, no I/O
beyond reading a package directory.

- **Package** (MDR §5.1): a directory with `manifest.json`, `contract.json`,
  `model/`, `code/`, `env/`, `reference/`, `tests/`. `Package.load(dir)`
  reads and VERIFIES it: every artifact's sha256 against the manifest, the
  env lock's hash, the manifest's own fields; a failure is a `Refused` naming
  the file and both hashes. A pickle or joblib artifact that does not verify
  is never handed to a runtime — the check is here, before any process.
- **Manifest** (MDR §5.2): `id`, `version` (semver), `language` (`r` |
  `python` | `jvm`), `entrypoints` (`predict`, optional `preprocess`,
  `explain`), `artifactHashes`, `envLockHash`, `runtimeVersion`,
  `libraryVersions`, `owner`, `purpose`, `riskClass`, `createdAt`,
  `createdBy`. `validationStatus` is NOT in the file — it is the registry's
  (okay-watch), and a manifest that carries one is refused, so a package
  cannot approve itself.
- **Contract** (MDR §5.3): a `Schema` pair, serialised to `contract.json`
  and read back to the same `Schema`. Field types: bool, int32, int64,
  float64, decimal(p, s), string, date, timestamp(tz), categorical(levels,
  ordered, unknown = error | other(level)), list and struct (flat, limited).
  Missing states are per field and DISTINCT: nullable; for R NA / NULL /
  NaN, for Python None / NaN / pd.NA / NaT — each a `Cell.Missing(kind)`,
  none coerced into another. The output contract: score(s), an optional
  decision label, an explanation shape (reason codes, contributions).
- **Typed refusals**: `MissingField`, `MissingValue`, `TypeMismatch`,
  `UnknownLevel`, `NoScore`, `DtypeDrift`, `ModelError`, `WorkerFailed`,
  `Timeout` — each with field, value, message; the dead letter's vocabulary.
- **ContractTables**: a chunk of contract rows as ONE `okay.arrow.Table` and
  back — categorical as a DICTIONARY with the contract's (or the model's)
  levels in their order, int32 never widened, decimal as decimal128, date as
  date32, timestamp with its zone; the answer table read as cells.
- **The scoring API** (MDR §6.1): `score(chunk: Chunk[In]): Chunk[Decision[Out]]
  ! Score` — an okay OPERATION, so the language is a HANDLER (R, Python,
  JVM, a canned table for tests) and policies wrap it as handlers too
  (§ Hooks). `scoreOne` is built on a chunk of one. `Decision` holds the
  output, the refusal if any, the explanation, and what produced it (package
  id, version, artifact hashes, env hash, role).
- **Golden tests** (MDR §5.1 `tests/`, §5.4 `check`): `Golden.check(package,
  handler)` scores every golden input and compares at the package's
  tolerance (0 by default) — and scores twice, refusing a package whose
  output is not deterministic.

Behaviour:
- [ ] a package verifies: a changed artifact byte, a changed env lock, a
      manifest naming a file that is not there — each refused by name
- [ ] `contract.json` round-trips every field type and missing state to
      the same `Schema`
- [ ] `ContractTables` round-trips every contract type and missing state
      through OkayArrow exactly (property test, seeded)
- [ ] a categorical's unknown level: `error` refuses, `other(x)` maps
- [ ] the scoring API over a canned handler: chunked, one call per chunk
- [ ] golden check: a mismatch and a non-deterministic model each refused

### okay-model-runtime (open, JVM)

The processes, the chunks, the routing and the glue — over okay-foreign,
okay-kafka, okay-http, okay-cluster.

- **Backends** (handlers of the scoring operation): R over okay-r's worker,
  Python over okay-foreign's, a JVM backend (a Scala function; PMML/ONNX
  later — MDR §19). Each loads the model ONCE per worker (a held value),
  checks the package's `libraryVersions` and `runtimeVersion` at load
  (refused unless the deployment allows drift), reads the model's own
  levels, reports its environment's hash.
- **Pool**: N worker processes, a bounded in-flight window (backpressure),
  a dead worker replaced and its chunk retried on another, N times, then
  `WorkerFailed`; a per-chunk deadline owned by the pool (not the wire), a
  timed-out chunk retried then dead-lettered. One supervisor: the pool —
  okay-r's own healing is switched off here so a restart is visible
  (counted, a new pid).
- **Forked pools** (MDR §6.2, Linux): the model loaded ONCE in a parent and
  the pool forked from it, copy-on-write — through okay-foreign's
  `ForkedWorkers` (below). A dead worker is a fork, not a load.
- **Chunks and latency mode** (MDR §6.3): the source's records gathered
  into chunks of at most `chunk` rows or `maxWait` milliseconds, whichever
  comes first (okay-stream's `Chunks.within`, below), so low traffic is not
  held back.
- **Routing** (MDR §6.5): per deployment, `champion` (returned),
  `shadow`/`challenger` (scored, recorded, never returned), `canary` (x% of
  business keys get the candidate as the returned decision). The canary
  share is decided by a STABLE hash of the business key (not a random draw),
  so a replay routes identically.
- **Sources and sinks** (MDR §6.6): Kafka — a record's identity is
  (deployment, topic, partition, offset); offsets committed only after the
  decision sink says durable; order kept per partition; a re-processing after
  a crash offers the same identities, so an idempotent sink drops the
  duplicate. HTTP — a synchronous endpoint (one record, latency mode) and a
  batch endpoint (a chunk), over okay-http. Files — Parquet (okay-parquet)
  and CSV in, a decision file out. Batch — okay-cluster's `Job` with pools
  RESIDENT per worker JVM. Dead letters — the typed refusal and the original
  payload, to a topic or a file.
- **Hooks** — the licensing seam as code: `DecisionSink` (where decisions
  go: the commercial journal plugs in here; the open default is a file or a
  topic), `Admission` (before scoring: limits, a review hold), `Explainer`
  (after scoring). Ports on okay-kernel, so a build is the set of plugins
  it was assembled with; nothing in the runtime names a commercial module.
- **Metrics** (MDR §9, the open part): throughput, p50/p99, refusals by
  kind, worker restarts, in-flight, over okay-ops.

Behaviour:
- [ ] R and Python pools score the same recorded stream; the JVM backend too
- [ ] a killed worker: its chunk rescored once, nothing lost, nothing twice
- [ ] a timed-out chunk: retried, then dead-lettered by name
- [ ] never more than N chunks in flight; a fast source waits
- [ ] latency mode: a lone record scored within `maxWait`
- [ ] routing: champion/shadow/canary deterministic by business key; a
      replay routes the same
- [ ] Kafka kill -9 of a worker and of the runtime mid-run: no loss, no
      duplicate in an idempotent sink (MDR §15)
- [ ] HTTP: one record and a chunk; p99 measured in latency mode
- [ ] Parquet in, decisions out; okay-cluster batch with resident pools

### okay-model-spark (open, JVM; its own module because Spark is 300 MB)

The batch path on Spark — the SAME scoring code per partition (a pool per
task, the model loaded once per task), outcomes handed to the driver's
`DecisionSink`. Behaviour: the recorded records through local Spark give
the stream's answers.

### okay-model-chain (open, JVM) — for okay-watch's `specs/mdr-crypto.md`

okay-chain's follower as an MDR source, and what a chain adds to a
decision.

- **The source**: `Follower.step()`'s `Confirmed(block)` becomes records
  for the runtime, `Rewound(from, to)` a ROLLBACK event the runtime hands to
  the `DecisionSink` hook (the commercial journal appends `orphaned`
  records; the open default writes the event to the dead-letter/file sink).
- **The chain position**, a value of every decision from chain data:
  network (CAIP-2, okay-chain's `Network`), block height, block hash, tx
  hash, output or log index, confirmation depth at decision time, finality
  status (`provisional` | `final` | `orphaned`).
- **Finality per deployment and chain**: `depth(n)` or `declared` (okay-chain's
  `Chain.finalized`, where the chain declares one — EVM, Tron, Solana; a
  refusal on Bitcoin and Cardano). A decision below finality is
  `provisional`; the source emits a `finalised(position)` event when the
  block reaches it, so a sink can move the decision to `final`.
- **Act on provisional or on final only**: the deployment chooses; acting
  on provisional is fast and may be corrected.
- **Re-appearance**: after a rollback, a transaction seen again in the new
  canonical chain is offered again with its new position and a link to the
  orphaned one.

Behaviour:
- [ ] on recorded blocks with a simulated rollback of depth 1..k: every
      decision in the rolled-back range gets a rollback event, a reappeared
      transaction is re-offered once, nothing is left `provisional` forever
- [ ] `declared` finality refused by name on a chain that declares none
- [ ] the chain position round-trips through the contract (CAIP-2, hashes)

okay-chain changes: none required for the source — `Rewound` and
`finalized` exist. One addition: a `Position` value (network, height, hash,
tx, index) with its CAIP-10/-19 forms, so the runtime and the journal name a
place on a chain the same way.

### okay-model-cli (open)

`mdr package check | publish`, `mdr deploy`, `mdr replay`, `mdr bench` —
the open commands; the journal's (`journal query | verify | export`,
`backtest`, `report`) are registered by okay-watch's plugins on the CLI's
command port, so the open CLI names none of them.

### Analyst packages (open; R and Python, under `analyst/` in okay)

- R `okaymodel`: `package_model(model, predict, explain, input, output,
  reference, golden, owner, purpose)`, `check(pkg)`, `publish(pkg, registry)`
  and the schema helpers `int()`, `double()`, `decimal(p, s)`,
  `categorical(levels, ordered)`, `date()`, `timestamp(tz)`.
- Python `okay_model`: the same three calls and helpers.
- `check` fails on a missing env lock, a golden mismatch, a contract that
  does not fit the sample, and output that differs between two runs.
- Written in their languages, tested in their containers (R + arrow, Python
  + pyarrow), versioned with okay.

## Changes to existing okay modules

### okay-arrow

- [ ] a DICTIONARY column survives a stream of ZERO record batches: kept as
      `Dictionary` with no rows and its value type (found by okay-watch's
      property test, 2026-09-26: pandas round-trips an empty categorical
      column, the reader answered a plain string column)
- [ ] decimal128 through `frameTable(exact)` to R and Python and back
- [ ] a timestamp's zone kept through R (POSIXct `tzone`) — measured, stated

### okay-foreign

- [ ] `ForkedWorkers` (from okay-watch's `ForkServer` + `zygote.py`): a
      parent process loads what the caller names (a module function, its
      arguments), freezes the GC, listens; every worker a connection, the
      child running the shim on the socket; a dead worker a new fork.
      Python first; R after okay-r's shim serves an inherited connection.
- [ ] a worker's pid reported by the shim's hello, so a pool shows it and
      a kill is by pid (today okay-watch asks `os.getpid` after connecting)
- [ ] `exact` the default for a caller that reads Tables (the model
      runtime), kept opt-in for frames

### okay-r

- [ ] the shim serves an INHERITED connection (`okay_serve(con)`, not only
      fds 0/1 — base R has no `dup2`), so a forked R pool is possible:
      `zygote.R` loads the model, `parallel::mcfork`s per connection
- [ ] stated in the spec and pinned by a test: int32's minimum is R's
      `NA_integer_` (a value that becomes missing), and what R's `arrow`
      does with it on the way back (okay-watch's probe, 2026-09-26)
- [ ] the R + arrow round-trip property suite in okay (every contract type
      and missing state), where okay-watch's lives today

### okay-stream

- [ ] `Chunks.within(maxRows, maxWait)`: a chunk closes at `maxRows` or
      `maxWait` after its first element, whichever first — the latency mode

### okay-persist

The MECHANISMS of a regulator-grade log; the decision record that uses
them is okay-watch's.

- [ ] `Chain`: a topic whose record is `<sha256>\n<body>`, the body
      carrying `seq` and `prev`; `append(body)`, `verify` (the first broken
      link: EDITED, DELETED, REORDERED), `head` (to anchor outside)
- [ ] signed checkpoints: every N records or T minutes, the head signed
      with a key from an okay-conf secret reference, written to the chain
      (MDR §7.2)
- [ ] `Sealed`: a compacted payload topic, AES-256-GCM under a key from an
      okay-conf secret reference; `put(key, text)`, `get(key)`, `purge(key)`
      (a tombstone, then compaction; FileStore's active segment rolled so
      the ciphertext leaves the disk — measured, not assumed)

### okay-kafka

- [ ] a consumer that commits a partition's offset only when a caller's
      `durable(offset)` says so, and hands every record its (topic,
      partition, offset) identity

## Stages (each lands on its own; M0–M3 are MDR's milestones)

1. **okay-arrow and okay-r fixes** — the empty dictionary, the int32
   minimum stated, decimal and timestamp zones through R and Python. (M0)
2. **okay-model** — package, manifest, contract, ContractTables, the
   scoring operation, golden check; okay-watch's contract code moves in. (M0)
3. **okay-model-runtime** — backends, pool, routing, sources, hooks;
   okay-watch's scoring pipeline becomes a user of it. (M0)
4. **okay-foreign `ForkedWorkers`**, then **okay-r inherited connection**
   and the forked R pool. (M0 for Python, M1 for R)
5. **okay-stream `Chunks.within`** and the HTTP endpoint (latency mode). (M1)
6. **okay-persist `Chain` and `Sealed`**, okay-watch's journal moved onto
   them; signed checkpoints. (M1)
7. **okay-kafka offset identity** and the crash test (kill -9, no loss, no
   duplicate). (M1)
8. **okay-model-spark**, **okay-model-cli**. (M1)
8a. **okay-model-chain** — the crypto pack's open part: the chain source,
    positions, rollback and finality events. (the pack's C0, after M0)
9. **Analyst packages** R `okaymodel`, Python `okay_model`. (M1–M2)

## Measurements each stage owes (MDR §15, §16)

- Chunk sizes 1, 64, 512, 4096 × R/Python × JSON/Arrow: records/s and p99
  (okay-watch `bench/scoring.md` has the protocol and the first rows).
- Per-record JSON over HTTP (Plumber, FastAPI) vs chunked Arrow, same model.
- Fork/COW vs independent workers: RSS and PSS per worker (Python measured
  2026-09-26: PSS 52 vs 205 MB a worker for a 44 MB forest; R to come).
- ≥ 20k records/s per 8-core node for a GLM in R at chunk 512 — measure,
  report actual.
- p99 ≤ 50 ms single-record HTTP in latency mode — measure, report actual.

## Decisions

- **Processes and pipes, not embedding** (MDR §19's first question,
  answered): okay-r and okay-foreign already decided it — a process per
  worker, the model held, Arrow over the pipe; measured in okay-watch's
  bench: chunked Arrow over a pipe is the same order as a batched HTTP
  endpoint and 50–100x a per-record one. Embedding is refused for R (one
  interpreter, global state behind JNI) and unnecessary for Python.
- **Mechanisms here, policies in okay-watch**: the chain, the sealed store
  and the forked pool are useful to any okay user; the decision record,
  retention, four-eyes and reports are the regulated product's.
- **Hooks are okay-kernel ports**: the commercial modules are plugins; the
  open build without them is complete and useful (decisions to a file or a
  topic).

## Out of scope

Training, feature stores, notebooks; GPU and LLM serving; the regulated
product's own features (okay-watch `specs/mdr.md` §7–§13).

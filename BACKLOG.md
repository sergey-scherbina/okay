# Backlog

## Cross-cutting — the 2026-09-01 audit (specs landed e3b5a74; slugs are implementation)
- [ ] queue-shape — DECIDED (specs/data.md, Queues): no Queue seam;
      ingress/egress bridges to topics with message-id dedup —
      implement the two bridges; engine adapters as named
- [ ] blob-seam — spec landed (specs/blob.md): fs engine + own
      SigV4 S3 subset over the http client; MinIO live tests;
      implement stage 0 (fs) then stage 1 (S3)
- [ ] wire-tls — spec landed (specs/tls.md): one transport seam,
      sslmode vocabulary, verify-full default, platform crypto;
      implement JVM SSLEngine leg first
- [ ] persist-wire-auth — spec landed (specs/persist.md, stage 2):
      bearer/API key via okay-security, per-topic capabilities,
      TLS via specs/tls.md; implement with persist-wire
- [ ] own-db-migrations — spec landed (specs/jdbc.md, Own
      relational databases): Flyway model, checksummed scripts,
      schema-version table + ops topic; implement over the Sql seam
- [ ] obs-tracing — spec landed (specs/obs.md): spans as values on
      a trace topic, W3C traceparent at the edges, tracing handler
      composition; OTLP exporter a later interop
- [ ] persist-backup — spec landed (specs/persist.md, Backup and
      restore): what remains is the DOCTOR tool (recovery scan
      against a backup, offline) and a blob-seam copy helper

## Correctness leads
- [ ] nio-close-race — INVESTIGATED 2026-09-01, narrowed but not fixed.
      Reproduced at ~1.3/1000 rounds (100 lines x ~480 bytes,
      localhost): the SERVE fiber stalls after 0–4 completed writes —
      its next write's completion handler never fires, `onComplete`
      never runs (stall, not death) — while the client usually sees a
      premature EOF, and in one round the listener's accept failed
      with AsynchronousCloseException while the client still received
      4 lines. A leak-free sequential harness also reproduced the pure
      HANG variant (no EOF at all). Suspects, in order: the default
      AsynchronousChannelGroup's dispatch under rapid
      channel-create/close cycles (macOS/KQueue), then a close racing
      a queued write. NOT the okay Async driver — its Await/cell CAS
      protocol was read and is sound, and CanBlock.block only cancels
      on interrupt. Next steps: (a) dedicated channel group per test
      to isolate the default group; (b) `shutdownOutput()` + drain
      before `close()` in `Nio.Conn`; (c) jstack the stalled fiber.
      Harness pitfall recorded: a watchdogged round that times out
      LEAKS its listener and thread — only the first failure of a run
      is trustworthy under that harness.

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-security (specs/security.md — staged, like persist)

## okay-codec
- [ ] codec-defaults — decode falls back to a field's declared
      default when the wire lacks it (companion-default access =
      a macro; Mirrors do not carry defaults)

## okay-py (specs/py.md — Python as a handler; model = specs/r.md by reference)
- [ ] py-subprocess — stage 0: module + shim (stdlib json wire,
      module:name addressing, clean env, version handshake),
      verify via importlib.metadata (the wrong-venv refusal)
- [ ] py-worker — stage 1: persistent worker holding imports;
      N-workers parallelism; two-engine acceptance
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
- [ ] docs-seam — the Docs trait (get/put CAS/delete/query,
      declared consistency) + the own-posture engine over View;
      then one foreign adapter (Dynamo or Mongo) to prove the seam
- [ ] jdbc-bulk-load — OLAP write posture: staged files + load-id
      idempotency (WithKey at batch granularity); row-DML refused
- [ ] lake-read-duckdb — Parquet/Iceberg reads through the JDBC
      seam (DuckDB embedded); verify + constant-memory asserted
- [ ] lake-delta — Delta specifics: DuckDB delta extension via the
      JDBC seam (read), Delta Kernel interop (read/write, no
      Spark), spark-bridge writes already available (specs/data.md)
- [ ] rag-pgvector — VectorStore over specs/jdbc.md against
      Postgres/pgvector; agrees with the memory engine on a fixture
- [ ] kafka-eos — producer idempotence/transactions on the okay-kafka
      sink, or the at-least-once contract asserted where EOS is off

## okay-cache (specs/cache.md)
- [ ] cache-memory — Cache trait, bounded LRU memory engine,
      single-flight getOrLoad, named Regime (no default TTL),
      negative caching, stats as values
- [ ] cache-view — regime 1: the log-fed View over a compacted
      keyed topic (lag = consumer lag; pairs persist-stage1)
- [ ] cache-redis — minimal RESP client (GET/SET PX/DEL/PING) over
      the Async transport, same contract suite; invalidation topic
      over okay-persist for cross-node regime 2

## okay-jdbc (specs/jdbc.md — the foreign database)
- [ ] sql-seam — okay-sql: the Sql driver trait (SqlValue/Col,
      Async), typed layer (rows/params/verify/transact) written
      once against it; okay-jdbc as the first driver passing the
      whole jdbc.md behavior list (H2, no-DDL user)
- [ ] sql-pg-wire — okay-pg: the Postgres v3 protocol over the
      Async transport (SCRAM, extended query/portals, COPY);
      cross-platform; same typed program runs over both drivers
- [ ] sql-r2dbc — the JVM reactive-driver hatch behind Sql (LOW:
      driver availability, not performance — virtual threads
      already cover JDBC-behind-Async)
- [ ] jdbc-write-bridge — the Durable policies over their unique
      constraints (WithKey = ON CONFLICT, Reconcile = SELECT by
      key), journaled in okay-persist
- [ ] jdbc-poll-source — incremental poll by a monotone column with
      journaled watermark and lag window (stated non-CDC)

## okay-conf (specs/conf.md)
- [ ] conf-impl — Secret/Secrets (env, file, memory, chain),
      Conf.read/load; cross-built; the invariants' tests (error
      names the ref, toString is the ref, trailing-newline trim)
- [ ] conf-topic — stage 2: managed config as a compacted keyed
      topic over okay-persist (audit/rollback for free)

## okay-persist (specs/persist.md — staged design; stage 0 landed)
- [ ] persist-wire — the remote Topic client over the stage-2
      frames: a non-JVM consumer reaches a persist node directly;
      format and wire as documented surfaces
- [ ] persist-replication — stage 2: leader/follower per partition,
      epochs, high-water mark, quorum acks, operator failover,
      replica stats, idempotent producer window
- [ ] persist-interop — stage 3: Store over Kafka (okay-kafka), a
      JDBC table, segment offload to object storage
- [ ] persist-consensus — stage 4: elected leadership, its own spec
      (Raft vs delegating election to a stage-3 engine)

## okay-http (sibling's area — coordinate before taking)
- [ ] http-flaky-mcphttp — TestMcpHttp "one Serving, three wires"
      answered 503 once in a full-matrix run (2026-09-01); green
      alone and on suite rerun — likely a port/readiness race
- [ ] http-streaming-responses — incremental bodies on the NIO and
      Netty backends (Jetty has it); unblocks MCP push there
- [ ] http-post-body-audit — Netty/NIO: do POST bodies reach routes?
      (Jetty's did not — found by mcp-push, fixed there)

## Elsewhere
- [ ] agent-langchain4j — their providers as Model handlers (ROADMAP
      P9 leftover)
- [ ] history-tsv-tabs — six rows with literal \t instead of tabs
      (bpeScan, indexKeyword, keywordTerms, jsonToCst, jsonProject,
      jsonDecode, buildOnly area; flagged in the room 2026-08-31)

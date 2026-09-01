# Backlog

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-durable — event-sourced sessions: intent-first journal
      (Durable shape), refold recovery, snapshots; the journal is an
      okay-persist topic (specs/persist.md; seam agreed 2026-09-01)
- [ ] ui-dom-patch — a raw-DOM patch Backend over js.Dynamic (the
      React host covers the browser today; Host.diffing is ready)
- [ ] ui-keyed-diff — keyed children matching in the diff (API
      unchanged, keys already in the tree)
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-codec
- [ ] codec-vector — Schema for Vector (and recursion + default
      params in derivation): what ui-wire needed and shipped without,
      on the hand mapping instead

## okay-mcp
- [ ] mcp-completion — completion/complete for prompt/resource args
- [ ] mcp-resource-templates — RFC 6570 templates, list + expand
- [ ] mcp-resumable-sse — Last-Event-ID on the HTTP GET stream
      (the journal is an okay-persist topic: read(from) IS
      Last-Event-ID, TooEarly answers a compacted-past id)

## The data landscape (specs/data.md — umbrella; vendor = seam impl)
- [ ] docs-seam — the Docs trait (get/put CAS/delete/query,
      declared consistency) + the own-posture engine over View;
      then one foreign adapter (Dynamo or Mongo) to prove the seam
- [ ] jdbc-bulk-load — OLAP write posture: staged files + load-id
      idempotency (WithKey at batch granularity); row-DML refused
- [ ] lake-read-duckdb — Parquet/Iceberg reads through the JDBC
      seam (DuckDB embedded); verify + constant-memory asserted
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
- [ ] jdbc-typed — Schema row decode (label-matched, total) +
      verify against ResultSetMetaData + typed params + transact
      region with declared isolation; H2 no-DDL-user test
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
- [ ] persist-stage1 — consumers prove the seam: Durable.Journal over
      a keyed topic (complete-as-append), streaming/tailable reads,
      typed Schema view with upcasts, consumer offsets, compaction
      (= snapshots), a Snapshots put/latest convenience (asked for
      by the ui lane)
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

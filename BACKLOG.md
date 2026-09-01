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
- [ ] persist-backup — spec landed (specs/persist.md, Backup and
      restore): what remains is the DOCTOR tool (recovery scan
      against a backup, offline) and a blob-seam copy helper

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-security (specs/security.md — staged, like persist)

## okay-codec

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
- [ ] lake-delta — Delta specifics: DuckDB delta extension via the
      JDBC seam (read), Delta Kernel interop (read/write, no
      Spark), spark-bridge writes already available (specs/data.md)
- [ ] rag-pgvector — VectorStore over specs/jdbc.md against
      Postgres/pgvector; agrees with the memory engine on a fixture
- [ ] kafka-eos — producer idempotence/transactions on the okay-kafka
      sink, or the at-least-once contract asserted where EOS is off

## okay-cache (specs/cache.md)
- [ ] cache-write-through — regime 2 paired with okay-jdbc: the
      invalidate-AFTER-commit ordering asserted, the stale window
      between commit and invalidate demonstrated and documented
      (the one cache.md box no existing slug covered)
- [ ] cache-redis — minimal RESP client (GET/SET PX/DEL/PING) over
      the Async transport, same contract suite; invalidation topic
      over okay-persist for cross-node regime 2

## okay-jdbc (specs/jdbc.md — the foreign database)
- [ ] sql-pg-wire — okay-pg: the Postgres v3 protocol over the
      Async transport (SCRAM, extended query/portals, COPY);
      cross-platform; same typed program runs over both drivers
- [ ] sql-r2dbc — the JVM reactive-driver hatch behind Sql (LOW:
      driver availability, not performance — virtual threads
      already cover JDBC-behind-Async)

## okay-conf (specs/conf.md)

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
      (second sighting, same family: okay-jetty TestResumable
      failed its first subscribe once in a full-matrix run
      2026-09-01, green twice alone — port/readiness race shape)
- [ ] http-streaming-responses — incremental bodies on the NIO and
      Netty backends (Jetty has it); unblocks MCP push there
- [ ] http-post-body-audit — Netty/NIO: do POST bodies reach routes?
      (Jetty's did not — found by mcp-push, fixed there)

## Elsewhere
- [ ] rag-langchain4j — their EmbeddingStore as a Retrieve handler
      (the other half of the interop sentence; when a consumer
      names a store)

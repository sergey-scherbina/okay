# Backlog

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-durable — event-sourced sessions: intent-first journal
      (Durable shape), refold recovery, snapshots; the journal is an
      okay-persist topic (specs/persist.md; seam agreed 2026-09-01)
- [ ] ui-dom-patch — a raw-DOM patch Backend over js.Dynamic (the
      React host covers the browser today; Host.diffing is ready)
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-security (specs/security.md — staged, like persist)
- [ ] security-es256 — ES256 for JWT (the JOSE raw R||S <-> DER dance,
      its own tested task)
- [ ] security-node — the Crypto seam over node:crypto, so the JS leg
      verifies too
- [ ] security-oidc — id_token validation, discovery document,
      nonce/at_hash
- [ ] security-argon2 — a satellite with a real KDF for new password
      stores (PBKDF2 stays the zero-dep default)

## okay-mcp
- [ ] mcp-resumable-sse — Last-Event-ID on the HTTP GET stream
      (the journal is an okay-persist topic: read(from) IS
      Last-Event-ID, TooEarly answers a compacted-past id)

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

## The textbook (user-requested, 2026-09-01)
- [ ] theory-textbook — a tutorial on the theories okay is built from,
      with the scientists' names, references to their papers, okay code
      as the running examples, and for each design decision the WHY
      grounded in the cited work. Chapters, with the anchors to cite:
      * monads and functors — Moggi (Notions of computation and
        monads, 1991), Wadler (Monads for functional programming,
        1995); okay's `!`/`Free` as the running example
      * PARAMETERISED monads — Atkey (Parameterised notions of
        computation, 2009); okay's `Cont`/`PState` typestate
      * free and FREER monads — Swierstra (Data types a la carte,
        2008), Kiselyov & Ishii (Freer monads, more extensible
        effects, 2015); okay's `Free`, the `Bind` normalization, why
        freer (no Functor constraint) and what the left-nested
        rebalancing buys (the bind-chain benchmark)
      * continuations, with and WITHOUT prompts — Felleisen (the
        theory of prompts, 1988), Danvy & Filinski (Abstracting
        control, 1990: shift/reset); okay's `Cont`, `Delim`
        (`shift`/`prompt`), `Loop`/`take`/`put` in Generate, and why
        the stack-safe Cont paramonad founds the whole tower
      * algebraic effects and HANDLERS — Plotkin & Power (Algebraic
        operations and generic effects, 2003), Plotkin & Pretnar
        (Handlers of algebraic effects, 2009); okay's effect rows as
        unions, `Handler`/`!.translate`/Cont-valued handlers as three
        points on one line, `TypeableK` row splitting and its trusted
        kernel; the Writer GADT story (six encodings,
        docs/existentials.md) as a worked example of why
        representation decides erasure
      * final TAGLESS — Carette, Kiselyov & Shan (Finally tagless,
        partially evaluated, 2009); okay's tagless layer over the
        Cont paramonad
      * STAGING — Taha & Sheard (MetaML, multi-stage programming,
        1997/2000); okay's inline/summonFrom specialization
        (ChunkBuf, Fold) as staging-by-inlining, and the reified
        `Pipeline` operator tree (reify, rewrite, compile) as
        staging-by-data
      * the sketches' papers are already cited in Sketch.scala
        (Flajolet HyperLogLog, Cormode-Muthukrishnan Count-Min,
        Dunning t-digest) — the chapter collects them
      Format: docs/theory/ with a page per chapter, cross-linked with
      the typepedia and the specs; every claim about okay backed by a
      file:line, every theory claim by a citation. The existing
      docs/existentials.md shows the house voice for this kind of
      writing.

## Elsewhere
- [ ] agent-langchain4j — their providers as Model handlers (ROADMAP
      P9 leftover)
- [ ] history-tsv-tabs — six rows with literal \t instead of tabs
      (bpeScan, indexKeyword, keywordTerms, jsonToCst, jsonProject,
      jsonDecode, buildOnly area; flagged in the room 2026-08-31)

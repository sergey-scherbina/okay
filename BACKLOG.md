# Backlog

Long-term items. Agents do not pick work from here directly — promote
to `SPRINT.md` when an item is ready to be worked on, and delete it
here at that point.

Each entry says what was measured or observed, so the next person
starts from evidence rather than from the idea.

## Performance — found by measurement, not yet acted on

- [ ] aggregator-floor — `aggCount` is 5.1us against a hand-loop floor
      of 8.6, so the flat cases are done. `aggVariance` is 74.7 against
      a floor of 49.6, and that gap is Welford's per-element division
      rather than the accumulator. Only worth touching if quantiles
      become hot.
- [ ] chunked-lex-residual — chunked lexing is SLOWER than
      element-wise (52.6 against 42.6) and the residual was measured to
      be per-CHUNK bookkeeping, not per-character boxing: a
      `Vector.newBuilder`, a token-chunk allocation and a Free node per
      input chunk. The value of the chunked path is streaming at
      constant memory, not speed; a builder reused across chunks is the
      obvious lever if it ever matters.
- [ ] json-read-gap — `Json.read` is 39.1us against circe's 1.33, and
      the split says 95% of it is scan+parse to a lossless CST. That is
      what losslessness and totality cost, and it is what they buy. The
      only honest lever is making the scanner cheaper, not skipping the
      CST.

## Build and tooling

- [ ] phantom-nowarn — five `@nowarn annotation does not suppress any
      warnings` appear in an aggregate clean build and in NO single
      module build, with no source position. They predate this work.
      Looks like a zinc/sbt artifact rather than code; cosmetic, but it
      keeps the build from being warning-clean.
- [ ] agents-md — this repository has no `AGENTS.md`, so the
      multi-agent protocol falls back to defaults (`SPRINT.md`,
      `BACKLOG.md`, `CHANGELOG.md`) and the spec-dev skill to
      `specs/` + `SPEC.md`. `specs/` matches; there is no global
      `SPEC.md`. Writing one would let both skills read their
      configuration instead of guessing, and would be the place to
      record the house rules that currently live only in the specs
      (dependency-free core, one dependency per module, transports are
      values not effects, totality).

## okay-http and its backends

- [ ] http-request-streaming — request bodies are `Empty | Text |
      Bytes`. The JVM could stream them (`BodyPublishers.fromPublisher`)
      but `fetch` needs a duplex mode that is not reliably available, so
      the shared type stays honest. Revisit when duplex fetch is
      dependable.
- [ ] ws-server-native — serving WebSocket is out of scope in
      `okay-http` because the JDK has no server-side API. okay-jetty and
      okay-netty both serve it now, so this is closed in practice; the
      item exists only to record that the base module still cannot.
- [ ] scala-js-dom — the JS transports use raw `js.Dynamic`, matching
      `llm.TransportJs` and the dependency rule. The surface here
      (Headers, ReadableStream reader, WebSocket events) is much larger
      than the one `fetch` call that precedent was set on. If the
      untyped surface starts costing bugs, the facade earns its weight.
      Decided once, in `specs/http.md`; recorded here so it is
      revisited deliberately rather than drifted into.

- [ ] perf-records-stale — records that now send a reader to work
      that is done or to a mechanism that does not exist, found by the
      2026-09-27 review of Free/Cont/Delim/schedulers/merge (docs-only
      lane; gate is `TestDocSnippets` + `scripts/check-citations.sh`).
      Each is a one-line fix with the current source cited: (1)
      docs/benchmarks.md §2 table still reads Reader 79 beside prose
      saying 60.6 (effect-op-cost), §22 quotes 2.24x from
      `freeDirectNested` 16.8 where §2c says 14.2 and effect-op-cost
      13.1 — one number, one ratio; §4 keeps the "8 us of bookkeeping"
      paragraph its own next paragraph refutes (27 ns each); the §6
      header row (elementwise 122) and §6b predate
      source-merge-via-ready — mark them as such or re-run the one
      lane. (2) specs/schedulers.md:43 says `adaptive` "moves a fiber
      to loom when it blocks" — not built (see own-managed-blocking;
      until it lands, say "stuck-check + overflow workers"); its
      Decisions say the stuck test reads the OWNER end, Results say
      the thief end — Results is what is built (Platform.scala:587+).
      (3) specs/interpreter-optimization.md describes `Cont.Fuse` with
      a depth field and a three-shape `/` runner — both gone
      (cont-fuse-one-step, freer-base); a dated note at the top that
      the file is history, or fold its live parts into
      specs/freer-base.md. (4) specs/jdk17-adaptive-runtime.md cites
      `src/main/scala-jvm/Platform.scala`; it is
      `okay-platform/src/main/scala-jvm/Platform.scala`. (5) BUGS.md
      `growing-stale-route` says `status: reopened` (line ~142) while
      backlog and memory say closed as a trade (8af62bc7) — BUGS.md
      loses. (6) `fusedSWr` is quoted at 13.7 us in
      `backlog.d/okay-core/handler-single-pass.md` and 12.4 in
      specs/handler-fusion.md's 2026-09-27 row — the bar should name
      the latest. (7) relay's loop size is 244/262/266/305 across
      memory, docs/benchmarks.md §2 and specs/core-gaps.md — the
      number `TestInlineBudget` asserts is the one to cite, the others
      dated. Not in scope: renumbering, prose, anything a lane in
      flight (map-flatmap-pair-cost, ring-chunk-bimodal-forks) will
      rewrite anyway. (2026-09-27, perf-plan)

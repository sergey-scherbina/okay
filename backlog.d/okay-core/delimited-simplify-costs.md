- [ ] delimited-simplify-costs — PRIORITY: MEDIUM, MEASURED 2026-10-04
      (history.d shift-generator-cost). Against cont-atm (6fc400e08), after
      shift-generator-cost: statePara 1.13x (+32 KB), fib100 1.11x (+3 KB),
      stateForeign 1.31x (same bytes). Two causes, measured apart:
      (1) stateForeign — N operations of State inside `Shift.run` under ONE
      reset — is push-one-boundary (ec9ee796a: 41.5-42.9 us; the same tree
      with `push` written back as `dollar(p)(pure)` 31.7-36.4). Per
      operation the two forms do the same work (the boundary is installed
      once a run), so it is the JIT's shape of the loop, not the machine's
      work: PrintInlining shows `go` (1025 B) never inlined in either, and
      the profile moves time into the benchmark's own `boxToInteger`
      (1.5% -> 26%). Read under hsdis before changing anything.
      (2) statePara/fib100 — the strict `k` of Cont — the one loop answers
      `Free[F, Z]`, so a forced `k` allocates a `Return` for its answer, and
      the `Kont` is called virtually (`Segment`, `Held`). A loop for a
      machine ALONE that answers `Z` itself would take both back, at the
      price of a second copy of `go` — the copy delimited-simplify deleted.
      (1) ANSWERED 2026-10-04 (state-foreign-shape, history.d): in one build,
      `push` as one boundary 40.5-41.9 us against `dollar(p)(pure)` 32.1-32.4;
      LogCompilation shows why — with `push` the continuation call `f(a)` in
      `Run.go` sees only the benchmark's two lambdas, and C2 inlines them
      (and `boxToInteger`, `Integer.valueOf`, State's constructors) into the
      loop: `go` 3832 B of code against 2568. `dollar`'s `ret` frame passes
      the same site and makes it megamorphic. With
      `-XX:CompileCommand=dontinline,okay.DelimBenchmark::*` `push` reads
      33.4: the gap is the inlining, not the machine's work — an artifact of
      a program with two continuation lambdas; a real one's site is
      megamorphic. Same mechanism as cont-frames-register-pressure. Open:
      (2), the strict `k`.
      (2) DONE 2026-10-04 (strict-k-cost): the allocation profile named it —
      one `Freer.Return` a strict `k` (Return per Bind 1.99 against cont-atm's
      1.35), the one loop's `Free[F, Z]` answer built and taken apart. A
      machine alone has its own loop again (`goAlone`, answering `Z`):
      statePara 0.94x, fib100 0.95x master's; against cont-atm 1.06x left.


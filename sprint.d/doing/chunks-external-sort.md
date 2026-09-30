- [ ] chunks-external-sort — a sort of a `Chunks[A]` that does not
      hold the stream: RUNS of `budget` elements sorted in memory and
      spilled to disk, then a k-way merge of the runs (a heap over one
      cursor per run) read back as `Chunks[A]` (Knuth vol. 3 §5.4,
      replacement selection optional). `Chunks.sortBy(p)(key)(using
      Ordering[K], Spill)`, where `Spill` is a facade (OURS OR THE
      STANDARD ONE, AGENTS.md): ours writes runs with okay-codec's
      binary `Codec[A]` into temp files under a `Resource` that deletes
      them; the JVM/Native platform supplies the file, JS has no disk
      and answers in memory up to the budget and refuses BY NAME above
      it. Then `Sort.viaTables` (Tables.scala, today "collect, sort,
      hand back" in memory) answers through it on `localBulk`, and the
      third road of the join question closes: two large UNSORTED parquets
      on one JVM = external sort of each side + `Chunks.joinSorted`
      (specs/stream-join.md), which the planner picks by itself under
      [[join-strategy-auto]]. Why: asked 2026-09-30 ("два паркета, без
      окна, полностью"): a hash join needs one side in memory,
      `joinSorted` needs both sides ordered, and between them one JVM has
      nothing today; Spark shuffles for us, a laptop cannot. Tests: the
      sorted output equals `sortBy` in memory on random input at budgets
      1, 7, 64 and above the input (no spill); the run count is
      ceil(n / budget); temp files are gone after the stream ends AND
      after an early stop (`take`); the memory held is one run plus one
      chunk per open run (measured, `Diagnosable`); a docs page with the
      literature (Knuth §5.4, the grace/hybrid hash join paper for the
      alternative refuted here: Shapiro 1986).
      (2026-09-30, operator ask)

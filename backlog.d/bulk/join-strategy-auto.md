- [ ] join-strategy-auto — the planner PICKS how a join runs, and a
      program can SAY. Today `Tables.join` has one strategy, the
      platform's hash join (`Bulk.join`), and the rewrite only turns
      the smaller side right; `Chunks.joinSorted` (specs/stream-join.md)
      exists beside it and nothing chooses between them. THE ASK
      (operator, 2026-09-30): `l.join(r)` decides by default, and
      `l.join(r, Join.Hash | Join.SortMerge | Join.Auto)` overrides.
      (1) `Plan.Join` carries a `Join.Strategy` (`Auto` default; `Hash`;
      `SortMerge`; later `Broadcast`/`Shuffle` where a platform names
      them), shown by `Plan.show` so a test reads the decision, and
      `Bulk.join` gains the strategy as a parameter with `Auto` meaning
      "the platform's own" so the local instance can run either
      (`Chunks.joinSorted` or its hash join) and Spark ignores what it
      cannot do (records that it did, in the log). (2) The rule for
      `Auto`, in `Plan.optimize` where the facts are: both sides KNOWN
      ORDERED by the join key → sort-merge (nothing held beyond a run);
      else the smaller side's `estimate` under a memory `budget`
      (`Tables.via(B, budget = …)`, default from `Runtime.maxMemory`) →
      hash, small side right (today's turn); else, once
      [[chunks-external-sort]] lands, external sort of each side and
      sort-merge — until then `Hash` with a `log` line naming the risk.
      (3) "KNOWN ORDERED" is a fact on the plan, never a guess:
      `Plan.ordered(p): Option[Key]` is true under a `Sort.By` node with
      that key, under `Read` of a format whose footer says so (Parquet
      `RowGroup.sorting_columns` — `Footer` does not carry it yet, one
      field to add in okay-parquet, and `groups` are read in file
      order), or under an explicit `t.assumeSorted(key)` node for a file
      the writer sorted without saying — WRONG assumptions fail loudly,
      since `joinSorted` checks the order at every row; `Select`/`Where`
      keep the fact only when the key is provably untouched, which a
      function is not, so the fact survives `Where` and `Columns` and
      dies at `Select`/`Expand` unless the join key is the same
      `A => K` value (reference equality; documented as the limit).
      An explicit strategy that the facts contradict (`SortMerge` on
      unordered sides) runs and fails at the first out-of-order row —
      the program said so; `Hash` on sides that do not fit is the
      user's memory to spend. Tests: the decision per fact table (ordered
      × fits × explicit) read from `Plan.show`; the answer identical
      under every strategy on the same input (the multiset law
      `TestSortMerge` already has); Parquet written sorted reads back
      `ordered`; okay2's Tables in step. Docs: guide §6's join paragraph
      gains "how it is chosen" and `docs/modules/okay-stream.md` the
      strategy table; literature: Selinger et al. 1979 (the optimizer
      chooses the join method by cost), Graefe 1993 survey.
      (2026-09-30, operator ask)

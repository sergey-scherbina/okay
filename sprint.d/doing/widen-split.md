- [~] widen-split — `!.widen` carries two jobs under one name, and
      only one of them is what its callers ask for. Its type says
      "the same program in a wider row", which `RowLift.plus[G]`/
      `.at[R]` already do for free (one commented coercion, nothing
      forced, nothing walked). Its body is a WALK: it resumes the
      head and rebuilds the tree node by node — a normalisation whose
      only measured beneficiary is `Source.merge` (5-7% faster with the
      walk than with a free upcast, specs/writer-covariance.md
      free-row-variance), because the rotation happens outside the
      merge's contended region. Found while closing
      windows-stage-rerun-loses-pane (2026-09-23): the walk's eager
      head forced a `Delay` and started stateful stages at widen time
      — fixed there, but the fix treats a symptom of the doubled
      name. ~37 call sites over 10 modules (jdbc 6, http 5, ui 4,
      stream 4, llm 4, resilience 3, direct 3, demo 3, agent 3, sql 2)
      say `widen` and, read one by one, want the upcast.
      THE LANE: (1) `!.normalize[A, F, G]` (or `rotate`) is the walk,
      named for what it does, and `Source.merge` calls it by that
      name with the measured reason beside the call; (2) `!.widen`
      becomes the coercion (`plus`), so every other site loses a walk
      and an eager head; (3) the operator's question that filed this,
      answered in the spec: is a row upcast ever legitimately a walk?
      NUMBER OWED: the merge lane unchanged (it keeps the walk by
      name), and one stream lane that only upcasts — `through` over a
      widened pure stage, TestPipe's shape — faster or equal, with
      the count of walks removed per pull. Operator, 2026-09-23:
      "widen несёт двойственную нагрузку — может стоит разделить?"

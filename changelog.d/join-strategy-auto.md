## join-strategy-auto - a Tables join chooses how it runs, and a program can say

- `JoinStrategy[K]` on `Plan.Join`: `Auto` by default becomes `SortMerge` when both
  sides are known sorted by key under ONE ordering — `t.sortByKey`, or
  `t.assumeSortedByKey` (the caller's word, checked row by row by the merge) —
  seen through a `Where` and lost at any function; `Hash` otherwise, the
  smaller side right as before. `l.join(r, JoinStrategy.Hash())` /
  `JoinStrategy.sortMerge[K]` say it outright. `Plan.show` prints `Join(hash)` /
  `Join(sort-merge)`.
- `Bulk.joinSorted` and `Bulk.sortByKey`, defaults for every instance (a hash
  join; a sort in memory): the local instance and BulkParallel merge with
  `Chunks.joinSorted`, the engine (`FlowBulk`) merges per bucket, Spark sorts
  by key natively.
- Not yet: two large unsorted sides through the external sort — a plan cannot
  summon a `RunCodec` for an erased element (specs/bulk.md).
- Tests: `TestJoinStrategy` (JVM, JS, Native: merged vs hashed same pairs, the
  fact through Where and lost at Select, one side or two orderings hash, the
  caller's word checked, the explicit strategies), TestPlan updated. Gate
  `affected master staged`.
- Commits: 16744449e.

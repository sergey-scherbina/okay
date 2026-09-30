## bulk-local-parallel - a parallel Bulk in one process on the platforms with threads

- `BulkParallel(parallelism, lines, bytes)(using Scheduler)` in okay-stream's
  scala-jvm-native, and `parallelBulk(n)` beside `localBulk` on the JVM: the
  same `Bulk[Chunks]`, a `Tables` program runs on it unchanged. Three
  operations spread over fibres: `read(path, format)` reads a file's splits
  ahead of the consumer in split order; `aggregate` folds batches of 16
  chunks on fibres and merges the partials in input order (a non-commutative
  `Sequential` stays right); `join` folds the right side in parallel and
  streams the left through a window of fibres, in the left side's order.
  `of`, `csv`, `map`, `flatMap`, `filter`, `cache` delegate to
  `Bulk.local`, which is unchanged.
- JS has no such instance (one thread), said by its absence rather than a
  parameter that does nothing.
- No measurement, by the operator's word ("Делай без замера"); the law is
  agreement: `TestBulkParallel` (4 — splits once and in order at 1/4/16
  fibres; sum and an order-dependent concat equal the local answer; the
  join's rows and left order; a Tables program and re-reading). Additive;
  gate the suite, Native compile, `affected master Test/compile`.
- Commits: 737fed4f5.

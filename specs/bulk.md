# Bulk — a collection too large to be in one place, said once

## Overview
The P1 algebra (specs/aggregators.md) made ONE aggregation run locally
over Chunks and on Spark unchanged. What it did not cover is the road
TO the aggregation — reading, projecting, joining, expanding — which
every demo so far wrote twice, once against Spark's API and once against
Scala collections, and which is where a platform's API leaks into a
program. `Bulk[D[_]]` is that road, said once: the handful of things a
data platform can do with a collection it holds, as a typeclass in the
dependency-free core, with an instance per platform in that platform's
interop module. The seam rule of specs/data.md applies: a platform
enters as an INSTANCE of this seam, never as an API surface.

## Interface
```scala
trait Bulk[D[_]]:
  def of[A](xs: Iterable[A]): D[A]              // hand a platform a collection
  def csv(path: String): D[Csv.Row]             // a header-first CSV, named fields
  def map[A, B](d: D[A])(f: A => B): D[B]
  def flatMap[A, B](d: D[A])(f: A => IterableOnce[B]): D[B]
  def filter[A](d: D[A])(p: A => Boolean): D[A]
  def join[K, A, B](l: D[(K, A)], r: D[(K, B)]): D[(K, (A, B))]   // equi-join
  def cache[A](d: D[A]): D[A]                   // materialise: what follows reads it twice
  def aggregate[A, Acc, Out](d: D[A])(agg: Aggregator[A, Acc, Out]): Out
  def toChunks[A](d: D[A]): Chunks[A]          // back to the local world
```
- Instances: `Bulk[Chunks]` (core, scala-jvm reads the files) — one JVM;
  `SparkInterop.bulk(spark): Bulk[SparkInterop.Rows]` — a cluster;
  `Parallel.bulk: Bulk[java.util.List]` (okay-java) — one machine's
  cores through parallel streams and the Collector bridge.
- A program written against `Bulk[D]` names no platform; the platform
  is chosen by the instance in scope. The extension methods make it
  read as a collection (`d.map(f)`, `l.join(r)`, `d.aggregate(agg)`).

## Decisions
- **No per-element evidence.** Spark wants a `ClassTag` for every
  element type and Flink a `TypeInformation`; threading that through a
  generic program means an evidence parameter per intermediate type,
  which is exactly the API leak the seam exists to stop. The Spark
  instance instead keeps every element as `Any` under an opaque wrapper
  (`Rows[A] = RDD[Any]`), which is what Spark stores anyway, and pays
  ONE documented cast at the element boundary — the `Refs.slot`
  precedent. A `ClassTag[Long]` would have unboxed a `collect`; the
  seam collects through `toLocalIterator` and never asks for an array.
- **Replayable by construction.** A `Bulk` value may be consumed more
  than once (the demo aggregates the same departures twice). The local
  instance builds every source under `Chunks.defer`, so a file is
  re-read and an `Iterable` re-iterated per run; `cache` is the explicit
  request to hold rows in memory instead.
- **`csv` is in the seam, not beside it**, because reading is the
  platform's: Spark reads a file in parallel, a JVM reads it in one
  stream, and both hand back the same `Csv.Row`. The local parser is
  RFC 4180 on one line (quotes, doubled quotes, commas inside quotes),
  BOM stripped from the header; quoted newlines are NOT handled and are
  said not to be.
- **The collection view is for code generic in `D`.** On a concrete
  `Chunks[A]` — which is a program, `Chunk[A] ! Produce` — `d.map(f)`
  resolves to the program's own monadic `map`, not the seam's; concrete
  local code calls `B.map(d)(f)`. Generic code (`[D[_]: Bulk]`) reads as
  a collection, and that is the code the seam is for.
- **`join` is the equi-join only.** It is what the demo needs and what
  every platform has natively; anything richer is a program over it.

## The effect layer — the same road as a program (bulk-effect, 2026-09-09)

`Bulk[D]` is the platform's contract: nine primitives every platform
must supply, and adding a tenth breaks every platform's build. The
program's vocabulary is a different thing and should be OPEN, so it is
an effect, `Tables`, over the same nine, with the platform's values on
a heap the program never sees:

```scala
enum Tables[+A] derives Effect:                  // Of, Read, Select, Expand, Where, Join, Cache, Aggregate, Collect
  case Select[A, B](t: Table[A], f: A => B) extends Tables[Table[B]]
  ...
opaque type Table[A] = Int                        // a typed slot on the handler's heap (Refs.Ref)
def departures: Table[Dep] ! Tables = direct { ... }       // the plan: a value, no platform in it
Tables.run(SparkBulk(spark))(prog); Tables.run(localBulk)(prog)   // the same value, twice
```

- **Handler = translation into State.** `Tables.via(B)` turns every
  operation into one step on `State % Heap[D]`, using only `B: Bulk[D]`
  — written once for every platform, and the heap is state rather than
  a private field so that an extension can share it (next point). The
  one cast is `Heap.get`, the `Refs.slot` argument: every value on the
  heap was put there under the `A` of the handle that reads it.
- **Extension = a new signature in the row, not a new method.** `Sort`
  is not in `Bulk`. `Sort.viaTables` answers it through the primitives
  (collect, sort, hand back) — correct on every platform, written once;
  `SparkBulk.sort` answers it NATIVELY by translating into the same
  `State % Heap[Rows]`, and `Tables.via` neither knows nor cares. A
  program that sorts says so in its type, `! (Tables + Sort)`, and a
  platform that has not been told about `Sort` refuses that program at
  ITS run site — nothing else in the build moves.
- **A plan is data — and since bulk-plan, ALL of it.** A `Free`
  continuation is a function, so the effect alone could see one
  operation at a time (`!.tracing` records them by running a handler).
  The heap therefore holds PLANS: a building operation (`Read`,
  `Columns`, `Select`, `Expand`, `Where`, `Join`, `Of`) puts a node of
  the first-order `Tables.Plan` tree on the heap and runs nothing; an
  action (`Cache`, `Aggregate`, `Collect`) forces the table it names,
  and at that moment the table's whole lineage is one tree —
  `Plan.optimize` rewrites it, `Heap.compile` turns it into `D` through
  the instance, and only then does the platform run. A native extension
  forces its child and holds the result (`Plan.Held`): a materialised
  boundary, and the rewrite scope is per forced tree.
- **Two rewrites, both measured (bulk-plan, 2026-09-09, A/B in one
  run).** (1) `Columns` is a STRUCTURAL projection — names, not a
  function — so `Columns(Read(p))` becomes `Read(p, cols)` and the
  platform prunes at the parser: `Bulk.csv(path, columns)` is a default
  method (an instance that never heard of pruning still compiles), the
  local instance never puts a dropped column in a Map, Spark selects
  before `.rdd`. stop_times ⋈ trips: Spark 2.5 → 1.3 s, local 1.9 →
  0.95 s. A `Columns` above a `Select` stays where it is: a function is
  opaque, and a rewrite that guessed through one would be wrong
  silently. (2) A join whose left side `Plan.estimate` finds smaller
  than its right is turned around and its answer turned back
  (`turned`, typed, no cast), so the small side is the one a platform
  hashes or broadcasts; the estimate is `Bulk.size(path)` for a read, a
  count for an `Of`, the child's for a step, the larger for a join,
  unknown for a held table — and unknown means no guess. Three joins
  written small-side-left: Spark 5.0 → 2.6 s, local 3.3 → 2.2 s.
- **What the rewrite is NOT.** Not Catalyst: the tree carries opaque
  functions, and the two rules are the two that need none. Broadcast
  itself stays the instance's decision at `join` (bulk-rewrite), made
  with the RDD in hand; the rewrite only makes sure the small side is
  where that decision can see it.
- **Direct style.** `derives Effect` registers the signature, so inside
  `direct { }` a mark (`!prog`) binds a handle: `val deps = !departures.cache`.
  A mark takes the block's own row exactly — a `! Tables` program in a
  `! (Tables + Sort)` block says `.plus[Sort]` — and the combinators on
  programs are row-polymorphic by membership (`In[Tables, F]`), so
  `read(p).select(f).join(q)` types in any row that carries `Tables`.
- **Two spellings, one meaning.** `select`/`expand`/`where`, not
  `map`/`flatMap`/`filter`: a program `Table[A] ! F` is a monad and its
  own `map` is the program's, so the query names are the ones that
  cannot collide. On a handle the same names give a `! Tables` program.

## Behavior
- [x] the same ETL (Wrocław's GTFS: four CSVs joined, service patterns
      expanded into departures) written once against `Bulk[D]` gives
      equal per-hour aggregates on `Bulk[Chunks]` and on Spark
- [x] `Bulk[java.util.List]`: join and aggregate over parallel streams
      equal the local instance's on the same data
- [x] `Csv.fields` handles quotes, doubled quotes and commas in quotes,
      and `Csv.line` is its inverse (quoting only what needs it)
- [x] a local source is replayable: aggregating twice reads twice
- [x] the effect layer: a `Tables` plan run on `localBulk` equals the
      direct computation; `!.tracing` lists its operations before any
      runs; `Sort` — absent from `Bulk` — answers through the primitives
      locally and natively on Spark, to the same top-3
- [x] the Wrocław analysis as ONE program over `Tables + Sort`, run on
      Spark and in one JVM to equal departures, hours and sorted minutes
- [x] bulk-plan: `Columns` over `Read` prunes at the parser; two
      projections intersect; a projection above a function stays; a
      join with the smaller estimated left side is turned and its answer
      turned back, equal to the join as written; unknown sizes are not
      guessed (TestPlan); the rewrite measured A/B on both platforms
      (TestWroclawStages)

## Out of scope
- Flink: `flink-core` alone carries no DataStream, so no instance yet;
  the seam's `Any`-element choice is what a `DataStream[AnyRef]`
  instance would do too.
- optimisation across the seam (predicate pushdown, column pruning):
  the seam is the RDD level, not the Catalyst level, on purpose.

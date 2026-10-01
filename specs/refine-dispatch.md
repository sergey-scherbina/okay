# refine-dispatch — hierarchical routing by document kind, written as a `match`

Status: stage 1 landed (2026-10-01); spec 2026-10-01 (operator ask: "hierarchical routing
(dispatch) by document type in streams, Spark, Flink, Kafka — convenient,
like pattern matching, or even using pattern matching itself"). Builds on
specs/refine.md: `Refine` recognises, `Routable` is what a table runs over
(refine-routable), `Routes` is the declarative table (refine-bulk).

## 1. Why

`Routes` declares lanes as tests (`route[Swap]`, `route(name){ case … }`)
and takes the FIRST that fits. It works, but it is a table in a little
language of its own. Scala already has the language for "which of these
is it, and what do I do with it": `match`. It brings three things a
declarative table cannot:

- **typed delivery** — `eurSwaps(s)` compiles only when `s` is a `Swap`;
- **exhaustiveness** — over a `sealed` document type, a missing case is
  a compiler warning (an error under `-Werror`, which okay and its
  products build with): "you forgot where CDSs go" is found before any
  document is lost;
- **hierarchy for free** — a sub-table is an ordinary method with its own
  `match`; lanes are named by path (`rates/swaps/eur`) and their counts
  roll up by prefix.

## 2. Interface (stage 1)

```scala
package okay.refine

abstract class Dispatch[A, B](val pattern: Refine[A, B]) extends Serializable:
  /** a lane: a name (a path: "rates/swaps/eur") and its values' type */
  protected def lane[X](name: String)(using TypeTest[Any, X]): Lane[X]
  final class Lane[X]:
    def apply(x: X): To                 // deliver x to this lane — the only way to make a To
  /** a value delivered to a lane, or a refusal; made only by a lane or `unrouted` */
  final class To
  protected def unrouted(why: String): To

  /** THE TABLE — written as a match over what the pattern recognised */
  def table(b: B): To
  /** the same with the verdict's path, for tables that route on WHERE it was read */
  def table(b: B, by: Path): To = table(b)

  def lanes: Vector[Lane[?]]
  def decide(a: A): Either[Rejected[A, B], String]
  def split[C, O[_], D[_]](c: C)(using Routable.Aux[C, A, O, D]): Split[O, D]
  // Split: out(lane): O[X], out.rejected, out.counts: D[Routed], out.release()

Routed.under(prefix): Int               // a hierarchical count: everything under "rates"
```

## 3. Behavior

Stage 1 — Dispatch over every `Routable` carrier:
- [x] a table written as a `match` routes each recognised document to the
      lane its case names; lane values are typed by the lane (`TypeTest`,
      no cast); `split` over a Vector, a `Bulk` (Chunks, SparkBulk,
      FlinkBulk) and a `Source` answers the same lanes, rejects and counts
- [x] a sub-table is a method; lane names are paths; `Routed.under("rates")`
      sums every lane under that prefix
- [x] `unrouted(why)` rejects with the table's reason; a document the
      PATTERN did not take (declined, `Unclear`) never reaches the table and
      is rejected with the verdict's reasons
- [x] a table that throws for a document (a `MatchError` from a partial
      match over a non-sealed type) rejects THAT document, named, and the
      rest of the input is routed — a table bug costs one document, never
      the stream
- [x] over a `sealed` document type, a missing case is a compile warning
      — verified by a probe, not pinned as a test: `compileErrors` sees
      errors only, never warnings (Results)
- [x] `table(b, by)` can route on the verdict's path (the same `Swap` read
      from FpML or from CDM to different lanes)

Stage 2 — Kafka out: a split's lanes to topics:
- [ ] `KafkaRouting.toTopics(producer)(lane -> topic, …, rejected -> deadLetterTopic)`:
      every lane's values produced to its topic (an encoder per lane),
      rejects with their reasons to the dead-letter topic, offsets committed
      after the chunk is routed (at-least-once, okay-kafka's contract);
      tested on `MockConsumer`/`MockProducer`

Stage 3 — Flink streaming: one pass with side outputs:
- [ ] an unbounded `DataStream` routed by ONE `ProcessFunction` emitting to
      an `OutputTag` per lane — Flink's own idiom for fan-out, no re-read
      (the bounded case is already covered: `FlinkBulk` is a `Bulk`)

Stage 4 — okay2: the same, in Scala 2 (`ClassTag`, no unions; exhaustiveness
the same, sealed traits).

## 4. Decisions

1. **The table is user code, a `match`, not a description.** The
   declarative `Routes` stays for tables built from data (config,
   plugins); `Dispatch` is for tables a person writes and a compiler checks.
2. **`To` is made only by a lane or `unrouted`.** The table cannot return
   "nothing": every case says where, or why not.
3. **A lane's type is checked twice, both cheap and both typed:** at
   compile time by `lane(x: X)`, and on extraction by the lane's `TypeTest`
   (the value crosses a `Bulk` as `Any` — one carrier holds every lane).
   No cast.
4. **Exhaustiveness needs a sealed document type.** okay-fin's `Fin.any`
   answers `Product`, which is not sealed: a table over it gets no
   exhaustiveness. A sealed `Instrument` in the domain is the follow-up
   that turns it on — a domain decision, recorded here, made there.

## 5. Results

Stage 1 (2026-10-01). TestDispatch (6): a two-level sealed domain
(`Instrument` → `Rate` → `Swap | Cds`, `Fx`, `Payment`) through a table
with a sub-table; lanes typed (a `Cds` case delivers the name, a
`String` lane); `under("rates") == 4`; the same table over a Vector,
Chunks and a Source agrees; routing on the path; a table that throws for
payments rejects those two documents, named, and routes the other five.
EXHAUSTIVENESS, by a probe compiled and deleted: a table forgetting `Cds`
gave `[E029] Pattern Match Exhaustivity Warning … It would fail on
pattern case: okay.refine.DispatchFixtures.Cds(_, _)`, and the gate went
RED on it ("no warnings, ever") — a forgotten kind does not build. Not
pinned as a test because munit's `compileErrors` reports errors only.
Found on the way: the verdict's path names the STEPS that took the input,
not a `map`'s name — a table routing on the path matches step names.
FlinkBulk is covered by being a `Bulk` (proven for SparkBulk in
TestSparkRoutes), not by a Flink test of its own.

## 6. Open questions

- Should `Routed` grow a tree (`Routed.tree`) rather than `under(prefix)`?
  `under` first; a tree when a page shows it.

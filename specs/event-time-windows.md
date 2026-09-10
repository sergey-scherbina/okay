# Event-time windows — the operator the core did not have

## Overview

specs/aggregators.md already carries the arithmetic (`Aggregator`:
init/add/merge/present) and a sliding window by ELEMENT COUNT over a
`Group` (aging data is subtracted by the inverse). Neither is the
operator a stream job actually asks for. What docs/benchmarks.md §20
measured — the same job on okay and on Apache Flink — is that Flink's
side of stage 2 is one line:

```scala
.keyBy(route).window(TumblingEventTimeWindows.of(Duration.ofMinutes(5)))
              .aggregate(fn)
```

and okay's side was fifty lines of keyed panes, a watermark and an
eviction sweep, written inside the benchmark because the core has
nowhere to put them. Every user who windows an event-time stream
writes those fifty lines again, and each of them gets the late-event
rule slightly differently.

This is that operator, in the core, cross-platform, with no `Async`
and no scheduler: windowing is a function of the DATA's time, never
of the machine's clock.

**What it is not.** It is not a scheduler, not a timer service, not
processing-time windows, and not session windows (a gap-based window
merges panes retroactively — a different, larger operator, out of
scope until something asks). It does not reorder: a stream's arrival
order is the caller's business, and out-of-orderness is priced by the
`lateness` parameter alone.

## Interface

```scala
/** one closed window, with the aggregate of everything that fell in it */
final case class Pane[+K, +O](start: Long, end: Long, key: K, value: O)

final class Windows[K, A, Acc, O](
    size: Long, slide: Long, lateness: Long,
    key: A => K, at: A => Long,
    agg: Aggregator[A, Acc, O]):

  /** fold one element into every window it belongs to, then emit the
   * windows its arrival closed */
  def add(a: A)(emit: Pane[K, O] => Unit): Unit

  /** the end of the input closes everything still open */
  def close()(emit: Pane[K, O] => Unit): Unit

  /** the bounded-out-of-orderness watermark: the greatest event time
   * seen minus `lateness`, monotone */
  def watermark: Long

  /** elements that arrived after every window they belong to had
   * closed — dropped, and COUNTED rather than silently folded in */
  def dropped: Long

  /** panes currently open: the operator's live state, which is the
   * memory question §20 left open */
  def live: Int

object Windows:
  def tumbling[K, A, Acc, O](size: Long, lateness: Long)
                            (key: A => K)(at: A => Long)
                            (agg: Aggregator[A, Acc, O]): Windows[K, A, Acc, O]

  def sliding[K, A, Acc, O](size: Long, slide: Long, lateness: Long)
                           (key: A => K)(at: A => Long)
                           (agg: Aggregator[A, Acc, O]): Windows[K, A, Acc, O]

  /** the same operator as a pipeline stage: awaits elements, tells
   * closed panes. Takes the PARAMETERS, never an instance — a Stage is
   * a value and driving it twice must not share one map between the
   * runs (the `Stage.chunked` precedent) */
  def stage[K, A, Acc, O](size: Long, slide: Long, lateness: Long)
                         (key: A => K)(at: A => Long)
                         (agg: Aggregator[A, Acc, O]): Stage[A, Pane[K, O], Unit]
```

## Decisions

- **The watermark is bounded out-of-orderness, and it is the only
  strategy.** `max(seen) - lateness`, monotone. It is what Flink's
  users overwhelmingly pick, it needs no clock, and it makes a run
  REPRODUCIBLE: the same input gives the same output, which is what
  let §20 assert two engines equal rather than close. A punctuated or
  per-partition strategy is a parameter someone can ask for later.
- **A late element is dropped per WINDOW and counted per ELEMENT.**
  With a slide, one element belongs to several windows and some may
  already have closed; the open ones still take it. `dropped` counts
  the elements that landed in NO open window — the same quantity
  Flink's `numLateRecordsDropped` reports. Silently folding a late
  element into a closed window's successor would be the one behaviour
  that cannot be detected downstream.
- **Eviction is a sweep, and only when the watermark crosses a slide
  boundary.** Windows end on multiples of the slide, so between two
  boundaries nothing can close; sweeping per element would be a scan
  per element for no possible emission.
- **Panes live in `HashMap[K, LongMap[Acc]]`** — one hash by key, then
  the window index — so the hot path allocates no tuple and needs no
  dense key. §20's hand-written operator packed `(window, key)` into
  one `Long`, which is faster and only possible for small dense
  integer keys; the general shape is measured against it there rather
  than assumed equal.
- **Emission ORDER inside one sweep is unspecified** and says so:
  panes come out in map-iteration order. A consumer that needs an
  order sorts, or keys by `start`.
- **No `Async`.** The operator is a fold with state; the platform
  contributes nothing. It therefore runs on JVM, JS and Native, and
  its test is in `src/test/scala-cross`.

## Behavior

- [ ] tumbling windows equal a brute-force recompute (group by
      (window, key), aggregate) on generated event-time data
- [ ] sliding windows equal the same brute force, and every element
      that is not near an edge lands in exactly `size / slide` panes
- [ ] a window is emitted exactly once, when the watermark passes its
      end; `close()` emits everything still open and nothing twice
- [ ] an element later than every window it belongs to is dropped, is
      counted in `dropped`, and changes no pane's value
- [ ] an element inside the lateness bound is NOT dropped, however far
      out of arrival order it is
- [ ] `live` falls back to zero after `close()`
- [ ] the `Stage` form gives the same panes as the class, and the SAME
      stage value driven twice gives the same answer (no shared state)
- [ ] the operator is driven through `through` in a pipeline, so a
      window composes with the stages around it

## Out of scope

- session (gap-merging) windows; processing-time windows; allowed
  lateness with re-emission of an already-closed window (Flink's
  `allowedLateness` + side output)
- a timer service, and anything that reads the machine's clock
- distribution: merging panes across processes is `Aggregator.merge`
  and belongs to whoever owns the partitioning (BACKLOG's
  `flink-okay-parallel-lane`)

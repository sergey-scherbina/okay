# writer-covariance — Writer[+W, +A], and the merge-fusion that wasn't

## Overview

Follow-on to channel-merge-regression (docs/benchmarks.md §6, specs/
stm.md Results): that investigation cleared the STM channel of the
1.95x `Source.merge` slowdown (a stale doc baseline, not a
regression) but left an open question — is there a real, safe
optimization available in `Source.merge` at all? This spec is that
investigation's answer: one safe, landed change (`Writer[+W, +A]`),
one measured-and-declined attempt (fusing `Writer.of` + the re-tell
`Source.merge` needs), and two diagnostic findings for whoever picks
this up with a profiler next.

## Decisions

- **`Writer[W, +A]` becomes `Writer[+W, +A]`** — landed. `W` is what
  a `Writer` TELLS; nothing in the type ever consumes a `W` back
  (the one constructor is `Say(w: W) extends Writer[W, Unit]`, a
  producer position), so covariance is the textbook-correct variance
  here, not a workaround. Verified safe empirically, not just by
  inspection: the whole `sbt test` suite (JVM+JS+Native, every
  module) is green unchanged with the flip — row membership
  (`TypeableK`, `<|>`'s class-based split), GADT answer-type recovery
  on `Say`, and every existing `Writer`/`Source` caller all tolerate
  it. `Free[F[+_], A]` itself stays invariant in its row `F`
  (unchanged, deliberate — see `!.widen`'s comment) — covariance at
  the `Writer` level does NOT make `Free[Writer % A + G, X]` a
  subtype of `Free[Writer % (A|B) + G, X]`; the Free-level walk a
  widen needs is still real. What covariance buys is narrower: the
  told OPERATION itself (`Say(w)`) needs no rebuilding when the walk
  reaches it, only the Free nodes around it.
- **`Writer.widen`** (new, alongside `map`) — the identity case of
  `map` priced separately: same walk over Free nodes, but reuses the
  existing `Say` instance via ascription (`sw: Writer[V, Unit]`)
  instead of allocating a fresh `Writer(f(w))`. `Source.merge` uses
  it in place of `Writer.map(s)(identity[A|B])`. Measured neutral
  (below) — landed anyway for being the more honest statement of
  what `merge` actually does (no transform runs), not for the
  (absent) speed gain.
- **`Source.mergeOf` (fusing `Writer.of` + the re-tell into one
  unfold) — MEASURED, DECLINED.** The plan: for the common shape
  `Source.of(x) merge Source.of(y)`, build each side directly at the
  union type `A | B` in one recursive unfold instead of building at
  `A` (`Writer.of`) then re-telling at `A|B` (`widen`) — fewer Free
  nodes allocated, one walk instead of two. Implemented, tested,
  benchmarked (`MergeBenchmark.okaySourceMergeOf` — see Results) —
  and it measured WORSE than the two-pass baseline, not better, so
  it was not shipped. Not reverted blindly: the code was correct and
  fully tested before being pulled, per the numbers below.

## Results

Quiet box, `compare/Jmh/run okay.MergeBenchmark`, 2026-09-02/03:

| lane | before (map+build-then-widen) | `widen` swap in `merge` | `mergeOf` (fused unfold) |
|---|---|---|---|
| `okaySourceMerge` (2x500 elements) | 305-308us | 305-308us (no change) | — |
| `okaySourceMergeOf` | — | — | 336-349us, noisier (±25-28 vs ±3.5-6.7) |

The `widen` swap is measurably neutral — expected, since it removes
one allocation per told element (the rebuilt `Writer(f(w))`) but the
Free-node walk itself, unavoidable while `Free` stays row-invariant,
was never what that allocation cost. The fusion attempt is the more
interesting result: reducing the WALK COUNT (two passes to one)
still didn't help, and made things worse. Two diagnostics narrow down
why, without fully explaining it:

- **The floor**: `Source.of(xs).toLazyList` (ONE source, no
  `Channel.merge`, no fiber, no Async at all) against the same
  `LazyList` consumed natively — 48.9us vs 11.1us for 1000 elements,
  ~38ns/element. That is the honest, load-bearing price of the
  program abstraction itself (building Free `Bind`/`Effect` nodes,
  then `resume`/`Writer.uncons` walking them one step per pull) —
  real, and not what either attempted fix touched.
- **The unexplained remainder**: that 38us accounts for well under
  half of the ~180us gap between `okayChannelMerge` (bare `LazyList`
  through `Channel.merge`, 125-127us) and `okaySourceMerge` (a
  Writer-wrapped source through the same `Channel.merge`, 305-308us)
  — meaning most of the cost is specific to `Channel.merge`
  consuming a WRITER-shaped stream rather than to Source-wrapping in
  general. `mergeOf`'s sources fed `Channel.merge` at row `Pure`
  instead of `Async` (deliberately, expecting `Pure` to be cheaper,
  matching the near-native floor above) and measured WORSE — the
  opposite of that expectation. Something in `Channel.merge`'s own
  `feed`/generic-`Stream`-dispatch (`async(St.uncons(x).runWith)`
  per element) behaves counter-intuitively across `Pure` vs `Async`
  rows, and static reading of the code did not explain it cleanly
  enough to trust a further change on. FILED for whoever continues
  this with a profiler (JMH's built-in `-prof jfr` needs no extra
  install) rather than guessed at further.

Gate: full `sbt test` green (JVM+JS+Native) with `Writer[+W, +A]`
and the `widen` swap; `okay.demo.TestChatDemo`'s one LIVE-model
failure reproduces identically on unmodified master (confirmed
before trusting the gate) — unrelated, pre-existing model-answer
flakiness, not this lane's doing.

## The follow-up: profiled, not guessed (writer-of-resume-fix, 2026-09-03)

Static reading stalled at "most of the cost is Channel.merge-side,
not explained" above. `compare/Jmh/run -prof jfr` (JMH's built-in
profiler, ships with the JDK) on `okaySourceMerge` settled it: of
~120 CPU samples landing in `okay.*` frames, **46 (38%) are two
lines in `!.resume`** (Effects.scala:375-376, the `Bind(Bind(a,h),k)`
and `Bind(Pure(a),k)` rotation cases) — called from `Writer.uncons`
on every pull. The cheap already-normal case (`case a => a`) is
barely sampled; the tree `Writer.of` builds needs REAL rotation work
per element, not a trivial match.

Traced to the idiom: `Writer.of`'s recursive step wraps EVERY
element's work in `okay.pure(()).flatMap: _ => ...` — one
`Bind(Pure(()), k)` node per element, purely for laziness (deferring
`St.uncons(s)` to interpretation time, not construction time — the
method's own doc: "nothing is pulled until consumed"). That wrapper
is load-bearing exactly ONCE, at the top: the RECURSIVE calls
(`of(rest)`) already sit inside the previous step's own
`.flatMap(_ => of(rest))`, which is itself the deferral the next
step needs — wrapping them AGAIN is N-1 redundant rotations for an
N-element source, paid by `resume` on every pull.

**Fix**: split `of` into the public entry (wraps ONCE) and a private
`ofLoop` the recursion calls directly (no re-wrap per element) — the
external laziness contract is unchanged (still nothing pulled before
the first consume), only the redundant per-element `Bind(Pure(()),
k)` nodes are gone.

**Results, re-profiled — a real but partial win.** `Source.of(xs).
toLazyList` alone (no `Channel.merge`): 48.9us -> **40.3us, -18%**,
clean (±0.14 vs ±0.20). `okaySourceMerge`: 305-308us -> **298.9us**,
±2.8 — real but small, ~2-3%. The profiler explains the gap between
those two results: re-profiling `okaySourceMerge` after the fix, the
targeted line (`Bind(Pure(a),k)`, Effects.scala:376) dropped from 28
samples to 5 — the fix worked exactly where aimed — but the OTHER
rotation case (`Bind(Bind(a,h),k)`, line 375) rose from 18 to 33:
without the pure-wrapper acting as a natural reset point between
elements, `widen`'s own recursive Bind-building and `ofLoop`'s
`.flatMap` stack into deeper nested Binds more often, and `resume`
pays a different rotation instead. Net: `resume`'s total share fell
modestly (46 -> 38 of ~205-209 samples), which is why the isolated
floor improved cleanly but the merge total barely moved.

**What actually dominates `okaySourceMerge` now**: at stack-depth 3,
`okay.TRef.modify` is the single most-sampled frame (75 of ~210) —
`Channel.merge`'s own transactional machinery under REAL fiber
contention (two Loom fibers genuinely racing to send, unlike the
single-threaded `ChannelBenchmark.offerReceive1k`/
`sendReceiveProgram1k` the STM lane measured, or even
`concurrentSendReceive1k`'s synthetic two-thread race from channel-
merge-regression, which only showed ~13% contention overhead — real
`Async`-scheduled fiber contention through `feed`/`sch.fork` appears
to cost more than that simpler benchmark could see). This is a
DIFFERENT, deeper investigation than `Writer.of`'s construction
shape — filed, not chased further here.

**Landed**: the `of`/`ofLoop` split — real, verified, zero
regression (full `sbt test` green, JVM+JS+Native), and it is the
correct shape regardless of `Channel.merge`'s own remaining cost.
**Not landed / filed for next**: `Channel.merge`'s `TRef.modify`
cost under genuine multi-fiber contention — needs its own profiler
pass, likely in `ChannelBenchmark` or a new fiber-contention
benchmark closer to `Channel.merge`'s actual `feed`/`sch.fork` shape
than `concurrentSendReceive1k`'s synthetic two-thread race.

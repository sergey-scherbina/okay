# Benchmarks: the cases, the numbers, and the why

Every number here is JMH average time (us/op, lower is better) from
`src/jmh/history.tsv`, which records every run with its date, commit,
host load and protocol — including the experiments that were tried
and REFUTED, so nobody re-runs them blind. The working rule of
specs/interpreter-optimization.md applies throughout: one measurement
is a hypothesis, not a result; lanes quoted together were measured in
the same session on the same (busy, honestly noted) host.

Versions compared: cats-effect 3.5.7, ZIO 2.1.14, kyo 0.16.2,
atnos-eff 7.0.4, fs2 3.10.2, circe 0.14.10.

Run them yourself: `sbt 'Jmh/run .*Fib.*'` (core lanes),
`sbt 'compare/Jmh/run .*Compare.*'` (ecosystem lanes; the heavy
dependencies live only in the compare module),
`sbt 'compare/Jmh/run RagBenchmark'` (retrieval),
`sbt 'compare/Jmh/run EmbeddingBenchmark'` (the embedding representation).

One claim here is not a time at all — structural chunking's advantage
is that a chunk is a WHOLE definition, so it is measured as a
percentage by `okay-rag`'s `TestChunkQuality` and reported in §11
beside the microseconds it costs.

---

## 1. Bind chain — 10k left-nested flatMaps, built and run

| **Okay Eager** | kyo | **Okay Cont** | **Okay Free** | cats Free | cats Eval | cats IO | ZIO | atnos |
|---|---|---|---|---|---|---|---|---|
| **4.8** | 58 | **89** | **95** | 129 | 136 | 153 | 181 | 260 |

**What it measures.** The raw cost of the monadic plumbing itself:
build a 10 000-step flatMap chain by foldLeft (the WORST, left-nested
shape), then run it. No effects, no I/O — just who pays how much per
bind.

**Why Okay's numbers.** Three encodings, one interface, chosen per
program:

- `Free` (the tree) stays fast because `fold` REBALANCES left-nested
  binds with tail-recursive rotations — the "reflection without
  remorse" problem solved with two lines of pattern match instead of
  a type-aligned queue (measured: stepping one-by-one costs only ~8%
  over bulk, so the queue is unneeded, with evidence).
- `Cont` is the same discipline one level down: a defunctionalized
  continuation monad whose runner is one tail-recursive loop, plus
  closure FUSION up to a depth budget (flatMap/map merge into the
  Shift closure while shallow, spill to Bind after — kept after
  measuring −10..28% across generator lanes).
- `Eager` is the kyo trick as an OPT-IN encoding (`import
  Eager.given`): pure binds apply at construction, so "running" the
  chain is running nothing. 12x under kyo on this lane — with kyo's
  hazards STATED, not hidden (see the asterisk below). (A same-day
  regression and fix, both filed: `casts-encapsulated` centralized
  the encoding's two casts into one `fold` taking ordinary closures
  — 5.1 -> 17.6us, 3.45x, an unconditional closure per branch plus a
  virtual dispatch through it. eager-dispatch-regression made `fold`
  `inline` with `inline` value/tree parameters instead: the casts
  stay in the one function, but each call site's branch compiles
  in-place with nothing built for the arm not taken — 4.8, matching
  the pre-regression number and then some. specs/eager.md Decisions.)

**Why the competitors' numbers.** cats Free lacks the rotation (its
fold re-associates by allocation); cats IO and ZIO run every bind
through a fiber runtime — shift checks, trace buffers, interruption
machinery — pure overhead when nothing suspends; atnos-eff pays the
open-union tagging of classic freer on every operation.

**The asterisk that reframes the whole table.** kyo's 58 is
FRONT-LOADED: construction evaluates. Build-only lanes
(LazinessBenchmark): Okay 13.8 (15% of its full cost), cats IO 26.9,
kyo 58.5 — **101%**: for kyo, construction IS the computation.
Building an infinite pure-recursive program runs 513 iterations
uninvited (their safepoint budget); an exception in a flatMap lambda
throws at BUILD time; an effect runs once-at-build, so the value is
not reusable as a description (all three demonstrated in
compare/TestLaziness). In build-many-run-few scenarios Okay is 4.2x
cheaper; and Okay offers the same trade EXPLICITLY as `Eager`,
per-program, instead of as the only semantics.

## 1b. Direct syntax — the same 10k binds, written as code

| | flatMap (hand) | direct | the syntax costs |
|---|---|---|---|
| **Okay** (while+var) | **95** | **101** | **1.06x — matched** |
| **Okay** (recursion) | **95** | **55** | **0.58x — faster** |
| kyo-direct (recursion) | 56 | 157 | 2.8x |
| zio-direct (recursion) | 187 | **120** | **0.64x — faster** |

**What it measures.** Each ecosystem's first-party direct form
against its own hand-written flatMap chain, same shape, same run
(cats has no first-party direct form and is absent honestly). The
PAIR's ratio is the price of the syntax; absolute columns compare
runtimes as usual. The Okay legs are the direct-tail-fusion
re-measure (quiet box, flatMap baseline 95.1±9.6 revalidating the
§1 chain, while 101.1±5.4, recursion re-isolated 54.9±0.7 after a
same-run fork spike); the competitor rows carry over from the
bench-direct run of the same suite — their code did not change.

**Why Okay's numbers.** The macro emits the monad's own plain
flatMap binds (direct-flatmap-emission, specs/direct-macro.md; the
first cut emitted Monadic's Cont layer, priced right here at 3.3x
and retired — this table is what filed that optimization). The
recursion form is FASTER than the hand-written chain by the same
mechanism that credits zio-direct below: the macro emits
right-nested binds where the hand-written foldLeft builds
left-nested ones the Free interpreter must reassociate — level with
kyo's hand-written chain. The while+var form landed at 2.0x on that
same measurement — the loop sequencer paid one flatMap per
iteration on top of the step's own bind — and direct-tail-fusion
(specs/direct-macro.md Decisions) closed it: the loop body compiles
against an explicit tail (`loop()`) so the sequencing bind merges
into the body's own last bind, one per iteration, the hand-written
recursion shape. The receipt is this row: 1.06x, matched within
measurement noise.

**Why the competitors' numbers.** kyo's defer pays 2.8x over its own
eager chain — the runtime-layer price Okay's first cut also paid.
zio-direct is the surprise the table exists to catch: its defer is
FASTER than the naive hand-written foldLeft chain, because the macro
emits a better-shaped program than left-nested binds — a genuinely
good macro, credited.

**The expressiveness line, measured by refusal.** Both competitors
FORBID `var` inside their blocks (kyo as a design stance, stated in
its error; zio-direct bans `var`/`def`/`class` alike), and kyo
refuses nested marks in one expression. The imperative direct form —
`var x = 0; while i < N do x = step(x).reflect` — compiles only in
Okay (direct-loops), which is why Okay is measured in both
spellings and the competitors in the one they allow.

## 2. Reader — 10k asks · Writer — 10k tells

| Reader | **Okay ctx direct** | **Okay ctx instance** | **Okay row** | ZIO | cats Kleisli | atnos | kyo Env |
|---|---|---|---|---|---|---|---|
| right-nested (recursion) | **0.3** | **46**† | **79** | | | | 291 |
| left-nested (foldLeft) | | | **124** | 245 | 328 | 3123 | 362 099* |

(† the ctx instance is measured at 1 000 binds — 4.6 µs, scaled ×10
here — because the chain is stack-bounded at ~2-5k binds; see the
paragraph below.)

| Writer | **Okay** | cats WriterT/Chain | atnos | kyo Emit |
|---|---|---|---|---|
| right-nested (recursion) | **163** | | | 215 |
| left-nested (foldLeft) | **209** | 1250 | 4054 | 364 313* |

**Two shapes, deliberately (kyo-fair-lanes, 2026-09-02).** The
foldLeft build — `(1 to N).foldLeft(ask)((m, _) => m.flatMap(_ =>
ask))` — nests LEFT: `((ask >>= f) >>= f) >>= f`. A for-comprehension,
a direct block or a recursive definition nests RIGHT: `ask >>= (_ =>
ask >>= (_ => ...))`. Both are the same N handled operations; only
the tree differs. The right-nested row is the everyday shape and the
number to quote; the left-nested row is the stress shape, kept
because it is where a freer-style interpreter's quadratic trap
fires (see the starred paragraph below). ZIO, cats and atnos have
only the foldLeft lane; all three are linear in it, so their number
stands for both shapes. Session: one run on a busy host (load
~7), all lanes together; the earlier session's numbers (110 / 202
for Okay, 362 756 / 342 761 for kyo) were the same shape and are
in history.tsv.

**What they measure.** Handled operations at volume — the everyday
shape of effectful code.

**Why Okay's numbers.** Reader runs at RELAY speed: a
tail-resumptive handler must resume exactly once, so handling is one
tail-recursive loop — no continuation capture, no allocation per ask
(relay measured 1.45x over the general handler on forwarding-heavy
work). Writer's `tell` is ZERO allocation: the operation is an
opaque IDENTITY signature — telling w IS the value w, no wrapper
node; the handler is a bespoke tail loop into a Vector.

**The ctx-function reader** (capabilities.md), measured into the
same case (quiet box, 2026-09-02): direct style — 10k ambient
reads via `wire[Int]` under one `provide` — runs at **0.31 µs**,
~350x below the relay, because there is nothing to interpret: a
read is a parameter access, the "monad" is gone at elaboration. The
same chain built THROUGH the `ctxMonad` instance (N flatMaps, each
literally `f(fb)`) measures **4.3 µs per 1 000 binds** — ~2.5x
faster than the relay per bind — but is stack-bounded: no trampoline,
~2-5k binds on a default stack, and it must be built by recursion
(a mutating-var build self-captures — E22 in
specs/context-functions.md). Width is the instance's domain
(traverse over a page of readers); depth belongs to the row Reader
above.

**Why the competitors' numbers.** cats WriterT allocates a
tuple-in-monad per tell; Chain helps, the wrapping doesn't. atnos
pays union tagging again. kyo in its natural shape is close: Env
3.7x over the Okay row Reader, Emit at parity with the foldLeft
Okay Writer and 1.3x over its recursive form.

**The starred kyo numbers are the left-nested trap, NOT kyo's
price.** Verified in kyo 0.16.2's source (kernel `Pending.scala`,
`ArrowEffect.handleLoop`): `map` over a suspended computation wraps
it in a `KyoContinue` whose `apply` re-applies the INNER
continuation and re-wraps the result; the handle loop never
reassociates. On a left-nested chain every resume therefore walks
the rest of the chain — O(N²), measured ×109 from N=1k to N=10k
(3.6 ms → 394 ms). The same chain nested right resumes in O(1) per
op, and kyo is linear. Okay is linear in BOTH shapes because `fold`
rotates left-nested Binds tail-recursively before stepping (the
rotation's cost is the 124-vs-79 / 209-vs-163 difference above).
The lane stays as the rotation's value made visible; an earlier
version of this page quoted the starred numbers as "~1000x off"
without the right-nested row beside them — that was the pathology
measured, not the library.

## 3. Choice — 2^13 branches, all collected

| List (floor) | **Okay** | kyo | atnos |
|---|---|---|---|
| 580 | **1603** | 3834 | 5392 |

The one handler that is genuinely MULTI-SHOT: the continuation runs
once per alternative. Okay's `runChoice` folds `k(x)` over the
alternatives on the Free tree directly; 2.8x from the bare-List
floor is the price of programs-as-values here, and it still halves
kyo. (Multi-shot is where one-shot-optimized runtimes can't follow:
this handler cannot be expressed with relay OR exceptions.)

## 4. Fork/join — 100 trivial fibers

| raw Loom (floor) | kyo | **Okay** | ZIO | cats IO |
|---|---|---|---|---|
| 21 | 25 | **29** | 50 | 140 |

**Why Okay's number.** There is almost no Okay here — that is the
design. A fiber IS a virtual thread; spawn is `Thread.startVirtualThread`,
join parks. No fiber runtime of our own means nothing added over the
floor but 8us of bookkeeping. ZIO and cats IO pay their own
schedulers, run-loops and interruption protocols; kyo sits close to
the metal too (its scheduler is excellent) — we simply refuse to
compete by NOT having one.

## 5. Stream pipeline — map/filter/take(1000)/sum

| Iterator (floor) | **Staged** | **Okay chunked** | **Okay elements** | Okay iterator | Okay LazyList | kyo Stream.range | kyo singleton | ZIO | fs2 |
|---|---|---|---|---|---|---|---|---|---|
| 14 | **1.6*** | **16.9** | **23.6** | 53 | 143 | 64‡ | 239 | 692 | 1410 |

(*Staged and its same-run floor of 19.3 come from a different
session; the rest is one session. The 12x-under-the-floor number is
real: see below. ‡kyo `Stream.range` — kyo's own chunked source,
4096-element chunks, the lane a kyo user would write for this
pipeline — was added by kyo-fair-lanes (2026-09-02) and measured in
its own session: 64 against a same-run Iterator floor of 15.3, Okay
chunked 12.4/23.5 and the hand-emitted singleton kyo lane at 330.
Ratios to the floor are what carry across sessions: kyo chunked
4.2x, Okay chunked 0.8-1.5x, kyo singleton 22x.)

**What it measures.** The bread-and-butter stream pipeline in each
library's fastest mode.

**Why Okay's numbers, mode by mode.** This table is one design
principle at four price points:

- `toLazyList` (143): the memoized, re-observable bridge — you pay
  for the caching.
- `.iterator` (53): linear, fused, consume-once — a specialized
  tree-walk with a mutable cursor, no Option/tuple per element
  (measured −44% over the generic unfold when introduced).
- Chunks (23.6 / 16.9): the tree steps once per CHUNK; an element
  costs an array index. Transformers are chunk-in/chunk-out array
  passes; what remains over the Iterator floor is the Free-node
  stepping per chunk — the price of programs-as-values, amortized 64
  ways.
- `Staged` (1.6): when the pipeline's shape is known where it is
  written, inline combinators beta-reduce the WHOLE pipeline into
  one while-loop with every lambda inlined — no operator dispatch,
  no iterator protocol, and `take(1000)` exits by a plain boolean.
  UNDER the Iterator floor because Iterator itself pays virtual
  `hasNext`/`next` per element and the fused loop pays nothing.

**Why the competitors' numbers.** kyo, ZIO and fs2 all run
STREAMING RUNTIMES here: every element (or worst-case singleton
chunk) crosses effect-dispatch machinery. fs2's pull-model
Pull/Chunk plumbing is built for concurrency and resource safety,
priced per element; ZStream similarly. The `iterate`/singleton-emit
lanes are their worst case (a per-element source), noted as such;
kyo's chunked `Stream.range` lane shows what the runtime costs when
the source is chunked the way its author intended — 5x under its
singleton lane, and still 4x from the floor where Okay's chunks sit
at ~1x. (fs2 and ZStream have chunked `range` sources too; they are
not yet measured — a same-session sweep of all three is the next
step, filed.)

## 6. Merge — two 500-element streams by readiness

| **Okay chunked** | ZIO | Okay elementwise | fs2 |
|---|---|---|---|
| **10.7** | 45 | **308** | 8878 |

Readiness-merge is what zip and ++ cannot express: a fiber per
source feeds one channel, the loser of every race simply arrives
later. Chunking the STREAM (not the queue — a chunked-queue variant
was tried and REFUTED, it's in history.tsv) beat ZIO's own
chunk-aware merge 3.2x. fs2's number is its worst case (singleton
elements through its concurrency machinery), stated as such.

**Okay elementwise moved from 158 to 308 — investigated, NOT a
regression** (channel-merge-regression, 2026-09-02). A full-sweep
re-run flagged this as a 1.95x regression and the STM Channel
rewrite landed the same day (`stm`, `stm-slot-stamped`,
`cast-free-sim`) was the obvious suspect. It wasn't: `TRef.modify`'s
generic `waiters` check (unused by Channel, which has its own
receivers/senders queues) accounts for at most ~13% under real
two-fiber contention (`ChannelBenchmark.concurrentSendReceive1k`,
new — the STM lane's own benchmarks were single-threaded and never
measured contention at all). The rigorous check: the exact same
benchmark, same run, one on today's HEAD (308 ±19), one checked out
at the LAST pre-STM commit (channel-cas, 500efb7) with the CAS-only
Channel (290 ±11) — within noise of each other. Whatever moved 158
to ~290-308 predates every commit from today; it is not attributable
to any landing here. (§4 Fork/join's uniform 30-45% drop across all
four libraries in the same full-sweep session is the same class of
finding: session-to-session environment, not code.) The `Okay
elementwise`/`Okay chunked` numbers above are the current honest
baseline; 158 is retired.

**Follow-on: is there a real optimization here at all?**
(writer-covariance, specs/writer-covariance.md) `Writer[W, +A]`
became `Writer[+W, +A]` — the correct variance (`W` is only ever
told, never consumed) — and `Source.merge` swapped `Writer.map(s)
(identity[A|B])` for a `Writer.widen` that reuses the told operation
instead of rebuilding it. Measured neutral: 305-308us either way,
because that per-element allocation was never the dominant cost. A
further attempt — fusing the source's own construction with the
re-tell into one unfold — was implemented, tested, and measured
WORSE (336-349us, noisier) and was not shipped. Two diagnostics for
whoever profiles this next: a bare `Source` with no merge at all
already costs 48.9us against a native `LazyList`'s 11.1us for the
same 1000 elements (~38ns/element, the honest price of the program
abstraction) — under half the ~180us gap this section's numbers
show, so most of the cost is specific to how `Channel.merge` pulls a
Writer-shaped stream, not to Source-wrapping in general.

**Profiled (writer-of-resume-fix): the 38ns/element floor, explained
and partly closed.** `-prof jfr` on `okaySourceMerge` found 38% of
its CPU samples in two lines of `!.resume` (Effects.scala — the
tailrec rotation that normalizes a Free tree before it can be read),
called from `Writer.uncons` every pull. Traced to `Writer.of`
re-wrapping EVERY recursive step in `pure(()).flatMap` for laziness
— load-bearing once, at the top, since each recursive call already
sits inside the previous step's own `flatMap`. Splitting `of` into a
one-wrap entry and an un-wrapped `ofLoop` closed the bare-`Source`
floor 18% (48.9 -> 40.3us) but moved `okaySourceMerge` only ~2-3%
(305 -> 299us) — re-profiling showed why: the ROTATION CASE `resume`
pays shifted (one line dropped 28 samples to 5, another rose 18 to
33 without the pure-wrapper's reset point), and a different frame —
`TRef.modify`, `Channel.merge`'s own transaction machinery under
REAL multi-fiber contention — now dominates the merge path's deeper
samples (75 of ~210). Landed anyway: real, verified, zero-regression
improvement to `Writer.of` on its own terms. The `Channel.merge`
contention cost is a separate, deeper investigation, filed not
chased.

**Scaled (merge-scaling-shape): linear, so the story ends here.**
The filed follow-ups closed one after another — the `Queue` in
`Channel.State` measured and declined (two replacements, neither
wins), the `TRef.modify` contention measured and explained as a
symptom rather than a cost (its retry is a spin, never a park). What
was left was `!.resume`'s rotation itself, whose textbook fix
(reflection without remorse — a type-aligned continuation queue in
place of the binary Bind tree) is a kernel rewrite touching 42 sites
through `resume`'s three-form invariant. That technique removes
QUADRATIC behaviour on left-nested binds and does nothing for a
constant per-element cost, so `ScalingBenchmark` swept `n` and read
the numbers PER ELEMENT:

| per element | 500 el | 1000 el | 2000 el | 4000 el |
|---|---|---|---|---|
| `rawLazyListDrain` (control) | 11.3ns | 11.4 | 11.0 | 10.6 |
| `sourceSingleDrain` | 41.2ns | 39.6 | 41.5 | 40.6 |
| `channelMerge` | 142.3ns | 121.9 | 127.9 | 131.8 |
| `sourceMerge` | 303.5ns | 299.7 | 300.7 | 291.6 |

Flat everywhere across an 8x range — the tree is linear, there is no
quadratic to remove, and the rewrite has no measured justification.
What the sweep exposes instead: the Writer layer costs ~30ns per
element alone (41 vs the control's 11) and ~160ns per element inside
the merge (292 vs `channelMerge`'s 132) — the same layer, ~5x more
expensive in the contended shape. The lever is therefore fewer
interpretation steps inside the contended region, not a cheaper
step — which is `Chunks.merge` (one queue operation per chunk),
already in the library and already measured at 10.7us for 2x500
against `sourceMerge`'s 299.7us.

**The last lever, tried and negative (free-row-variance).**
`Source.merge` calls `Writer.widen` per source, and widen rebuilds
every Free node — because `Free` is invariant in its row. That
invariance turns out to be removable (`enum Free[+F[+_], A]` passes
the variance check; the row subtyping then holds at concrete rows),
so the pass could in principle become a coercion. Measured before
adopting: widen costs 7.4–10.4ns/element in ISOLATION (20.4 -> 24.1us
at 500 el, 77.5 -> 98.3us at 2000), but the same merge built WITHOUT
it, at one element type so widen is the only difference, is *slower* —
1141.8 ±6.7 against 1202.6 ±14.6us on 2x2000, and 1162.4 ±11.4
against 1240.1 ±10.1 on a repeat, bars non-overlapping both times.
The walk is also a NORMALIZATION: it hands `feed` an already
head-normal tree, so the rotation it saves is not paid per pull
inside the contended region. Declined; `Free` stays invariant as a
measured choice. `WidenBenchmark` guards the conclusion.

**And then the thing that actually works (source-merge-chunked).**
Four lanes had refuted the COST side of the per-element merge (the
queue's data structure, the retry rate, the kernel's tree shape, the
row's variance). Profiling it one more time, this time attributing
every frame rather than the two already suspected, said why: 71% of
samples sit in the per-element channel TRANSACTION — 33% the CAS
itself, 19% the immutable Queue rebuilt around it, 19% the rotation
`resume` does per pull. Nothing there is cheaper than it is; there
are simply too many of them. `merge`'s `chunked = true` divides the
count by a fixed chunk size — the same sources, the same channel, one
transaction per 16 elements — on 2x2000:

| | 2x2000 | |
|---|---|---|
| `chunked = false` | 1163.4 ±21.2 | readiness, exact |
| `chunked = true`, capacity 64 | **443.6 ±23.8** | 2.6x, same 64-element budget |
| `chunked = true`, capacity 1024 | **226.5 ±1.2** | 5.1x |

The two knobs stay orthogonal because `capacity` counts ELEMENTS
either way: the channel gets `capacity / 16` slots when chunking, so
turning the flag on alone buys 2.6x while holding the same amount of
data, and the rest is bought explicitly with memory. 226.5 is the
ceiling — a hand-built chunk pipeline measures 223.2.

It is OFF by default, and not out of politeness: on its own `chunked`
emits when a chunk is full or when its input ends, with no flush on
time. On the slow or unending sources this merge exists for — a
model's tokens, a live feed — an element would wait for 15 others
that may never come. `flushAfter = Some(millis)` bounds that wait and
costs nothing standing: 230.0 ±3.3 with a 30-second window against
230.1 ±0.9 without, the same number. `Flush.now` is the exact form of
the same thing — a boundary the producer states rather than a timer
guesses — and it too costs the ordinary path nothing (220.5 ±0.7
against 219.6 ±1.5), but only after a refuted tidier design: routing
BOTH row shapes through the one flushing walk measured 244.3 ±15.2,
11% worse, for one extra tree rebuild per source and one extra row
split per element. Two walks sharing the accumulation is what the
numbers bought. The default path is unaffected
either way — 307.2 ±2.0 against master's 310.2 ±18.4 the same hour.

**The receive side, which needed no trade at all (channel-drain).**
Chunking cuts the per-element transaction count on the SEND side, and
pays for it by delaying an element that could have gone now — which
is why it is opt-in. The consumer's half of that same 71% has no such
price: what is already in the buffer is already late, so taking up to
64 of them under one CAS hands over exactly the same elements in
exactly the same order. `Channel.receiveMany` does that, and the
merge's output reads through a carrier that serves from the batch and
touches the channel only when it runs out. Measured on 2x2000 in one
window: **828.3 ±11.3 against 1180.5 ±11.8, 30% faster**, with no
flag and no semantic change — the largest single win on the
per-element path in this whole arc, and the one that needed no
permission from the caller.

`Channel.buffer` is the other per-element channel consumer, and had
never been benchmarked at all. It inherits the same win through the
same carrier — `Channel.buffer(1024)(xs).drained` reads at one
transaction per 64 elements: **437.2 ±2.6 against 1068.5 ±18.4, 2.4x**.
It is an explicit `.drained` rather than the default because the
plain `Stream[Channel, Async]` instance has nowhere to keep a batch —
its carrier IS the channel, and keeping one inside would hand a
second consumer elements the first had already taken.

## 6b. Chunking and flushing — the three shapes, three libraries, and one methodology bug this section itself had

Merging two streams has three shapes worth measuring, and fs2 and ZIO
have a direct spelling of each: elementwise, in chunks, and in chunks
with a TIME bound on a partial one — okay's `flushAfter`, fs2's
`groupWithin`, ZIO's `groupedWithin`. Every lane folds the same
2x2000 elements to one Long.

The first cut of this table read okay's per-element `Source.merge`
against `ZStream`'s own chunk-of-4096 default and called the row
"elementwise" for both. That is not a comparison — `ZStream` has NO
per-element representation to measure; its "element" is a slot in an
array, and every operation is a loop over the array. Naming ZIO's
number 59.1us "elementwise" priced a chunked walk against a
per-element one and made okay look 20x slower than it is. The fix is
not to omit the row — both libraries CAN be forced to work one
element at a time (`ZStream.range(chunkSize = 1)`, fs2's `.unchunk`)
— it is to ask every library the same question:

| 2x2000 elements | okay | ZIO | fs2 | reads as |
|---|---|---|---|---|
| chunk-native (each library's own default) | **22.3 ±0.3** | 126.2 ±1.0 | 44443 ±2359 | comparison |
| chunked at a matched size (16) | 223.7 ±5.0 | **127.2 ±2.2** | 38508 ±403 | comparison |
| chunked + timed flush | **244.3 ±3.5** | 4907 ±98 | 54270 ±1790 | comparison |
| per-element (chunk of one, forced on ZIO/fs2) | 824.9 ±6.9 | 10032.5 ±111.0 | 36030 ±1204 | diagnostic |

**The matched-size row is the same mismatch a fourth time, corrected
2026-09-05.** It priced our `Source.chunked(k).merge(...).unchunked`
— chunking applied to a PER-ELEMENT source — against `ZStream`, which
is chunk-native by construction. Our chunk-native equivalent is
`Chunks`, not `Source.chunked`, and swept across the same sizes it
wins at every one:

| chunk size | okay `Chunks`-native | okay `Source` fused | okay `Source` composed | ZIO |
|---|---|---|---|---|
| 16 | **29.4 ±3.8** | 275.4 ±91.0 | 416.4 ±71.0 | 126.4 ±2.7 |
| 256 | **31.8 ±7.3** | 216.3 ±14.2 | 333.2 ±142.2 | 72.4 ±2.1 |
| 1024 | **24.8 ±2.0** | 217.2 ±4.3 | 409.4 ±8.7 | 68.5 ±2.3 |

Flat in the chunk size, and 2.2x to 4.3x ahead. The two `Source`
lanes stay and read as DIAGNOSTIC, not comparison: what they measure
is real — the cost of chunking a per-element stream — but `ZStream`
has no per-element representation to chunk, so there is nothing on
the other side to put them beside.

**A retraction belongs here.** An earlier run of the composed lane
read 216.8 → 301.7 → 434.7 across those sizes, and it was written up
as "our chunking gets worse as the chunk grows, the opposite of what
chunking is for", with an explanation involving boxed arrays and the
`through` trampoline. The re-run reads 416 → 333 → 409 — not monotone
— and both runs carry error bars of ±44 to ±142 on a difference of
that size. There was no trend; there was noise with a story attached
to it. The `Chunks`-native lane, whose bars are ±2 to ±7, is flat,
which is what a correct chunking path looks like.

**Three of these rows compare and one diagnoses, and an earlier draft
read all four as a scoreboard.** It opened *"forced onto equal
footing, okay is ahead in every shape"* — which its own matched-16 row
contradicts, and which the paragraph beneath it then walked back.
Rewritten in `idiomatic-headline-honest` (2026-09-05) after §6c named
this section as the first of three places one mistake appeared. No
number here was re-measured and none changed: the reading was what was
wrong.

**Chunk-native is the row that compares.** Each library doing what its
own users would write: **22.3 against 126.2, okay 5.7x ahead**.
Neither builds a node per element, and the ~100ns/element gap is
genuinely `Chunks.merge` against `ZStream.merge` rather than an
artefact of which library got its home field.

**At a matched chunk of 16, ZIO leads** — 127.2 against 223.7 — a
real result, unpacked two paragraphs down.

**Timed flush is okay's by 20x** (244.3 against 4907), which says
more about `groupedWithin` than about either representation.

**The forced per-element row is a DIAGNOSTIC, not a win.** Nobody
writes `ZStream(chunkSize = 1)`: it wraps every element in a one-slot
array and pays the chunk machinery on top, so 824.9-against-10032
measures what that mode costs a library with no per-element
representation — not what okay beats `ZStream` at. It earns its place
because a genuinely one-at-a-time source exists (LLM tokens, SSE) and
someone has to pay that cost; it does not earn a "12x ahead"
headline. §6c reaches the same chunk-of-one mechanism through
`ZStream.unfold`, where it is ZIO's own and nobody's forcing — a
single stream rather than a merge, so not the same workload — and the
gap there is 3x.

**And read §6c before quoting any of this**, because the same
methodology bug lived there too and moved a number by 14x when it was
fixed: the collection row said "ZIO 3x ahead" while pairing
`ZStream.fromIterable` against our per-element surface plus a
memoising bridge; paired like-for-like it is **okay 4.5x ahead**. The
lesson that section draws — every lane names its granularity and
whether it memoises — is the one this table's four rows are trying to
carry in a column instead.

fs2 is 30-1600x behind both in every shape and is stated as such
rather than compared row by row.

**The row ZIO wins is a size we do not need to match — which is a
reason, not an excuse.**
Chunking okay's per-element `Source` at a size ZIO would use as its
OWN default (rather than forcing ZIO down to ours) is the only row
left where ZIO leads, and even there the lead does not grow the way
an earlier draft of this section claimed — see chunk-size-
representation, which tried to close it and found the size-curve's
premise wrong. The honest reading: `chunked` at matched sizes prices
what a per-element `Source` costs before chunking, against a stream
that never had elements to price. Where that per-element cost has to
be paid — a live source whose elements arrive one at a time and
cannot be pre-chunked — reach for `Source.range`/`chunked`; where it
does not, `Chunks` is both native to the data and ahead of ZIO's own
native path.

**Why not just replace `Source` with `Chunks` entirely?** The
operator's question, and the honest answer is that we already made
the ZIO-shaped representation — `Chunks`, array-native, 5.7x ahead of
`ZStream`'s own default — and keep `Source` separately on purpose,
for a reason this section can now put a number on rather than assert.

An array-of-chunks representation pays a chunk allocation per
PRODUCTION regardless of size. Forced to size 1 — what a genuinely
one-at-a-time live source (LLM tokens, SSE) would force on it —
`Chunks.merge` costs **780.7 ±14.1**, a 33x collapse from its own
64-element default (23.5 ±0.2). That is not a ZIO implementation
quirk: it is structural to representing a stream as arrays, and
`Chunks` pays it exactly as `ZStream` does.

The number that decides the question: at that same forced size,
`Chunks(1)` (780.7 ±14.1) and `Source.merge` (819.6 ±4.8) are within
a few percent of each other — genuinely per-element load collapses
BOTH of okay's representations to the same floor, because that floor
is the cost of per-element semantics itself, not a property either
data structure adds or removes. `ZStream` forced the same way costs
**9984.9 ±125.0** — 12.2x worse than either — because it has no
second representation to fall back to; the array-native shape is all
it has, and there is nowhere else for the pathology to go.

So `Source` is not an unmerged duplicate of `Chunks`; it is what
keeps genuinely per-element work off the 33x cliff that any
array-native representation, ours included, pays for the same reason
ZIO's does.

**Two more levers found by profiling the per-element path, both
small and both kept.** `Stage.chunked` became `inline`: `ChunkBuf`
allocates an unboxed array when the element type is concrete at the
point of expansion, and a plain `def` hid that type behind an
abstract `T`, boxing every element on the way into a chunk — the
profiler named `boxToLong` among its frames. Measured, same window:
197.5 ±0.8 against 206.3 ±3.2, bars non-overlapping. `Source.range`
generates a half-open range directly rather than walking a
`LazyList`, cutting a cell allocation the profiler also named. It
helps where the cost of that cell is not already amortised —
per-element, 646.8 ±23.9 against 829.5 ±5.1 (-22%) — and slightly
HURTS where it is: chunked, 214.4 ±2.0 against 197.5 ±0.8, because the
`Bind`-chain `range` builds pays per node exactly where `LazyList`'s
per-cell cost was already spread across 16 elements sharing one
transaction. Kept as the specialised choice for a per-element
producer, not a universal replacement for `Source.of`.

**Where okay wins outright: the timed flush.** A bound on how long a
partial chunk may wait is what makes chunking safe on a live source,
and it is the shape both competitors are worst at — ZIO's
`groupedWithin` costs 37x its own plain `grouped` (4907 against 127),
fs2's `groupWithin` 1.4x its own `chunkN`. okay's `flushAfter` costs
9% over its own chunked merge (244.3 against 223.7), because the
flusher is one sleeping fiber beside the feed rather than machinery
in the per-element path. Against ZIO that is **20x**, against fs2
**222x**.

**The stack-safety bug this found (chunk-stack-safety).** Writing the
edge cases turned up an overflow that predates all of it: `through`
drives a stage by calling into the producer and back, and only an
EMISSION goes through a `flatMap` that lets the stack unwind, so a
stage that accumulates recurses once per element. `chunked(16)` never
came near it; `chunked(4096)` over 4000 elements blew the stack, as
did any chunk a short stream cannot fill. Reproduced on b8c65c7 with
`through` and `Stage.chunked` alone. Fixed with a budget rather than
an unconditional defer — past `PullBudget` the loop answers with a
deferred program instead of recursing, one extra node per 256
elements rather than per element, since per-element deferral is
exactly what writer-of-resume-fix removed from this same path. Free
at the sizes anyone uses: 222.3 ±4.0 after against 224.5 ±3.7 before.

## 6c. Idiomatic API — what each library's own surface offers, not a forced mode

The chunk-size-one lane compared okay against `ZStream` forced to
`chunkSize = 1`, which turned out to be exactly `ZStream.unfold`'s own
mechanism (`Chunk.single(a)` per step, verified in zio-streams 2.1.14
sources) — a real number under the wrong name. This section drops the
forcing and asks what each library's OWN idiomatic surface gives you,
paired axis by axis, N=4000.

**Sending a collection, reading the whole stream.** This row read
"ZIO 3x ahead" for a long time, and it was measuring the wrong thing.

`ZStream.fromIterable(list).runSum` makes ONE chunk of the collection
and walks the array. It was paired against
`Source.of(list).toLazyList.foldLeft` — our per-element surface, plus
a bridge that allocates a `LazyList` cell per element and buys
re-observability the zio lane never pays for. Mismatched on
granularity AND memoisation, at once.

Re-measured 2026-09-05 with the like-for-like partner, which existed
in the library all along and simply was not used:

| lane | us/op |
|---|---|
| `Chunks.foldLeft(Chunks.fromIterator(list.iterator, N))` | **11.0 ±0.5** |
| `ZStream.fromIterable(list).runSum` | 49.6 ±2.8 |
| `Source.of(list).toLazyList.foldLeft` (kept, ours only) | 154.1 ±17.5 |

**4.5x ahead, where the mismatched row said 3.1x behind.** The
elementwise lane stays in the table because it measures a real thing —
what our per-element surface with a memoising bridge costs — but it is
not what `ZStream.fromIterable` does and never was.

Four control lanes in the same run agree with their recorded values
(`okayStep_elem_lazyList` 90.3 against 90.4, `zioCollection_chunk_
runSum` 49.6 against 49.2, `zioStep_elem_runSum` 280.0 against 275.3,
`zioCollectionForeach` 85.5 against 85.3), so the flip is the pairing
and not the weather.

**This is the third appearance of one mistake**, after §6b (our
per-element merge priced against `ZStream`'s chunk-of-4096, both rows
called "elementwise") and the guarantee table (§15). Three is a
pattern, so the fix is a rule rather than another correction: every
lane in `IdiomaticApiBenchmark` now carries its properties in its
NAME — `_elem_` or `_chunk_` for granularity, `_lazyList_` against
`_runForeach_`/`_runSum_` for whether the consumer memoises. A
mismatched pair is then visible in the results table itself, without
reading a single benchmark body.

**Generating by an effectful step, no collection.**
`Source.range` against `ZStream.unfold`: **okay 3x ahead** (90.6
±0.5 against 276.9 ±8.4) — the one axis where per-element cost
genuinely favours the Free-tree representation over any array-native
one, because `unfold` pays the same chunk-of-one tax `Chunks(1)` was
measured paying.

**`Source.unfold` — the general form of `range`, added on request.**
`def unfold[S, A](s: S)(f: S => Option[(A, S)]): Source[A]`, the same
shape as `ZStream.unfold` verified above. `range` stays as the
specialised form since a generic step allocates a tuple per call that
a hand-written `Long` loop does not, but the two produce identical
streams (tested).

**`runCollect`/`runForeach` — added for API parity, and it is an
honest trade, not a free one.** `Source` gained `runCollect: Vector[A]
! Async` and `runForeach(f: A => Unit ! Async): Unit ! Async` at this
library's own `run`-prefix (`Writer.run`, `Async.run`, `!.run`) —
programs, not values forced by `CanBlock`, unlike `toLazyList`. That
turns out to cost something in a single-threaded run: `runCollect`
measures **188.7 ±4.3 against `toLazyList.foldLeft`'s 145.6 ±6.2 —
30% SLOWER**, bars non-overlapping — the `Either`-unwrap through
`Writer.uncons` plus a `Vector :+` per element outweighs `CanBlock`'s
per-pull park here. `runForeach` (153.5 ±9.6, no `Vector` to build)
sits close to `toLazyList` instead. Kept for the composability
`toLazyList` cannot offer — an async caller wants a program back, not
a value already forced — but it is an ergonomics addition, not a
performance one, and is stated as such rather than assumed free.

**Reading a collection under a callback.** The same pairing, and the
same outcome. `ZStream.fromIterable(list).runForeach` walks arrays;
the lanes it sat beside walked a program tree one element at a time.

| lane | us/op |
|---|---|
| `Chunks.foldLeft(Chunks.fromIterator(...))` | **12.8 ±0.1** |
| `ZStream.fromIterable(list).runForeach` | 96.9 ±3.1 |
| `Source.of(list).runForeach` (diagnostic, ours only) | 169.8 ±20.8 |
| `Source.of(list).toLazyList.foreach` (diagnostic) | 165.5 ±4.5 |

**7.6x ahead.** With this row, five of the six places where zio
appeared to lead this table were the same mismatch — our per-element
surface against their chunk-native one — and every one of them flips
when the question is asked the same way on both sides.

**Exactly one genuine gap remains**, and it is the row below.

**Reading one at a time from a buffered channel.**
`Channel.buffer(1024)(list).drained` against a bounded `Queue` +
`ZStream.fromQueue(...).runForeach`.

Re-measured 2026-09-04 after the channel arc (`channel-sentinel-
default` through `relaxed-queues-builder`): **412.5 → 232.6 ±9.8**,
a 1.77x improvement, against `zioChannelForeach` at **113.2 ±1.0**
— which had read 114.2 before and did not move, as the control it
is. Four other lanes in the same run agree with their recorded
values to within a few percent (`zioCollectionWhole` 49.2 against
49.1, `okayStepWhole` 90.4 against 90.6, `zioStepWhole` 275.3
against 276.9), so this is a real change in the code and not in the
weather.

Re-measured again 2026-09-05 with the memoisation removed as well:
`.drained.runForeach` reads **226.8 ±3.8** against
`.drained.toLazyList.foreach`'s **219.1 ±9.6** — no cheaper. So
memoisation was not the cost here either, and the row is now honestly
1.77x behind `zioChannelForeach` at 123.7.

A chunked lane was tried on this row and is kept as a warning:
`.drained.chunked().runForeach` reads **318.7**, SLOWER than
elementwise, because `.drained` already batches internally through
`receiveMany` — putting `.chunked()` on top adds a layer instead of
removing one. Chunking pays only where it replaces a per-element
coordination step, and on this path there was none left to replace.

**Closed 2026-09-05, and the fix was on the producer.**
`Channel.bufferChunked(64, size = 256)(list).drained.runForeach` reads
**90.4 ±2.1** against `zioChannelForeach`'s **135.6 ±6.3** — 1.5x
ahead, from 264.5 before, and the last row where zio led anything in
this table.

The diagnosis took two attempts and the first was wrong. Profiled, the
per-element lane spends 62% in effect machinery and 8% in the channel,
so the obvious move was to batch the PROGRAM — a chunk-shaped read.
That measured WORSE (447.9), and the reason was the batch size: the
producer delivered **1.67 elements** per `receiveMany`. There was
nothing to chunk.

Both sides were paying a program-as-values step per element — `feed`
does `uncons` + `async` + `flatMap` + `send` + `flatMap`, the consumer
does `uncons` + a `Free` step — so neither could run ahead of the
other and no buffer ever accumulated. zio's 137.9 elements per queue
operation come from an ASYMMETRY: a cheap `offer` against a consumer
paying a `Ref` update per element.

`bufferChunked` builds that asymmetry. The producer accumulates into a
LOCAL array — not the `TRef` that `feedChunked` needs for its timed
flush, which costs a transaction per element — so an element costs an
array store and a chunk costs one send. Then the batch is real and
everything above it amortises.

The lesson generalises past this row: **chunking cannot help until
something can run ahead.** It is the same finding as
`channel-send-fastpath`, one layer up, and it is why the per-element
lanes stay in the table as diagnostics rather than being tuned.

**What the elementwise row still measures.** The original profile attributed ~52 of ~140 samples
to `Queue`'s reversal inside the channel — that structure is gone,
replaced by a ring with termination travelling in it — and the rest
to machinery ABOVE the channel: ~28 in `resume`'s rotation, ~21 in
`Drain`/`ChunkBuf`'s batching, ~14 in the `LazyList` cells
`toLazyList` allocates on the consumer side. So the next move on
this lane, if it is worth making, is `Drain` and the bridge, not the
queue underneath it.

## 7. Resource — 1000 bracketed acquire/use/release

| | **Okay region** | **Okay bracket** | ZIO | cats IO | kyo |
|---|---|---|---|---|---|
| right-nested (recursion) | **15.0** | | | | 838 |
| left-nested (foldLeft) | **21.5** | **36** | 135 | 237 | 9011* |

(Same two shapes as §2, same session as §2's table. *The starred kyo
number is the left-nested O(N²) trap explained in §2 — an earlier
version of this page attributed it to "kyo's Resource + Async
runtime"; it isn't the runtime, the right-nested row is. kyo's
natural-shape price is 56x over the Okay region, 6x over ZIO.)

The region is a while-loop with a finalizer list — visible in the
number. The catch must see the CURRENT finalizer list (a tailrec
parameter would hide it — was a real bug, now a comment); releases
run in reverse at Pure or exception. Nothing suspends, so runtimes
built around suspension pay their machinery for nothing.

## 8. Generators — the 1000th Fibonacci, element by element

| Iterator | LazyList | **Okay Producer** | Okay LazyList | kyo | ZStream | fs2 |
|---|---|---|---|---|---|---|
| 12 | 13.5 | **18.4** | 35 | 61 | 172 | 245 |

Per-element unfold — the generator's honest per-element price. The
Okay Producer is 1.5x from the bare-iterator floor; the streaming
libraries pay 10-20x in their per-element mode (their strength is
batches; so is ours — see lane 5).

## 9. Async terminals — runWith vs runAsync (10k ops)

| **runWith** (parking handler) | **runAsync** (the universal drive) |
|---|---|
| **241** | **289** |

The event-loop drive JS runs on, measured on the JVM against the
parking handler: **+20%**, and that is the whole price of
universality. The drive adds one atomic exchange per `Await`
(the callback may fire during registration, on any thread — whoever
loses the exchange continues) and NOTHING per `Run`. So the same
program is portable to a platform with no threads for a fifth more,
and on the JVM you simply keep `runWith`.

## 10. The text stack — lex, parse, reparse, codecs

Measured at load 2.4 with tight bars; 2.5KB JSON document, 50 members.

**Lexing** — and the one result that went the other way:

| element-wise | chunked (512) | chunked (64) | chunked (8) |
|---|---|---|---|
| **42.6** | 52.6 | 55.6 | 70.5 |

Chunked lexing is SLOWER, and the first explanation for it was
WRONG — which is worth more than the number. The three-size probe
showed per-chunk overhead falling away by size 512 while 23%
remained, and the conclusion drawn here was that the residual must be
per-CHARACTER boxing (`Chunk[A]` is an `ArraySeq` over
`Array[AnyRef]`, where the element-wise path reads `charAt`). Two
targeted experiments say otherwise: unboxed storage
(`Chunks.ofChars`, a primitive `Array[Char]`) bought 5%, and reading
that array directly instead of through the generic `apply` bought
another 3%. Eight percent, where the gap was twenty-three.

So the residual is per-CHUNK bookkeeping, not per-character work: a
`Vector.newBuilder`, a token-chunk allocation and a Free node for
each of the forty input chunks, against one builder and no chunk
machinery on the element-wise path. Both improvements are kept —
they are real, if small — and the chunked path's value remains what
it always was: streaming and constant memory over a source you
cannot materialize (a socket, a gigabyte file), where `Scan.all`
needs the whole input in memory.

**Parsing, full vs incremental:**

| full parse | incremental reparse (one-member edit) |
|---|---|
| 85.1 | **38.0** |

2.2x under the full parse for a one-in-fifty edit — real, and
honestly below what O(damage) suggests: the relex dominates the
reparse, and the common-prefix/suffix token scans are O(tokens).
That is the next lever if incremental parsing gets a demanding
workload; the correctness property (untouched subtrees returned BY
REFERENCE) is what the layer exists for.

**Codecs — where the contract shows up as a number:**

| | write | read |
|---|---|---|
| **Okay CBOR** | **0.418** | **0.807** |
| circe (JSON) | 0.422 | 0.623 |
| **Okay JSON** | **0.628** | **10.3** |

Read this table as a price list for CONTRACTS. Our CBOR write ties
circe's JSON write; our CBOR read is 1.3x it — that is the Schema
fold on its own, right next to a hand-tuned parser. Our JSON write
is 1.5x. But our JSON read is 16x slower, and that gap is the whole
point: `Json.read` runs chars → total scanner → total driver →
LOSSLESS CST → projection → Schema fold, where circe parses straight
into its AST. What the gap buys is damage-as-data, byte-for-byte
losslessness, and a HALF-ARRIVED document that still decodes (the
LLM case). Need raw JSON decode speed and none of that? Use circe —
and keep the same `Schema` for the wire where the contract matters.

That was the whole of the explanation for a while, and charging each
stage separately shows it is directionally right and wrongly
emphasised. It is not a five-stage pipeline each taking its share:

| stage | us | share |
|---|---|---|
| chars → CST (scan + drive + build) | 37.3 | **95%** |
| CST → `Json` value | 5.6 | 14% |
| `Schema` fold over that value | **0.97** | 2.5% |
| `Json.read` end to end | 39.1 | |
| circe, for scale | 1.33 | |

Wide bars on a loaded machine, but the proportion is not in doubt. The
generic `Schema` fold — the part a reader might reasonably suspect,
since it is the one piece that is derived rather than written — is two
and a half percent, and **faster on its own than circe's entire
decode**. Everything is in the first two stages, which is exactly
where losslessness and totality live: a CST that keeps every byte,
including the damaged ones, is what costs, and it is what is being
bought. `Json.value` on an already-parsed tree is a public entry point
for anyone holding a session who should not pay for it twice.

**BPE**: 306.7us for a ~3.3KB corpus, from 424.0 — 28%, by taking the
constant out of the scan rather than the exponent. It is still
quadratic per word (a pass per merge, and a merge shortens the word by
one), which is the shape BPE asks for; what went is the waste inside
each pass. The old one built a `Vector` of every adjacent pair,
filtered it into a second `Vector`, then called `minByOption(ranks)` —
so every pair cost TWO map lookups, once for `contains` and once for
the comparison, plus two tuple allocations, on every pass. It now
finds the same minimum in one pass with one `get` per pair and nothing
allocated, over an `Array[String]` instead of a rebuilt `Vector`.

Making it linear needs a heap keyed by rank with positions tracked,
and that is still not worth it: words are short, so k is around ten
and the constant was the whole cost.

## 11. Retrieval — indexing, re-indexing, chunking, query

Measured at load 4.3–6.9 with tight bars (±2% or better on every lane
but one). The document is 8.5KB of Scala — 30 definitions with doc
comments, strings and nesting — and its 6.3KB Python twin.

**Indexing** — parse a file and build its symbol index:

| Scala, 8.5KB | Python, 6.3KB |
|---|---|
| **644** | **449** |

**Where a symbol index's time goes**, split on the same 8.5KB file:

| | us | share |
|---|---|---|
| `Code.source` — lex, parse, tree | 376.1 | 60% |
| `Symbols.of` — the walk over it | 221.5 | 35% |
| `indexFull` end to end | 628.7 | |

The walk looked like the same defect as everything else in this
section: it rebuilds the whole `Index` on every identifier it sees — a
fresh case class, a copied path through the map, a `Vector` append —
and a file of a few thousand tokens is a few thousand of each. It was
rewritten to fill mutable buckets and build the `Index` once, and the
rewrite was **reverted, because it bought nothing**: 244.1us against
245.4 before, which is no difference at all.

So the cost is somewhere else in the walk, and the quiet machine
eventually said where: `indexFoldNoRefs` 189.6 ±5.4 against
`indexFoldOnly` 235.0 ±14.9 — the identifier branch is only **19%** of
the walk, and **81% is the traversal machinery itself**: the recursion,
the `path :+` per definition node, `defHead` scanning each head,
`span`. That is consistent with the refuted rewrite above (mutable
buckets targeted the 19% and bought nothing measurable) and it prices
any future optimization honestly: nothing short of restructuring the
traversal touches the bulk. (An earlier attempt at this lane on a
loaded machine read 427 ±136 while doing strictly less work than the
full walk — kept here as the reminder that a noisy number is not a
small number.)

Worth stating plainly because the pattern held four times in a row
before this: finding the same SHAPE is not finding the same cost.

**Keyword indexing** — building the BM25 postings from a document's
segments, and one more instance of the same defect:

| | tokenization (the floor) | index build |
|---|---|---|
| before | 40.7 | 157.9 |
| after | 41.1 | **75.7** |

`Keyword.fold` was `combine(p, one(s))`: a whole one-segment
`Postings` per segment — with a `groupBy` allocating a `Vector` of
duplicate strings per distinct term, a `mapValues.toMap`, and a map
plus two vectors for a singleton index — followed by a merge that
shifts every document id in it and concatenates a vector per term. All
of it thrown away one line later. Accumulating directly (count into a
mutable map on one pass, append at the document index already known)
leaves the tokenization, which is real work, and drops the rest: the
machinery around it went from 117us to 34.6.

`combine` is untouched and still the monoid — shards merge, and there
the shift is real rather than a shift by zero.

That is 13.2 and 14.0 MB/s warm. Cold, over a real tree —
`IndexReport` on this repository, 201 files and 898KB, including file
I/O and with no JIT warmup at all — the same work runs at 1.2 MB/s
and finishes in 744ms. Quote whichever matches your question; the
gap between them is warmup and I/O, not algorithm.

**Re-indexing after an edit** — one character changed in the 8.5KB
file:

| full re-parse | incremental reparse |
|---|---|
| 400 | **110** |

3.6x, and the same honest caveat as the JSON lane: below what
O(damage) alone suggests, because the relex dominates and the
prefix/suffix token scans are O(tokens). It is the ratio that makes a
live index of a repository the agent is EDITING affordable, which is
the whole reason this layer exists.

**Chunking — the price of parsing, and what it buys:**

| structural (parsed) | windows (unparsed) |
|---|---|
| 684 | **309** |

Structural chunking costs **2.2x** a sliding window. This is the one
table here where the slower number is ours on purpose, so the
comparison has to be made on the thing that actually matters — and it
is measurable, so `TestChunkQuality` measures it rather than asserting
it in prose. On the same file, at chunk counts deliberately matched
(12 structural against 11 windows):

| | definitions returned WHOLE |
|---|---|
| **structural** | **24 / 24 (100%)** |
| windows | 17 / 24 (71%) |

Nearly a third of the window chunks are half of one definition glued
to half of another. And the window split here is not a straw man — it
is `Split.windows`, exact, landing on the lexer's own token spans, and
it reassembles its source byte-for-byte at `overlap = 0`. The 2.2x
buys the 29%, and it is paid once at ingestion rather than per query.

**Per query, with no embedding service in play:**

| symbols (exact) | keyword (BM25) | hybrid (fused) | hybrid + assemble | vectors (240 segs, 1536 dim) |
|---|---|---|---|---|
| **0.55** | 12.4 | 17.9 | 16.7 | 347 |

Half a microsecond for an exact symbol lookup is the number worth
staring at: it is the argument for having a half of retrieval that
needs no vectors at all. "The definition of X" costs a map lookup and
a substring, so an agent can afford to ask it speculatively — which
is exactly what `Grounded.context` does on every turn.

**The embedding representation — the boxing question, asked again
and answered differently.** `Embedding` was `Vector[Float]`, and
`Vector` is a generic trie over `Array[AnyRef]`, so a 1536-component
provider vector was 1536 boxed `java.lang.Float` objects. Four ways
of holding the same numbers, one cosine at provider dimension:

| `Vector[Float]` | `ArraySeq[Float]` | `ArraySeq.ofFloat` | `Array[Float]` |
|---|---|---|---|
| 11.70 | **1.043** | **1.035** | **1.034** |

**11.3x**, and the three unboxed forms are indistinguishable — the
JIT devirtualizes the generic `apply`, so the win needed only a type
alias, not the concrete subclass. Scoring a 2000-segment corpus:
21495µs → 2065µs, 10.4x.

Read this next to §10, where the same hypothesis about the same
mechanism was REFUTED — unboxing the lexer's chunks bought 8% where
23% was predicted. Both results are correct, and the difference is
the point: a scoring loop reads three components per iteration and
does nothing else, so per-element cost is the entire cost, while the
lexer does real work per character and boxing disappears into it.
The lesson is not "boxing is cheap" or "boxing is expensive" — it is
that neither generalizes, which is why both experiments exist.

That last per-query number was 49µs until this benchmark was written. The retriever
built a `Segment` — a substring of the source — for EVERY definition
matching the query, then took the top k; on a corpus where a common
name has hundreds of definitions, that was essentially all of its
cost. Replacing the collection pipeline with an `Iterator` so only the
k returned segments are ever cut made it **91x faster**. Nothing about
the design changed; the benchmark simply asked a question nobody had
asked, which is what benchmarks are for.

---

## 12. Consumption — where the boxing was, per 10k Longs

Every number here is 10 000 elements in chunks of 64, JMH, 3 forks
where a decision hung on it. The interesting part is that two
intuitions were wrong before the lanes were written, so the diagnostic
lanes matter as much as the result.

**A fold whose step is written at the call site.**

| lane | us/op |
|---|---|
| `Chunks.fold` + `Fold.sum[Long]`, before | 38.2 |
| `Chunks.foldLeft(p)(0L)(_ + _)` | **7.0** |
| the same step, via `Numeric.plus` | 8.2 |
| a hand loop over the same chunks | 2.6 |

`Numeric` survives inlining fine, so the typeclass was never the cost.
`Fold[A, S]` is `add(s: S, a: A): S`, generic in both, and a
megamorphic call site gives the JIT no way to remove the boxes.

**Which half of the boxing.** This is the lane that changed the design:

| lane | us/op |
|---|---|
| accumulator generic, element read directly | 29.4 |
| element boxed, accumulator a raw `long` | **2.8** |
| floor | 2.6 |

The accumulator is essentially the whole cost; boxing the element read
is nearly free. That is why only the accumulator is specialized, and
why the specialization is useful at all — `Chunks.fold` cannot know
the element type either way.

**A fold that arrives as data**, where nothing can inline — an
`Aggregator`'s, a java `Collector`'s:

| lane | us/op |
|---|---|
| a plain `Fold[Long, Long]` | 34.1 |
| `Fold.long(z)(f)`, constructor **not** inline | 27.5 |
| `Fold.long(z)(f)`, constructor inline | **7.8** |

The middle row is the trap: a plain constructor stores the step as a
`Function2`, whose `apply` erases generic, so the boxing the subtrait
just removed comes straight back in the field it closed over. `inline`
beta-reduces the lambda into `addLong` and there is no function object
left to call. The anonymous class is then duplicated per call site,
which is the mechanism rather than an accident.

**Aggregators**, after the specialization was carried up and the tuple
accumulators flattened:

| lane | before | after | floor |
|---|---|---|---|
| `count` | 37.8 | **19.5** | 8.6 |
| `sum` | 40.8 | **18.5** | 8.6 |
| `mean` | 87.0 | **37.3** | 18.6 |
| `variance` | 90.9 | **74.7** | 49.6 |

`Aggregator.fold` used to build a generic `Fold` unconditionally, so
none of the above reached Spark, Flink, the cluster or a java
`Collector` — the callers that can inline nothing and therefore need it
most. `count` was 5.5x slower than `Fold.count` for identical
arithmetic.

The tuple accumulators were the larger hole and were not obvious in
advance: `mean` carried a `(N, Long)` and `variance` a
`(Long, Double, Double)`, which is three and four allocations per
**element** — the tuple, plus a box per field, since a tuple's fields
are `Object`. Flat case classes with primitive fields cost one, and in
a local fold the JIT often drops even that. `variance` stays close to
its floor because Welford's per-element division dominates it, not the
accumulator.

## 13. The sketches — where the same defect was hiding

The aggregators' tuple accumulators turned out to be a bigger hole than
the boxing above them. The sketches had the same defect one layer
further in, and worse, because their state is large.

| lane | before | after | floor |
|---|---|---|---|
| `tDigest` | 70 030 | **120.7** | — |
| `countMin` | 2 282.5 | **191.1** | 68.7 |
| `hyperLogLog` | 489.3 | **163.8** | 36.2 |

Per 10k elements. The t-digest is not a typo: it was 7us per element.

**Count-Min** kept `Vector[Vector[Long]]` and did
`rows.zipWithIndex.map(...)` with an `updated` per row on every add —
a tuple per row, a fresh outer vector, and a copied path through each
2048-element inner one, for what is `depth` counter increments.
`Array[Array[Long]]` written in place: 12x.

**HyperLogLog** kept `Vector[Byte]` and rebuilt a path through 16 384
registers whenever a rank improved. One byte store instead: 3x.

**t-digest** was algorithmic, not representational. Each add did an
`indexWhere` (a linear scan of up to `2*delta` centroids), a `patch`
(a full copy of the vector to insert one point), and a `compressed`
that sorts when it runs. The standard shape — the one Dunning
describes — buffers incoming points at O(1) and compresses once the
buffer fills, merging the sorted centroids with the sorted buffer in
one pass. 580x.

All three now mutate their accumulator in place and hand the same one
back. That is within the contract `Aggregator` is declared against —
Spark's `seqOp` is explicitly allowed to modify and return its first
argument, and `Collect.aggregator` already did this for a java
`Collector` — and it is kept safe by two rules: `init` allocates a
fresh sketch on every call, so two folds never share one, and `merge`
allocates its result, so neither side is disturbed by combining.

### The two places the sweep cleared

Having found the same defect in the aggregators and then, worse, in
the sketches, the obvious next question was where else state is
rebuilt per element. Two candidates, both cleared, and the reasons are
worth keeping so they are not re-examined.

**`Delim`** carries its continuation stack as a `List[Seg]` — push and
pop are O(1) and allocate one cons cell, which is what a stack costs.
Nothing to fix.

**`Parse.build`** does look like the others: its accumulator is a
`List` stack of tuples whose third field is a `Vector`, so every token
does a `kids :+ c`, a fresh tuple, a fresh cons cell and a fresh
`Building` — four allocations. But two things say leave it.

It is 13% of a full parse (21.0us against 157.6 for `parseFull`, on a
machine reading 157.6 where section 10 above measured 85.1 — the
ratio is the number to read, not the absolutes). Lexing is 47%.
Halving the builder would buy six percent.

And the persistence is load-bearing. `Parse.reparse` resumes the
builder from a snapshot at a node boundary before the damage and
returns the old tree's untouched subtrees BY REFERENCE. That requires
state that can be shared and held, which is exactly what an in-place
accumulator cannot be. The sketches could go mutable because nothing
holds an old sketch; a builder is held by design.

## 14. Granularity — the comparison that was never like for like

`okayWeak 206us` against `zioWeak 169us` looked like a mechanism gap.
It was a units gap. `ZStream.fromQueue` takes up to `maxChunkSize`
(4096 by default) elements per queue operation; our consumer called
`receiveBlocking` once per element. One coordination step per element
was being measured against one per batch.

Measured at BOTH granularities, both libraries at the same weak
guarantee (`ChannelGranularityBenchmark`, N=4000, cap=1024):

| lane | before `popMany` | after |
|---|---|---|
| okayElementwise | 190.1 | 212.3 |
| okayChunked | 182.9 | **111.6** |
| zioElementwise | 304.0 | 336.8 |
| zioChunked | 113.7 | 149.5 |

(The two columns are separate runs on a box whose absolute level
drifts by ~30% between them — `zioChunked` moved without its code
changing. Read the ratios inside a column, never a number across two.)

Two things the table says.

**Elementwise, we were already ahead** — 190 against 304, and 212
against 337 in the later run. The original gap was entirely the
granularity mismatch.

**Chunking bought them 2.7x and us 1.04x**, and that was the defect
worth finding. The batch was real: 4000 handshakes became 299, an
average of 13.4 elements each. It bought 4%, because
`receiveManyAsync` called `pop()` in a loop. We had batched the
HANDSHAKE and not the QUEUE — the ring still paid a head CAS per
element, and that CAS was 24% of the leaf samples.

`Ring.popMany` claims a run of consecutive published slots with one
`compareAndSet` and leaves only the slot read and the stamp write per
element, since those carry the data. Chunking now buys 1.90x, and the
chunked lane lands ahead of `zioChunked` in the same run.

The general lesson is the one §6c already stated from the other side:
amortization is a property of the batch, not of a representation. A
batched API over an unbatched primitive amortizes only the part it
touches, and here that part was 13% while the untouched part was 60%.

### The handshake, separately

The profile also put `CanBlock.block` third among leaf frames, level
with the ring's own CAS. It allocated a `CompletableFuture` per
operation — a node, a Treiber stack of signallers and a spin before
parking — to carry one value to one waiter exactly once, on a path
where the callback usually fires SYNCHRONOUSLY inside `register`
because the element was already buffered. Replaced by a typed one-shot
slot with a fast path that never parks: ~8% elementwise, and no cast,
since the slot is parameterised on `A`.

## 15. Both ends, and the guarantee table on one axis

Three follow-ups from §14, measured together.

**The guarantee table was on two axes.** Its okay lanes were
elementwise and its zio lanes went through `ZStream`, which is
chunked — `zioWeak` read 114.8 against `zioChunked`'s 114.0, and
`okayWeak` read 203.7 against `okayElementwise`'s 197.1. No row could
be read across. Every lane now names its granularity, and only lanes
sharing a suffix compare:

| | elementwise | chunked |
|---|---|---|
| okayStrong — drain-on-close as an INVARIANT | 279.4 | 166.1 |
| okayWeak — close discards | 204.6 | 113.1 |
| **okayLayered — the same strong contract as a LAYER** | **194.5** | **115.9** |
| zioStrong — `Queue[Option]` | 294.9 | 124.0 |
| zioWeak — `Queue` | 298.3 | 115.3 |

The layer result survives the correction and gets sharper: the strong
contract costs 2.4% bought as a sentinel (115.9 over 113.1) and 47%
baked into the mechanism (166.1 over 113.1); elementwise the layer
costs nothing at all. `okayLayeredChunk` at 115.9 delivers the same
contract as `zioStrongChunk` at 124.0.

The elementwise column also shows what the old table hid: `zioWeakElem`
is 298.3, not 114.8. Their advantage was never the queue.

**The send side needed the same treatment, and answered differently.**
`Ring.pushMany` claims a run of writable slots with one tail CAS, and
`Channel.sendManyNow` exposes it. Note first what does NOT need it:
`feedChunked` amortizes by REPRESENTATION, putting whole chunks into a
`Channel[Chunk[A]]`, so its channel already pays one transaction per
chunk. `sendManyNow` is for a producer holding a batch of ELEMENTS.

| lane | us/op |
|---|---|
| okaySendBulk + chunked receive | **66.9** |
| okayChunked | 105.9 |
| okaySendElem + chunked receive | 109.0 |
| zioChunked | 114.8 |
| okaySendElem + elementwise receive | 196.7 |
| okaySendBulk + elementwise receive | **280.4** |

**Batch both ends or neither.** Against a draining consumer the bulk
send is 1.63x (66.9 against 109.0) and lands 1.71x past `zioChunked`.
Against an ELEMENTWISE consumer it is a 1.43x LOSS. The cause is room,
not the claim: a consumer taking one element at a time keeps the ring
full, so every bulk attempt fails its scan and falls back to a single
send anyway — the scan is pure overhead on top of work that had to
happen regardless. A batched primitive is not a free upgrade; it is a
bet that the other end leaves room.

**The acceptance answer stopped boxing.** `Function1` is specialised
on Int, Long, Float and Double and not on Boolean, so every send's
answer went through `apply(Object)` — 8% of the leaf samples. `Accepted`
is a SAM with a primitive signature, and `CanBlock.blockAccepted` is
the wait that carries the bit as a bit, since generic `block` boxes
into its own slot as well. The `Right`+`Some` on the receive side was
left alone deliberately: `receiveBlocking` returns `Option[A]`, so on
that path the wrapper is in the return type rather than the
implementation, and only an internal consumer can avoid it.

### A note on the measurements themselves

Two runs were discarded. In one, `zioChunked` moved from 114 to 216
without its code changing; in another the control lane
`okaySendElemRecvChunk` — whose body is `okayChunked`'s — read 414
±244 against its own 106. Both were the box, contested by a sibling
build. The control lane exists for exactly this: two lanes that must
agree, so a run can be checked before it is believed.

## 16. Every capacity a ring — the table that closes the arc

`ChannelGuaranteeBenchmark`, N=4000, cap=1024, one quiet box, f=3
i=8. Only lanes sharing a guarantee AND a granularity compare — the
methodology §15 had to correct twice.

| pair | okay | zio | ratio |
|---|---|---|---|
| unbounded, chunked | **49.1** | 383.8 | **7.8x** |
| unbounded, elementwise | **110.5** | 439.1 | **4.0x** |
| bounded strong, chunked | **56.2** | 125.9 | 2.24x |
| bounded strong, elementwise | **248.6** | 286.9 | 1.15x |
| weak, chunked | **54.8** | 116.1 | 2.12x |
| weak, elementwise | **237.6** | 304.0 | 1.28x |
| `StmChannel`, elementwise | **235.7** | 286.9 | 1.22x |
| `StmChannel`, chunked | 128.0 | 125.9 | zio by 1.6% |

The unbounded pair is the largest gap in this file and it deserves the
scepticism: `Queue.unbounded` is the like-for-like, not
`Queue.bounded`, because a channel that never makes its producer wait
is not the same object as one that does. Both sides here are
unbounded, both carry drain-on-close, and both are read at the same
granularity.

WHERE THE ARC STARTED. `okayWeak 206` against `zioWeak 169`, read as a
mechanism gap. It was four things, in this order:

1. a **units** gap — `ZStream.fromQueue` takes up to 4096 elements per
   queue operation and our consumer took one (§14);
2. a **batched API over an unbatched primitive** — the chunked receive
   looped over `pop`, so the ring still paid a head CAS per element
   and a real 13-element batch bought 4% (§14);
3. a **producer that could not run ahead** — `sendBlocking` ran the
   parking handshake even with room in the ring, and a producer that
   cannot get ahead leaves the consumer nothing to batch: their
   average batch was 137.9 elements against our 35.4 (§15);
4. a **guarantee bought in the wrong place** — drain-on-close as an
   invariant of the mechanism cost 47%, as a mark travelling in the
   FIFO stream 2.4%.

None of the four was the mechanism being slower, and each was found by
measuring the thing rather than the story about it.

## Why the good numbers, in one place

1. **No runtime where none is needed.** Pure binds are plain data
   (or, opted in, plain calls); fibers are virtual threads; blocking
   is parking. Every lane where competitors pay a scheduler/run-loop
   tax and Okay doesn't traces to this.
2. **The rotation.** Left-nested binds rebalance tail-recursively in
   `fold` — the freer monad's classic quadratic trap (visible in the
   kyo Env/Emit lanes) never fires.
3. **Zero-allocation telling.** Writer's operation is an opaque
   one-constructor GADT: emitting costs one small node, and
   measurably nothing against the rest of the work (198.0us with it,
   203.2 without) — while making the answer type recoverable, so no
   continuation is resumed by assertion.
4. **Relay for the one-shot majority.** Tail-resumptive handlers run
   as tail loops; the general (abortive/multi-shot) handler exists
   for the minority that needs it.
5. **Chunks amortize the tree.** One Free node per 64 elements, tight
   array passes between — program-as-value at near-array prices.
6. **Inline staging for known shapes.** When the pipeline is spelled
   where it runs, partial evaluation removes even the amortized cost
   — under the Iterator floor.
7. **The laziness contract is kept, not sold.** Construction does no
   work; the one encoding that trades it away (`Eager`) says so on
   the label. Several "slower" competitor numbers are actually THIS
   difference measured (see the kyo asterisks).
8. **The same incremental machine, at every layer.** The lexer's
   reconvergence and the parser's snapshot resume are one mechanism,
   and §10 and §11 are that one mechanism measured on JSON and on
   source code — 2.2x and 3.6x under a full re-run. Nothing in the
   retrieval layer had to invent incrementality; it inherited it.
9. **Cheap questions stay cheap.** Half a microsecond for an exact
   symbol lookup is what lets `Grounded.context` retrieve on EVERY
   turn instead of asking the model whether it should. Design
   decisions above depend on prices below being small.

## Where the numbers are honest about limits

- Microbenchmarks: naked plumbing, no real workloads; they price
  mechanisms, not applications.
- fs2/ZStream generator and merge lanes are their per-element worst
  cases (stated in place).
- JSON decode pays the totality/losslessness contract (stated
  above); CBOR and encode do not.
- The retrieval lane has no third-party comparison, deliberately: no
  Scala library ships this shape, and benchmarking a Python stack
  across a process boundary would measure the boundary. What it
  compares instead is our own two methods against each other, which
  is the choice a user of this library actually faces.
- Structural chunking is SLOWER than windowing (2.2x) and that is the
  intended trade; the quality percentage next to it is the other half
  of the number and should never be quoted apart from it.
- The host is a busy laptop; medians across forks and same-session
  grouping are the discipline, and history.tsv records the load.

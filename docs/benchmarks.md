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

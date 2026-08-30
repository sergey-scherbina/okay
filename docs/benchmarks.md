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
dependencies live only in the compare module).

---

## 1. Bind chain — 10k left-nested flatMaps, built and run

| **okay Eager** | kyo | **okay Cont** | **okay Free** | cats Free | cats Eval | cats IO | ZIO | atnos |
|---|---|---|---|---|---|---|---|---|
| **5.1** | 58 | **89** | **95** | 129 | 136 | 153 | 181 | 260 |

**What it measures.** The raw cost of the monadic plumbing itself:
build a 10 000-step flatMap chain by foldLeft (the WORST, left-nested
shape), then run it. No effects, no I/O — just who pays how much per
bind.

**Why okay's numbers.** Three encodings, one interface, chosen per
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
  chain is running nothing. 10x under kyo on this lane — with kyo's
  hazards STATED, not hidden (see the asterisk below).

**Why the competitors' numbers.** cats Free lacks the rotation (its
fold re-associates by allocation); cats IO and ZIO run every bind
through a fiber runtime — shift checks, trace buffers, interruption
machinery — pure overhead when nothing suspends; atnos-eff pays the
open-union tagging of classic freer on every operation.

**The asterisk that reframes the whole table.** kyo's 58 is
FRONT-LOADED: construction evaluates. Build-only lanes
(LazinessBenchmark): okay 13.8 (15% of its full cost), cats IO 26.9,
kyo 58.5 — **101%**: for kyo, construction IS the computation.
Building an infinite pure-recursive program runs 513 iterations
uninvited (their safepoint budget); an exception in a flatMap lambda
throws at BUILD time; an effect runs once-at-build, so the value is
not reusable as a description (all three demonstrated in
compare/TestLaziness). In build-many-run-few scenarios okay is 4.2x
cheaper; and okay offers the same trade EXPLICITLY as `Eager`,
per-program, instead of as the only semantics.

## 2. Reader — 10k asks · Writer — 10k tells

| Reader | **okay** | ZIO | cats Kleisli | atnos | kyo Env |
|---|---|---|---|---|---|
| | **110** | 240 | 350 | 1737 | 362 756* |

| Writer | **okay** | cats WriterT/Chain | atnos | kyo Emit |
|---|---|---|---|---|
| | **286** | 1127 | 3202 | 386 322* |

**What they measure.** Handled operations at volume — the everyday
shape of effectful code.

**Why okay's numbers.** Reader runs at RELAY speed: a
tail-resumptive handler must resume exactly once, so handling is one
tail-recursive loop — no continuation capture, no allocation per ask
(relay measured 1.45x over the general handler on forwarding-heavy
work). Writer's `tell` is ZERO allocation: the operation is an
opaque IDENTITY signature — telling w IS the value w, no wrapper
node; the handler is a bespoke tail loop into a Vector.

**Why the competitors' numbers.** cats WriterT allocates a
tuple-in-monad per tell; Chain helps, the wrapping doesn't. atnos
pays union tagging again. The starred kyo numbers are ~1000x off
because kyo's Env/Emit resumption RE-TRAVERSES the pending
computation — quadratic on left-nested chains with handled ops. That
is precisely the pathology okay's Bind rotation exists to prevent;
the lane is the rotation's value made visible.

## 3. Choice — 2^13 branches, all collected

| List (floor) | **okay** | kyo | atnos |
|---|---|---|---|
| 580 | **1603** | 3834 | 5392 |

The one handler that is genuinely MULTI-SHOT: the continuation runs
once per alternative. okay's `runChoice` folds `k(x)` over the
alternatives on the Free tree directly; 2.8x from the bare-List
floor is the price of programs-as-values here, and it still halves
kyo. (Multi-shot is where one-shot-optimized runtimes can't follow:
this handler cannot be expressed with relay OR exceptions.)

## 4. Fork/join — 100 trivial fibers

| raw Loom (floor) | kyo | **okay** | ZIO | cats IO |
|---|---|---|---|---|
| 21 | 25 | **29** | 50 | 140 |

**Why okay's number.** There is almost no okay here — that is the
design. A fiber IS a virtual thread; spawn is `Thread.startVirtualThread`,
join parks. No fiber runtime of our own means nothing added over the
floor but 8us of bookkeeping. ZIO and cats IO pay their own
schedulers, run-loops and interruption protocols; kyo sits close to
the metal too (its scheduler is excellent) — we simply refuse to
compete by NOT having one.

## 5. Stream pipeline — map/filter/take(1000)/sum

| Iterator (floor) | **Staged** | **okay chunked** | **okay elements** | okay iterator | okay LazyList | kyo | ZIO | fs2 |
|---|---|---|---|---|---|---|---|---|
| 14 | **1.6*** | **16.9** | **23.6** | 53 | 143 | 239 | 692 | 1410 |

(*Staged and its same-run floor of 19.3 come from a different
session; the rest is one session. The 12x-under-the-floor number is
real: see below.)

**What it measures.** The bread-and-butter stream pipeline in each
library's fastest mode.

**Why okay's numbers, mode by mode.** This table is one design
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
priced per element; ZStream similarly. It's an honest worst-case for
them (singleton-unfold generators) and honestly noted as such.

## 6. Merge — two 500-element streams by readiness

| **okay chunked** | ZIO | okay elementwise | fs2 |
|---|---|---|---|
| **14.7** | 47 | 158 | 9031 |

Readiness-merge is what zip and ++ cannot express: a fiber per
source feeds one channel, the loser of every race simply arrives
later. Chunking the STREAM (not the queue — a chunked-queue variant
was tried and REFUTED, it's in history.tsv) beat ZIO's own
chunk-aware merge 3.2x. fs2's number is its worst case (singleton
elements through its concurrency machinery), stated as such.

## 7. Resource — 1000 bracketed acquire/use/release

| **okay region** | **okay bracket** | ZIO | cats IO | kyo |
|---|---|---|---|---|
| **18.7** | **26.3** | 106 | 197 | 8566 |

The region is a while-loop with a finalizer list — visible in the
number. The catch must see the CURRENT finalizer list (a tailrec
parameter would hide it — was a real bug, now a comment); releases
run in reverse at Pure or exception. Nothing suspends, so runtimes
built around suspension pay their machinery for nothing.

## 8. Generators — the 1000th Fibonacci, element by element

| Iterator | LazyList | **okay Producer** | okay LazyList | kyo | ZStream | fs2 |
|---|---|---|---|---|---|---|
| 12 | 13.5 | **18.4** | 35 | 61 | 172 | 245 |

Per-element unfold — the generator's honest per-element price. The
okay Producer is 1.5x from the bare-iterator floor; the streaming
libraries pay 10-20x in their per-element mode (their strength is
batches; so is ours — see lane 5).

## 9. Async terminals — runWith vs runAsync (10k ops)

The universal terminal (`runAsync` — the event-loop drive JS uses,
runnable on the JVM too) against the parking handler on the same
chain. Numbers land with the next quiet-host session; the drive adds
one atomic exchange per Await and nothing per Run, so the expected
gap is small. See `AsyncDriveBenchmark`.

## 10. The text stack — lex, parse, reparse, codecs (NEW)

`TextBenchmark` (compare module) covers: element-wise vs CHUNKED
lexing of a 2.5KB JSON document; full parse vs INCREMENTAL reparse
after a one-member edit (the O(damage) claim as a number — the first
noisy run already shows ~3x with the session machinery included);
one derived Schema written/read as JSON text and CBOR binary, with
circe on the same value as the ecosystem line; and BPE tokenization
throughput.

**The honest framing for the circe lanes.** okay's `Json.read` runs
chars → total scanner → total driver → LOSSLESS CST → projection →
Schema fold. circe parses straight to its AST with a hand-tuned
parser. okay's write side and CBOR are competitive (a string
builder / byte buffer over the Schema fold); the read side pays for
a different CONTRACT — totality (damage is data, truncation
decodes: the LLM case) and byte-for-byte losslessness (the editor
case). When you need raw JSON decode speed and none of that, use
circe; the flagship here is that a HALF-ARRIVED document still
answers. Final numbers land with the next quiet-host session (the
last attempt ran at load 160+ from a sibling project's builds —
recorded, discarded).

---

## Why the good numbers, in one place

1. **No runtime where none is needed.** Pure binds are plain data
   (or, opted in, plain calls); fibers are virtual threads; blocking
   is parking. Every lane where competitors pay a scheduler/run-loop
   tax and okay doesn't traces to this.
2. **The rotation.** Left-nested binds rebalance tail-recursively in
   `fold` — the freer monad's classic quadratic trap (visible in the
   kyo Env/Emit lanes) never fires.
3. **Zero-allocation telling.** Writer's operation is an opaque
   identity signature: emitting a value allocates nothing.
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

## Where the numbers are honest about limits

- Microbenchmarks: naked plumbing, no real workloads; they price
  mechanisms, not applications.
- fs2/ZStream generator and merge lanes are their per-element worst
  cases (stated in place).
- JSON decode pays the totality/losslessness contract (stated
  above); CBOR and encode do not.
- The host is a busy laptop; medians across forks and same-session
  grouping are the discipline, and history.tsv records the load.

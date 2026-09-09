# Benchmarks: the cases, the numbers, and the why

Every number here is JMH average time (us/op, lower is better) from
`src/jmh/history.tsv`, which records every run with its date, commit,
host load and protocol — including the experiments that were tried
and REFUTED, so nobody re-runs them blind. The working rule of
specs/interpreter-optimization.md applies throughout: one measurement
is a hypothesis, not a result; lanes quoted together were measured in
the same session on the same (busy, honestly noted) host.

Versions compared: cats-effect 3.5.7, ZIO 2.1.14, kyo 0.16.2,
atnos-eff 7.0.4, fs2 3.10.2, circe 0.14.10 — on **Scala 3.9.0 LTS**
since 2026-09-08 (every table below §0 was re-measured on it; earlier
tables on this page were 3.7.4).

Run them yourself: `sbt 'Jmh/run .*Fib.*'` (core lanes),
`sbt 'compare/Jmh/run .*Compare.*'` (ecosystem lanes; the heavy
dependencies live only in the compare module),
`sbt 'compare/Jmh/run RagBenchmark'` (retrieval),
`sbt 'compare/Jmh/run EmbeddingBenchmark'` (the embedding representation).

One claim here is not a time at all — structural chunking's advantage
is that a chunk is a WHOLE definition, so it is measured as a
percentage by `okay-rag`'s `TestChunkQuality` and reported in §11
beside the microseconds it costs.

## The short version

Every number is microseconds per operation, lower better, JMH, from
one session on a quiet 14-core box. Where a competitor appears, both
sides were checked against the Lane rules below — same shape, same
granularity, same source, same resources — because five lanes that
failed that check were found on 2026-09-08 alone, and every one of
them read as "we are slow" or "we are fast" for the wrong reason.

| the question | okay | best competitor | § |
|---|---|---|---|
| run an already-finished program | **0.0021** | ZIO 0.043 | [§0](#0-the-floor--what-each-runtime-charges-to-run-nothing) |
| 10 000 flatMaps, built and run | **5.5** eager, **95** Cont | kyo 60 | [§1](#1-bind-chain--10k-left-nested-flatmaps-built-and-run) |
| direct syntax over its own flatMap chain | **1.05x** | zio-direct 0.65x | [§1b](#1b-direct-syntax--the-same-10k-binds-written-as-code) |
| 10 000 handled reads | **79** | kyo 253 | [§2](#2-reader--10k-asks--writer--10k-tells) |
| 10 000 handled writes | **159** | kyo 178 | [§2](#2-reader--10k-asks--writer--10k-tells) |
| fork and join 100 fibers | 24.0 | **kyo 18.5** | [§4](#4-forkjoin--100-trivial-fibers) |
| fork and join 10 000, runtime-native | **796** | kyo 884 | [§4b](#4b-adversarial-lanes--the-rows-we-expected-to-lose) |
| map/filter/take/sum over 1 000 | **1.70** staged, **8.2** chunked | fs2 21.9 | [§5](#5-stream-pipeline--mapfiltertake1000sum) |
| merge two streams by readiness | **13.3** | ZIO 51.5 | [§6](#6-merge--two-500-element-streams-by-readiness) |
| 1 000 bracketed acquire/release | **15.2** | ZIO 116 | [§7](#7-resource--1000-bracketed-acquireuserelease) |
| the 1 000th Fibonacci, per element | **19.2** | kyo 70.7 | [§8](#8-generators--the-1000th-fibonacci-element-by-element) |
| read a 2.5 KB JSON document | **0.35** staged, 0.66 lossless | circe 0.56 | [§10](#10-the-text-stack--lex-parse-reparse-codecs) |
| look up a symbol in an index | **0.56** | — | [§11](#11-retrieval--indexing-re-indexing-chunking-query) |
| 4 000 elements through an unbounded channel, chunked | **56.0** | ZIO 439.7 | [§16](#16-every-capacity-a-ring--the-table-that-closes-the-arc) |
| 8 000 elements, 16 producers (okay's own buffers) | **128** | — | [queues.md](queues.md) |

**Where okay loses, and it is here rather than buried:** fork/join of
100 fibers against kyo (24.0 against 18.5, §4), and the single-channel
many-to-many row against ZIO (§4b). Both are stated in place with the
matched pairing that makes them honest.

### Lane rules — before a competitor's number is quoted

Every wrong row this document has carried was a lane asking a
different question of each side. Three checks, each with the row
that taught it:

1. **Shape.** A lane built by `foldLeft` gets a RIGHT-NESTED twin
   before its number is quoted. kyo's Env/Emit/Resource read ~1000x
   on the left-nested shape `((ask >>= f) >>= f) >>= f` — quadratic
   in kyo, linear right-nested — and were quoted as the library's
   price for a week (§2; `ReaderBenchmark` keeps both shapes side by
   side, and its header says which is which). A number that changes
   by orders of magnitude with the nesting is the SHAPE's price, not
   the library's, until the twin says otherwise.
2. **Pairing.** Only lanes sharing a granularity compare: chunked
   against chunked, elementwise against elementwise, memoised against
   memoised. Five of six "ZIO ahead" rows on 2026-09-04/05 were
   mismatched pairs, and every one flipped once paired (§6b, §14–§16).
   A lane names its granularity in its name.
3. **Source.** A competitor is priced from the source its author
   intended: `ZStream.range`, `fs2.Stream.emits`, kyo `Stream.range`
   — not `iterate`, not fs2's `range`, which is a singleton chunk per
   element by construction (§5). The per-element source stays in the
   table as the worst case it is, beside the fair one, never alone.
4. **Resources.** Both sides get the same BUDGET: the same buffer
   capacity, the same worker count, the same chunk size — anything the
   runtime is handed rather than earns. Added 2026-09-08 after an
   audit found four lanes breaking it, and the rules above could not
   catch any of them because each pair had matching granularity and
   an intended source. `adaptive.parts(16).each(1024)` is **16 384
   slots**; the `oneRing`, `growing` and ZIO `Queue.bounded(1024)`
   lanes it was quoted against have 1024. That is a 16x memory
   advantage read as a mechanism advantage, and it sat in two files
   and three tables. A lane whose capacity comes from
   `availableProcessors` breaks this rule too — its number is not
   comparable with the same lane on another machine.

A new competitor lane lands with all FOUR answered in its header, or
it lands without a number.

**And the rules bind the lane you wrote yourself.** Every violation
found in the 2026-09-08 audit was on OUR side, because our lane is
written first and the competitor's second, against it. One of them
(§5's chunk size: okay at its 64-element default against three
competitors getting the whole stream in one chunk) runs AGAINST us and
went unnoticed for months precisely because okay won the row anyway. A
comparison unfair in your own disfavour is still unfair.

## How to read this page

Each section is one question, and has three parts in this order: the
table, why okay's number is what it is, and why the competitors' are
what theirs are. After that comes the part most pages do not have and
this one exists for — **what was tried and REFUTED**, kept because the
`performance` skill's first rule is that a plausible idea nobody
recorded as wrong gets tried again.

So the long prose is a record, not a preamble. If you want the number,
the table is the top of the section. If you want to change the number,
read the refutations first: several of them cost a day and say
plainly what did not work and by how much.

Numbers are re-measured in batches, and a section says which run it
belongs to. When a row moved for a reason other than the code — a
different compiler, a corrected lane, a busier box — the row says
that too, because a table that silently improves is a table nobody
can trust.

## THE RUN THESE TABLES COME FROM (lane-fairness, 2026-09-08 evening)

    date       2026-09-08, 16:00-19:08
    tree       master + the lane-fairness audit
    host       Apple Silicon, 14 cores, nothing else running
    runner     sbt-free, one JVM per class, `-f 1`, each class at its
               own declared @Warmup/@Measurement, 3 rounds, per-lane
               MINIMUM
    scale      22 classes, 501 lanes with all three rounds, 0 failures
    quality    the three rounds are indistinguishable — each one's
               median ratio to its own lane's best is 1.010-1.012, so
               nothing here rests on a quiet round or a noisy one

**THIS RUN FOLLOWED AN AUDIT, AND FIVE LANES CHANGED BEFORE IT.** Every
cross-library class was checked against the Lane rules at the end of
this page, and five pairs were found asking different questions of the
two sides — all five on OUR side, because our lane gets written first
and the competitor's second, against it. Four gave okay an unearned
advantage; one handicapped us and had gone unnoticed for months
precisely because okay won the row anyway.

The audit added a FOURTH rule — same resources: same buffer capacity,
same worker count, same chunk size — which none of the existing three
could have caught, since every mismatched pair had matching
granularity and an author-intended source.

**Numbers below that got WORSE than the previous table mostly did not
regress; they stopped being flattered.** Where that is so, the row
says it.

### The previous run, kept for the diff (bench-refresh, 2026-09-08 morning)

Sections 0-11 and 14-16 were re-measured in ONE session, because the
tables had drifted into a dozen sessions on hosts of varying quiet and
this page's own rule is that lanes quoted together share a session.

    date       2026-09-08, 23:35-02:57
    tree       master cf820fa7 — Scala 3.9.0 LTS (75377969), and the
               scheduler fix 3f09bd9c in it
    host       Apple Silicon, 14 cores, freshly booted, no sbt and no
               sibling build for the duration; both scalascript
               launchd guards unloaded so nothing killed a fork
    runner     sbt-free: `java -cp <exported Jmh classpath>
               org.openjdk.jmh.Main`, ONE JVM per benchmark class
    protocol   `-f 1`, each class at its own declared
               `@Warmup`/`@Measurement`, 3 rounds, PER-LANE MINIMUM.
               The +- beside a number is that round's own 99.9% bar.
    scale      22 classes, 499 lanes, 0 failures

**Why the minimum of three rounds, stated because this run proves the
point.** `StreamOpsBenchmark.okayIterator` read 126.2, then 94.9, then
57.3 across the three rounds — a box still settling, not a change.
Read after one round it looked like a 2.2x regression against the
standing table and was very nearly reported as one. `catsFree` did the
same in the other direction (121.8 / 168.0 / 121.0). A single round of
this suite is not a number; it is a draw from a distribution whose
left tail is the answer.

**What this run did NOT re-measure**, so nobody reads a fresh date over
a stale number: §12 (consumption), §13 (the sketches), §17 (actors and
the reactive bridge), §18 (JS and Native), §19 (okay-script), §11's
embedding-representation table, and every A/B inside the prose that
records a past experiment — those keep their own dates and are marked
where they sit. §9's runWith/runAsync pair is unchanged too.

**Scala 3.9.0 changes the compiler that generates every lane here**, so
these numbers are not strictly comparable with any earlier table on
this page, which was 3.7.4. No same-session 3.7.4-vs-3.9.0 A/B was
run — that needs both trees built and alternated, and it is not what
was asked for. Where a row moved a lot against its predecessor the
text says so and does NOT attribute it to the compiler.

---

## 0. The floor — what each runtime charges to run nothing

| **Okay** | ZIO | kyo `.eval` | kyo `runAndBlock` | cats IO |
|---|---|---|---|---|
| **0.0021** | 0.043 | 0.0048 | 8.1 | 8.4 |

(`FairnessProbeBenchmark`, `floor_*`, bench-refresh 2026-09-08: an
already-finished program, run. Same run as the `chain_*`, `pipeline_*`
and `queue_*` lanes cited below. The two zeroes this table used to
print were a rounding of 0.002 and 0.005 — real numbers, three
thousand times under ZIO's, and worth printing as themselves.)

**Why this table exists.** Every cats lane in this document goes
through `unsafeRunSync`, which in cats-effect 3.5.7 schedules the
fiber onto the compute pool and blocks the caller on an
`ArrayBlockingQueue` (`IO.scala:1031`, `IOPlatform.scala:70`) — two
thread handoffs per invocation, **7.6 µs**, paid whether the program
is one bind or ten thousand. ZIO's `unsafe.run` starts the fiber on
the calling thread and returns its exit directly when it finishes
without suspending (`Runtime.scala:143-165`); okay runs inline. kyo's
`.eval` is free and its async entry pays the same 7.2 as cats.

So a cats row of 140 (§4) is 132 of cats and 8 of entering it, and a
row of 153 (§1) is 5% floor. Nothing here changes an ordering; it was
still never stated, and a floor nobody has measured is a number
nobody can subtract. (ZIO's fiber DOES leave the caller thread once
it passes `MaxOperationsBeforeYield = 10240` operations,
`FiberRuntime.scala:1503`, so the 10k-bind chain in §1 pays one
handoff on the ZIO side too; the floor lane is one operation and
cannot show it.)

## 1. Bind chain — 10k left-nested flatMaps, built and run

| **Okay Eager** | kyo | **Okay Cont** | **Okay Free** | cats Free | cats Eval | cats IO | ZIO | atnos |
|---|---|---|---|---|---|---|---|---|
| **5.5** | 60 | **95** | **112** | 117 | 153 | 163 | 193 | 286 |

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

**The cats row is 15% node kind, not runtime (benchmark-fairness-
audit, 2026-09-06).** The lane binds `IO(x + 1)` — what a cats user
types, and a `Delay` node carrying a thunk — against `Cont.Pure(x +
1)`, a value. The like-for-like cats twin of `Pure` is `IO.pure`, and
with it the row reads **124.1 ±2.9 against 149.1 ±4.0** for the
`IO(...)` spelling, same run, quiet box; `okayCont` 87.3 ±7.1 in that
run. The 1.7x above is therefore 1.4x with the node kinds matched.
ZIO's equivalent (`Exit.succeed` for `ZIO.succeed`) moves 5% and
within its bars. The table keeps the `IO(...)` spelling because it is
the one people write; this paragraph is so nobody reads the gap as
all interpreter.

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
| **Okay** (while+var) | **104** | **109** | **1.05x — matched** |
| **Okay** (recursion) | **104** | **62** | **0.60x — faster** |
| kyo-direct (recursion) | 56 | 176 | 3.1x |
| zio-direct (recursion) | 196 | **127** | **0.65x — faster** |

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
| right-nested (recursion) | **0.35** | **46**† | **79** | | | | 253 |
| left-nested (foldLeft) | | | **116** | 258 | 346 | 1469 | 382 800* |

(† the ctx instance is measured at 1 000 binds — 4.4 µs, scaled ×10
here — because the chain is stack-bounded at ~2-5k binds; see the
paragraph below.)

(atnos's left-nested Reader reads 1460 here against the 3123 this
table carried from an older session — the largest single move in this
refresh, and NOT investigated: different tree, different compiler,
different host. It is recorded, not explained.)

| Writer | **Okay** | cats WriterT/Chain | atnos | kyo Emit |
|---|---|---|---|---|
| right-nested (recursion) | **159** | | | 178 |
| left-nested (foldLeft) | **217** | 1222 | 3385 | 375 400* |

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
| 615 | **1645** | 4185 | 5487 |

The one handler that is genuinely MULTI-SHOT: the continuation runs
once per alternative. Okay's `runChoice` folds `k(x)` over the
alternatives on the Free tree directly; 2.8x from the bare-List
floor is the price of programs-as-values here, and it still halves
kyo. (Multi-shot is where one-shot-optimized runtimes can't follow:
this handler cannot be expressed with relay OR exceptions.)

## 4. Fork/join — 100 trivial fibers

| raw Loom (floor) | kyo | **Okay** | ZIO | cats IO |
|---|---|---|---|---|
| 21.8 | **18.5** | 24.0 | 46.6 | 121 |

**The twin this table was missing, added by the 2026-09-08 audit.**
`okaySpawn` forks and joins K fibers from OUTSIDE the runtime — K
crossings of the boundary — while every competitor lane enters its own
runtime ONCE and does all K inside (`parTraverse` in one
`unsafeRunSync`, `foreachPar` in one `Runtime.unsafe.run`,
`parallelUnbounded` in one `runAndBlock`). §4b hit exactly this at
K = 10 000 and had okay losing 3.1x to kyo until the two shapes were
separated. §4 had the same pairing and no twin to separate it with.

Measured: `okaySpawnInside` — one entry, 100 fibers inside, joined
asynchronously — reads **35.8**, WORSE than the 24.0 above. So at
K = 100 okay's outside shape is genuinely its better one and the
table's number stands. The twin is kept because a number nobody has
compared against its alternative is a number on trust, and at
K = 10 000 the ordering reverses (§4b: inside 796, outside 1957).

kyo's 18.5 is under the raw-Loom floor and 1.3x under okay's best
here. That is a real loss on this shape and it is stated as one.

**Why Okay's number.** There is almost no Okay here — that is the
design. A fiber IS a virtual thread; spawn is `Thread.startVirtualThread`,
join parks. No fiber runtime of our own means nothing added over the
floor but 8us of bookkeeping. ZIO and cats IO pay their own
schedulers, run-loops and interruption protocols; kyo sits close to
the metal too (its scheduler is excellent) — we simply refuse to
compete by NOT having one.

**The 8 µs did not reproduce (close-the-gaps, 2026-09-06), and this
refresh puts a number on what IS there.** That session read `okaySpawn`
19.6 against `rawLoom`'s 19.1 — about 5 ns of bookkeeping per
fork/join, not 80. bench-refresh 2026-09-08 reads **24.4 against the
floor's 21.7: 2.7 µs over 100 fork/joins, so ~27 ns each**, and kyo at
18.3 is now UNDER the raw-Loom floor rather than beside it. The honest
ratio to quote is 1.13x the floor, not 1.0x and not the 8 µs the
paragraph above still says. An attempt to shave the join — parking on
the
`CompletableFuture` directly instead of through the fiber's
callback and a slot — measured WORSE, 19.6 → 22.3 in every round
(`get()` spins before it parks), and was reverted.

## 4b. Adversarial lanes — the rows we expected to lose

Three lanes chosen because the predictions in the claim said we lose
them (`compare/src/jmh/scala/okay/AdversarialBenchmark.scala`,
`-f 1 -wi 3 -i 5`, sbt-free launcher, quiet box, us/op, ±error):

**This table's first row was a MISMATCHED PAIR for as long as it has
existed, and the sixth instance of the one mistake this page keeps
making** (forkjoin-pairing, 2026-09-08). `forkJoin10k_okay` spawns
10 000 virtual threads and joins them from OUTSIDE the runtime;
`forkJoin10k_kyo` is `Async.parallelUnbounded` inside `runAndBlock`
and never leaves kyo's. Both numbers are true, of different questions,
and Lane Rule 2 below forbids putting them in one row. The matched
lanes were already in the file on both sides — `forkJoin10k_kyoOutside`
and `forkJoin10k_okayOwnInside` — so this is a reading error, not a
missing measurement. Asked properly it is two rows, and okay is ahead
in both:

| fork/join 10 000, `work=100` | okay | raw Loom | zio | cats | kyo |
|---|---|---|---|---|---|
| **spawned and joined from OUTSIDE** | **1957** | 2661 | 3503 | 2730 | 33 900 |
| **spawned INSIDE, runtime-native** | **796** | — | — | — | 884 |

At `work=10000` the inside pair is okay **3218** against kyo **26 260**
— 8.2x — because kyo's answer never spreads and okay's decides to.
That decision is the point: the self-deciding `Owned` reads 796 / 3218
where its own fixed policies read 709 / 27 640 (`forShortTasks`) and
2618 / 3200 (`forLongTasks`). Best of both, at a 12% premium over
either at the end where that one wins.

The rest of the block, unchanged in pairing:

| lane | okay, one channel | okay adaptive | zio | cats |
|---|---|---|---|---|
| many-to-many 4×4 | 4720 | **2986** | 3076 | 4260 |
| many-to-many 16×16 | 9199 | **2256** | 6723 | 8682 |
| cancel 1 000 parked fibers | 1116 | — | 887 | **748** |

(All four now hold **1024 slots**. That sentence is the whole
correction: `manyToMany_okayAdaptive` used to be
`parts(16).each(1024)` — **16 384** slots against zio's and cats'
`Queue.bounded(1024)`.)

(bench-refresh 2026-09-08, `work=100`, minimum of three rounds. **Read
this block as ranks, not as measurements**: every lane in it except
kyo's carries 15–25% round-to-round spread on a 14-core box — `okay`'s
fork/join rounds were 2715 / 3197 / 1957 — because ten thousand fibers
on fourteen cores is a scheduling experiment, not a microbenchmark.
kyo's own rounds sat inside 5%, which is itself the finding: its
scheduler is the steady one here.)

**The cancel row is the same asymmetry, one step milder.**
`cancel1k_okay` runs on Loom, where a cancel is a thread INTERRUPT;
cats does its thousand cancels inside `unsafeRunSync`, entering its
runtime once. okay's matched lanes are the pool ones, and the comment
over them in the benchmark says so outright — "a cancel is a CAS on
the fiber's own cell rather than an interrupt of a thread — §4b blamed
the interrupt, and this is the lane that says whether it was right".
It was right: `cancel1k_okayOwn` reads **750** against cats' 748, a
tie, and `cancel1k_okayDrive` reads **597**, the best number in the
block. The 1116 above is the price of Loom's interrupt, which is a
real number for the default scheduler and not a defect of the
cancellation protocol.

**A correction to this section, from the day after it was written.**
The adaptive row above said 870 and 1042, "3.6x and 6.1x AHEAD" of
ZIO. It had **16x ZIO's buffer** — `parts(16).each(1024)` is 16 384
slots against `Queue.bounded(1024)` — and I wrote that note without
checking. Matched at 1024 slots it reads 2986 and 2256: a TIE with ZIO
at 4×4 (2986 against 3076, 3%) and 3.0x ahead at 16×16. Still the
answer this library ships for many producers, still worth knowing, and
a third of the advantage claimed.

What survives unchanged: the single-channel row is the like-for-like
against ZIO's single queue and okay loses it, 1.5x at both shapes. And
on cancellation `cancel1k_okayDrive` reads **597** where the default
scheduler reads 1116 — the drive scheduler wins its own lane by 1.9x
and beats every competitor here.

Predicted: lose all three. Measured: lose two, and sit on the floor
for the third.

- **Fork/join is the JDK pool's floor, not Loom's and not the
  thread's** (drive-scheduler-jvm, 2026-09-07; the first reading of
  this row said "kyo's fibers are not threads", and the operator
  asked the right question: okay has schedulers of its own too —
  `Schedulers.forkJoin` on a pool, `Async.Drive` on JS where a fiber
  is a continuation and nothing else). Measured, medians of four to
  six quiet runs, `-f 1 -wi 3 -i 5`, us per 10 000 fork/joins:

  | lane | us | ns per fiber |
  |---|---|---|
  | kyo, the step deferred into the fiber (`IO(step)`) | 792 | 79 |
  | raw `ForkJoinPool`, external submit, no okay | 1871 | 187 |
  | raw `ForkJoinPool`, forked from inside a worker | 1910 | 191 |
  | okay `Schedulers.drive` — fiber, task and promise one object | 2310 | 231 |
  | okay `Schedulers.loom` (the table above) | 2712 | 271 |

  So: the JDK pool itself costs 2.4x what kyo's scheduler costs per
  small task, external and internal push alike (submission is not
  it — signalling and parking workers is); okay's layer over the
  pool is 25 % (a `DriveTask` walk, a cell, a closure); the virtual
  thread is 15 % more. Two attempts refuted on the way: separate
  Promise + Drive + Runnable + Fiber objects against the fused
  `DriveTask` (2417 vs 2310 — allocation count is 4 %, not the gap);
  one latch instead of 10 000 joins (3209 — worse: the counter is a
  contended line, and the joins were never the cost). kyo's lane
  had to be corrected first: `(step(i): Int < Any)` is a VALUE, so
  the table's form computed every step on the caller before
  `parallelUnbounded` forked 10 000 finished values — kept as
  `kyoEager` (830); the deferred form is FASTER (792) because the
  steps run in parallel. A kyo-class number here needs a scheduler
  that owns its threads (BACKLOG `own-scheduler-jvm`); `DriveTask`
  is the fiber object it would schedule.
- **kyo's 3.3x at fork/join is one column of a two-column table, and
  the other column is 7x the other way** (schedulers-family,
  2026-09-07). The row above forks 10 000 fibers each doing ~100
  integer ops — about 30 ns of work. That is a measurement of
  SCHEDULING, and kyo wins it by not scheduling: `runAndBlock` puts
  the forking program on a worker, `Worker.current` puts every child
  on that same worker's queue, and the whole burst runs there, in
  order, with no signal to anyone (the stack profile: 87 % of kyo's
  threads parked the entire run, one worker in `IOTask.eval`). The
  JDK pool does the opposite — `scan → signalWork → unpark` for
  nearly every task. At 30 ns of work per fiber, waking a core costs
  more than the work.
  
  Give each fiber 10 000 ops (~2.5 us) instead — real work, the same
  10 000 fibers — and the same choice reverses:

  | lane (us per 10 000 fork/joins) | 100 ops each | 10 000 ops each |
  |---|---|---|
  | kyo `parallelUnbounded` | **778** | 25 331 |
  | okay `own`, forked inside a fiber (kyo's form) | 845 | 26 589 |
  | okay `drive`, forked inside a fiber (kyo's form) | 894 | **3 447** |
  | okay `own`, forked from outside | 2 083 | **2 877** |
  | okay `drive` (JDK pool), forked from outside | 2 284 | 3 620 |

  25 ms is 10 000 × 2.5 us: kyo ran the whole burst on ONE core, and
  a pool that spreads it over the machine is **7.3x faster on the
  same fibers, same form of call** (3 447 against 25 331), 8.8x
  against the external form. Neither number is a defect and neither
  is a win: they are two policies, each right for one shape, and the
  benchmark that quotes only the first column is measuring what it
  chose to measure. Both columns are in `AdversarialBenchmark` under
  `@Param work`, so a lane cannot quietly return to one of them.

  **Can kyo spread when it needs to? Yes, and it cannot reach the
  spreading path from its own idiom** (measured 2026-09-07, both
  lanes' error under 6 %). `Scheduler.schedule(task, submitter)` has
  two paths: called from INSIDE a worker it puts the child on that
  worker's own queue (`Worker.current`), and called from outside it
  picks the least loaded of a random sample and wakes it. Its rebalancing
  (`checkStalling` -> `drain`) fires only when the CURRENT TASK has
  been running longer than `timeSliceMs`, 10 ms by default; a burst of
  2.5 us tasks never trips it, and an idle worker does not steal —
  it exits its loop and waits to be handed work. No flag changes this:
  `coreWorkers` is already the core count and those workers are simply
  asleep.

  Forking the same fibers from OUTSIDE a worker takes the other path,
  and the numbers show it working — and show why it is not a way out:

  | kyo, 10 000 fibers | 100 ops each | 10 000 ops each | added |
  |---|---|---|---|
  | `parallelUnbounded` under `runAndBlock` (inside) | **829** | 25 222 | +24.4 ms |
  | `Async.run` per fiber from the caller (outside) | 26 951 | 31 532 | **+4.6 ms** |

  The last column is the proof: 10 000 fibers × 2.47 us of added work
  is 24.7 ms of work, and the outside form absorbed it in 4.6 ms of
  wall clock — about 5.4 cores. The scheduler spread it. But the same
  form costs 2.7 us PER FIBER before any work at all (26 951 at
  work=100 against 829), because that path joins through
  `Fiber.block` per fiber instead of `parallelUnbounded`'s single
  completion counter. So kyo has both behaviours and no way to ask for
  the good half of each: the cheap join keeps the work on one core,
  the spreading placement costs more in the join than the spreading
  saves.

  **The comparison, one run, every lane** (2026-09-07, accepted only
  when the run's own error bars were tight; the two marked ~ are 13 %
  and 16 %, the rest are 1-4 %):

  | us per 10 000 fork/joins | 100 ops each | 10 000 ops each |
  |---|---|---|
  | kyo, its own idiom (forked inside a worker) | 776 | 25 379 |
  | **okay `drive`, the same form** | **782** | **2 954** |
  | okay `own`, the same form | 874 | ~14 054 |
  | okay `own`, forked from outside | 1 249 | **2 430** |
  | okay `drive`, forked from outside | ~2 206 | 3 729 |
  | kyo, forked from outside | 27 495 | 31 863 |

  Read the first two rows together: called the way kyo calls itself,
  okay's `drive` scheduler is within 1 % of kyo on the scheduling
  measure AND 8.6x faster on the real-work one. That is the whole
  answer to "kyo is 3.3x ahead": it was ahead on one shape, in one
  form of call, against okay's EXTERNAL form. Paired properly, the
  JDK pool okay already ships wins the pair — because forking from
  inside a pool worker pushes onto that worker's own deque, which is
  what made kyo fast, and the pool then spreads what the deque cannot
  drain, which is what kyo cannot do.

  **`Schedulers.own` holds both columns, and picks the column itself**
  (one tight run, every error 1-3 %; the presets pin the decision the
  default makes for itself):

  | us per 10 000 fork/joins | 100 ops each | 10 000 ops each |
  |---|---|---|
  | kyo, its own idiom | 880 | 27 097 |
  | **okay `own` — the default, deciding** | **750** | **3 645** |
  | okay `own.forShortTasks` — never spread (kyo's policy) | 674 | 26 430 |
  | okay `own.forLongTasks` — spread at once (the pool's policy) | 2 038 | 3 678 |
  | okay `drive` (the JDK pool) | 785 | 3 021 |

  The two preset rows ARE the two runtimes this table has been
  comparing, in one scheduler: pin the decision to "never spread" and
  it behaves like kyo (674 / 26 430), pin it to "always" and it
  behaves like a pool (2 038 / 3 678). The default row is the point —
  within 11 % of the better preset in each column without being told
  which case it is in, and ahead of kyo on both (15 % at 30 ns a
  fiber, 7.4x at 2.5 us).

  It reads the work rather than being told about it: at every
  sixteenth task a worker asks two questions over two different
  spans — have I been busy longer than `helpAfter` since this run of
  work began, and are my LAST sixteen tasks averaging more than
  `spreadAbove`? Both true, it wakes one sleeper. The second span
  matters: the fiber that forks a burst is itself a long task, and
  averaging from the start of the run made every burst look expensive
  and flipped the decision between iterations (1 538 / 5 389 with
  25-45 % error, against 750 / 3 645 with 1-3 % after).

  Three defects were found on the way and all three were in the
  waking, not the queues. An "active prefix" unparked only the worker
  at its edge, so a worker that had parked earlier slept for ever —
  counters showed ONE activation and two workers sharing 10 000 tasks
  (14 054 us). Per-worker inboxes with a random victim woke a SLEEPING
  worker for nearly every external submission (1 249 -> 5 822 us);
  one shared submission queue, signalled only when nobody is awake,
  put it back. And a benchmark with three owned schedulers alive at
  once was measuring its own thread count, which is how
  `Schedulers.Running` got a `close()`.

  The scheduler is chosen and tuned the way a queue is —
  `Schedulers.own.workers(4).forLongTasks.build`,
  `Schedulers.adaptive.build` — and `docs/schedulers.md` is the page
  for it; the two preset lanes below are the two ends of the one
  decision the default makes for itself.

  What okay does with that, and it is the point of the whole
  exercise: the policy can be one scheduler's, not two. `Schedulers.
  own` keeps the work at home while its queue drains fast and wakes a
  neighbour only when a worker is still busy after a threshold — the
  helper rule, measured next.

- **Many-to-many is one ring and one park per blocked hand-off — so
  do not use one ring.** All P producers and C consumers contend on a
  single Vyukov ring's head and tail, and every blocked side parks a
  virtual thread where ZIO suspends a fiber. The partitioned buffer
  (`Queues.strong.adaptive`, one part per producer) removes the first
  half of that, and with the P x C deadlock fixed it is now correct
  under contention (`adaptive-p-x-c-deadlock`, closed 2026-09-07: the
  channel was waking senders on the part its shared SCAN CURSOR named
  rather than the part a pop had just freed). Medians of within-run
  ratios over six runs:

  | many-to-many, one channel | vs okay's ring | vs ZIO | vs cats |
  |---|---|---|---|
  | `Queues.strong.adaptive`, 4x4 | **0.63** | **0.93** | 0.64 |
  | `Queues.strong.adaptive`, 16x16 | 0.77 | 0.98 | 0.68 |
  | okay's ring default, 4x4 | — | 1.43 | — |

  So the row is won by the buffer and lost by the default, and the
  default stays anyway: at ONE producer the partitioned buffer costs
  about 15 % over a plain ring (144 against 122 in the last tight run;
  a thread-local cache of the producer's own buffer took that from
  18 % to about 15 %, which is not a gain worth claiming). One
  producer is what a channel usually has, so `Channel.apply` keeps
  the ring and the four- and sixteen-producer wins — 136 against 704,
  98 against 3 138 — belong to a channel that asked for them.


  | cancel 1 000 parked fibers | us | ratio to cats |
  |---|---|---|
  | okay on `drive` | **570** | **0.77** |
  | okay on `own` | 700 | 0.94 |
  | cats | 745 | 1.00 |
  | ZIO | 845 | 1.13 |
  | okay on `loom` (the row as it stood) | 1149 | 1.54 |

  The absolute column is the MINIMUM of five rounds — this box has
  been shared all day, and a minimum is the closest thing to an
  uncontended cost when the mean is contended; where a tight run did
  land, the two agree (the producer sweep below reads 122 either way).
  The ratio column is that minimum against cats in the same table; the
  medians of within-run ratios over seven runs said 0.70, so the row
  holds either way.

  Two defects had to be fixed before the row could even be measured,
  and both were contract bugs rather than speed: a cancelled
  `DriveTask` never answered at all, so a `join` on it would have
  waited for ever while loom's returned a `Left`; and `Platform.block`
  read "the value arrived" before "I was interrupted", so an answer
  that arrived after a cancel still became the fiber's answer. Both
  are laws now.


**What was done about the many-to-many row, and why the default did
not change (adversarial-lanes, 2026-09-06/07).** The operator asked
for the adaptive buffer as the default channel. It was made so, and
the law that decision required — P producers × C consumers, one
channel, every consumer sees the end, every element exactly once —
found three defects in a day, all fixed and each now under a law:

- `AdaptiveFifo` opened a part at the producer's CLAIM index but every
  scan walked `0 until open`, a COUNT; one producer slow between
  claiming and opening left a whole output in a slot nobody scanned
  (16×16: late producers' elements lost entirely). The slot a
  producer opens is now the count `open` had before it.
- `seal` placed one end mark per CALLER, not per part (83 seals for
  16 parts; 11 019 elements unread when every consumer had been told
  the stream was over). A CAS claim fixed that and opened a second
  window — a refused push and a concurrent seal crossing left a part
  unsealed for good, a consumer parked on a closed empty channel —
  closed by a three-state seal (`TestChannelLaws` had a hang in one
  run of two; six of six after).
- the channel's waiter queue was woken from the live queue instead of
  a snapshot, so a receiver that re-parked was re-woken for ever: the
  100 % CPU `close → wakeAll → receiveAsync` that had looked like a
  livelock in the buffer.

With all three in, one round of the ring-against-adaptive A/B on the
default's own paths read, same run, quiet box:

| lane | ring default | adaptive default |
|---|---|---|
| `manyToMany_okay` 4×4 | 4838 ±233 | **2502 ±69** (zio 3105) |
| `okayChannelForeach_chunkNative_runForeach` | **18.2 ±0.3** | 427 ±768 |
| `okayChunked` (merge, 256) | **216 ±3** | 2509 ±827 |
| `bufferPerElement` / `bufferDrained` | **552 / 177** | 2218 ±1385 / 714 ±1711 |
| `zioChunked` (control) | 73 | 620 ±583 |

The control moved 8x in the adaptive arm, so that arm is not a
measurement — it was taken under page-outs — and the second round's
adaptive arm **deadlocked** in `manyToMany_okay` 4×4 after two warmup
iterations: four consumers parked on "empty", one producer parked on
"full", the fourth face of the same family and not yet named (dump
and reproducer in BACKLOG, `adaptive-p-x-c-deadlock`). So the ring
stays the default. What stands from the exercise: the P×C win of the
partitioned buffer is real and now correct where the laws reach; the
per-part capacity on single-producer paths is unmeasured (one bad
round is not a number); and the buffer has one more race to find
before it is a default anyone should ship.

## 5. Stream pipeline — map/filter/take(1000)/sum

One session, every lane of `StreamOpsBenchmark` (bench-refresh,
2026-09-08): each library's CHUNKED source beside its per-element one,
the ratio to the Iterator floor of the same run. Times are the minimum
of three rounds; bytes come from a `-prof gc` pass of the same tree in
the same session, and allocation does not drift with host load the way
time does.

| lane | us/op | vs floor |
|---|---|---|
| Iterator (floor) | 15.21 | 1x |
| **Okay Staged** — whole-stage inline pipeline | **1.70** | **0.11x** |
| **Okay chunked, one chunk** — the competitors' chunking | **8.22** | **0.54x** |
| **Okay chunked, default 64** — `Chunks.map/filter/take/fold` | **10.21** | **0.67x** |
| fs2 `Stream.emits` (pure, one chunk) | 21.9 | 1.4x |
| **Okay elements** — the `.elements` door | 24.5 | 1.6x |
| ZIO `ZStream.range` (4096 a chunk) | 35.8 | 2.4x |
| Okay iterator | 54.2 | 3.6x |
| kyo `Stream.range` (4096 a chunk) | 65.9 | 4.3x |
| Okay LazyList / Producer | 171.8 / 181.8 | 11x / 12x |
| kyo singleton emit | 266 | 17x |
| ZIO `ZStream.iterate` | 700 | 46x |
| fs2 `Stream.iterate` | 1510 | 99x |

**THE CHUNK SIZE WAS NOT THE SAME ON BOTH SIDES, and it ran against
us** (lane-fairness, 2026-09-08). Every competitor lane here gets the
whole 3 004-element stream as ONE chunk — `fs2.Stream.emits` by
construction, `ZStream.range` and kyo's `Stream.range` because their
4096 default is bigger than the input. okay's lanes used
`Chunks.nats`'s default of **64**, so they crossed 47 chunk boundaries
where the others crossed none. Nobody noticed for months because okay
won the table anyway.

Both are now here. The default costs **24%** (10.21 against 8.22), and
that is the honest price of a chunk size a caller gets without asking
for one. Quote the 64 row when the question is "what do I get by
default" and the one-chunk row when the question is "how does the
mechanism compare" — the second is the one the rows beneath it answer.

(The B/op column is dropped from this run: the `-prof gc` pass was not
repeated after the audit, and carrying allocation figures from a
previous tree beside times from this one is exactly the mixing this
page keeps having to correct. `chunked-source-sweep`'s bytes are in
history.tsv and were 112 for Staged, 116 192 for chunked, 108 872 for
the floor.)

(The previous version of this table mixed three sessions — Staged from
one, kyo `Stream.range` from another, ZIO's and fs2's chunked sources
from the fairness audit's own. Every ratio it reported survives this
single-session re-measure within a tenth: Staged 0.11x both times,
Okay chunked 0.68x → 0.73x, kyo chunked 4.4x both times.

`Okay iterator` is the one row worth a warning to whoever reads this
next. Across the three rounds it read 126.2, then 94.9, then 57.3 —
the box settling — and after one round it looked like a 2.2x
regression. It is not: the minimum agrees with the 49.2 this table
used to carry, once you allow that this is a different tree on a
different compiler. Do not quote this suite from a single round.)

**What it measures.** The bread-and-butter stream pipeline in each
library's fastest mode.

**Why Okay's numbers, mode by mode.** This table is one design
principle at four price points:

- `toLazyList` (145): the memoized, re-observable bridge — you pay
  for the caching.
- `.iterator` (49): linear, fused, consume-once — a specialized
  tree-walk with a mutable cursor, no Option/tuple per element
  (measured −44% over the generic unfold when introduced).
- Chunks (22.5 / 9.5): the tree steps once per CHUNK; an element
  costs an array index. Transformers are chunk-in/chunk-out array
  passes, and the transformer form now reads UNDER the Iterator
  floor; what the elements door pays over it is the per-element
  cursor, not the tree.
- `Staged` (1.55, 112 bytes): when the pipeline's shape is known where it is
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
at ~1x.

**The chunked sources, found 2026-09-06 (benchmark-fairness-audit)
and in the file since chunked-source-sweep.** The first version of
this table priced ZIO and fs2 from their worst source only.
`ZStream.range` is 20x under `ZStream.iterate` in the same run;
`fs2.Stream.emits` is 63x under `fs2.Stream.iterate` — and PURE: an
fs2 pipeline with no effect compiles synchronously, no runtime and
no `unsafeRunSync` at all. fs2's `range` is `emit(o) ++ go(o + step)`
(Stream.scala:3981-3993, 3.10.2), a singleton chunk per element, and
its own scaladoc says to use `emits` for one chunk. The audit
measured these in a session of their own (36.7 and 19.8 against a
floor of 14.3); the table above has them in the session with
everything else.

With their sources the way their authors intended, ZIO sits at 2.3x
the floor and fs2 at 1.5x — beside Okay's elements door at 1.6x and
its chunked transformers at 0.68x, not two orders of magnitude
behind them. The `iterate` rows stay in the table as what they are:
the price of a per-element source in a chunked runtime, each
library's worst case. They are not the library. The bytes column
says the same thing the time column does, with less noise: the
three fair chunked sources allocate 150–340 KB for the pipeline,
the three per-element ones 1.6–9.9 MB.

## 6. Merge — two 500-element streams by readiness

| **Okay chunked** | ZIO | fs2 chunk-native | Okay elementwise | fs2 singletons |
|---|---|---|---|---|
| **13.3** | 51.5 | 94.4 | **122** | 10 746 |

(Re-measured after `Channel.apply`'s default became `growing`
(default-retable, 2026-09-08). `Okay elementwise` — `Source.merge`,
which runs two producers into one channel — went 130 → 125 → 122
across the two changes that touched it.)

(bench-refresh 2026-09-08, N=500 a side. `Okay elementwise` reads 130
against the 308 this table carried, and `ScalingBenchmark` agrees at
the same N (122.5). The paragraphs below are the record of an
investigation that ran when the number WAS ~300; they are dated and
kept, but the row they were explaining has moved and nothing here has
re-run that investigation to say why.)

(fs2 re-paired 2026-09-06, `fs2-chunked-merge-lanes`, same run and
N=500: `Stream.emits` a side — fs2's own one-chunk source — reads
**84.0 ±0.9**; the `Stream.range` spelling, which is a singleton chunk
per element in 3.10.2, reads 8965 ±61 in that run, and the okay and
ZIO ties 10.9 / 44.6 against the 10.7 / 45 recorded. 107x inside fs2,
from the source alone. Okay chunked is 7.7x ahead of fs2 asked
fairly, not 830x.)

Readiness-merge is what zip and ++ cannot express: a fiber per
source feeds one channel, the loser of every race simply arrives
later. Chunking the STREAM (not the queue — a chunked-queue variant
was tried and REFUTED, it's in history.tsv) beat ZIO's own
chunk-aware merge 3.2x. fs2's number is its worst case (singleton
elements through its concurrency machinery), stated as such.

**And fs2's fair spelling, measured (benchmark-fairness-audit,
2026-09-06, N=2000 a side, same run, quiet box):** chunk the sources
BEFORE the merge — `Stream.emits(range).chunkN(256).unchunks` on each
side — and fs2 reads **281 ±7** where the singleton spelling reads
35 700 ±169 at that N; `ZStream.range` merging in the same run 59.0
±1.1. A 127x difference inside fs2, from where the chunking sits.
`ChunkFlushBenchmark`'s `fs2Chunked` lane chunks AFTER the merge, so
the merge itself still sees singletons — filed to fix at this
table's N=500 rather than guessed by scaling.

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
| `rawLazyListDrain` (control) | 14.0ns | 14.0 | 13.5 | 13.0 |
| `sourceSingleDrain` | 49.4ns | 47.3 | 49.3 | 54.7 |
| `channelMerge` | 88.8ns | 82.5 | 83.5 | 92.0 |
| `sourceMerge` | 139.2ns | 119.0 | 118.9 | 115.3 |

(bench-refresh 2026-09-08. `ScalingBenchmark`'s `n` is elements PER
SIDE and both sides are drained, so the per-element figure is the lane
divided by 2n; its params are 250/500/1000/2000, which is what the
four columns are. The shape is what this table exists for and it is
unchanged: flat across an 8x range, so the tree is linear and there is
no quadratic to remove. The levels are lower than the 2026-09-02
sweep this table used to carry — same conclusion, different tree.)

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

**These rows predate the default change and are being re-measured
lane by lane; the ones below carry their 2026-09-06 values.** What is
already known from `default-retable` (2026-09-08), against a noise
floor of 5.9% established from 54 lanes that cannot be affected by the
change:

| lane | ring default | `growing` default | |
|---|---|---|---|
| `chunksMergeSize1` (k = 16 / 256 / 1024) | 664 / 626 / 640 | **386 / 380 / 393** | **−39%** |
| `okayElementwise` (k = 256) | 539.9 | **423.2** | **−21.6%** |
| `elementwiseFromRange` (k = 1024) | 464.6 | **368.0** | **−20.8%** |
| `okayChunked` (k = 16) | 339.3 | 292.8 | −13.7% |

Nothing regressed that can be shown to have regressed.
`okayChunkedFlush` reads 10–12% higher, and that is NOT claimed as a
regression: `sourceSingleDrain`, a lane with no channel in it at all
and therefore untouchable by this change, moved 15.1% in the same
pair of runs. For these contended lanes the honest band is ~15%, not
the 5.9% the single-threaded ones support, and 12% sits inside it.

| 2x2000 elements | okay | ZIO | fs2 (re-paired 2026-09-06) | fs2 as first measured | reads as |
|---|---|---|---|---|---|
| chunk-native (each library's own default) | **24.0** | 146 | 134 | 49 150 | comparison |
| chunked at a matched size (16) | **293** | 146 | 2 912 | 43 540 | comparison |
| chunked + timed flush | **382** | 5 591 | 15 803 | 57 120 | comparison |
| per-element (chunk of one, forced on ZIO/fs2) | **446** | 11 668 | 40 586 | 42 640 | diagnostic |

(Re-measured 2026-09-08 under the `growing` default, k = 16, minimum
of three rounds. The previous values were from 2026-09-06 and were the
last stale table on this page — found by the operator asking whether
the numbers had actually been updated, which they had not. The
`chunked at a matched size` row is now a TIE with ZIO rather than 1.8x
behind, and the timed-flush row is 15x ahead rather than 20x.)

**The fs2 column was the same methodology bug a fifth time, and this
section had already named it four (fs2-chunked-merge-lanes,
2026-09-06).** Every fs2 lane was fed from `fs2.Stream.range`, which
in 3.10.2 is `emit(o) ++ go(o + step)` (Stream.scala:3981-3993) — a
singleton chunk per element — and the "matched size" lane put its
`chunkN(k)` AFTER the merge, so the merge saw 2x2000 singletons
regardless of k. So "fs2 chunk-native" measured a fold over
one-element chunks, and "fs2 at 16" measured the same merge as the
per-element row plus a regroup. Fed from `Stream.emits` (fs2's own
one-chunk source, per its scaladoc) and chunked BEFORE the merge, the
column becomes the one on the left; the old column stays on the right
as what it was. The ZIO and okay ties in the re-pairing run agree with
the recorded values (`zioChunked` 129.4/70.9/77.0 against 126.4/72.4/
68.5, `okayChunksNative` 24.5/21.9/22.3 against 29.4/31.8/24.8,
`zioPerElement` 10 021 against 10 033), so the column moved and the
table did not.

What the honest column says: **fs2 chunk-native at 109 is slightly
AHEAD of ZIO's 126 and 4.9x behind Okay's `Chunks`** — the one row
where fs2 was ever within an order of magnitude was the one nobody
had measured. At a matched 16 it is 2373: fs2 genuinely pays about a
microsecond per chunk through `merge` (142 at 1024, 293 at 256, 2373
at 16 — linear in the chunk count), which is a real price and now a
stated one rather than a strawman. `groupWithin` at 14 597 is
genuinely expensive — a timer per group — and 3x ZIO's
`groupedWithin`, which is itself 20x Okay's `flushAfter`.

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

**Timed flush is okay's by 15x** (382 against 5 591), which says more
about `groupedWithin` than about either representation. (It read 20x
— 244.3 against 4907 — before the 2026-09-08 re-measure; the ratio
moved because okay's flush lane rose, not because ZIO's fell.)

**The forced per-element row is a DIAGNOSTIC, not a win.** Nobody
writes `ZStream(chunkSize = 1)`: it wraps every element in a one-slot
array and pays the chunk machinery on top, so 446-against-11 668
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
`groupedWithin` costs 38x its own plain `grouped` (5 591 against
146), fs2's `groupWithin` 5.4x its own `chunkN`. okay's `flushAfter`
costs **11%** over its own chunked merge (349.4 against 315.8),
because the flusher is one sleeping fiber beside the feed rather than
machinery in the per-element path. Against ZIO that is **16x**,
against fs2 **45x**.

**It cost 29% until 2026-09-09, and the reason was a leak
(flush-premium).** The two flusher fibers were forked and dropped;
`done.get` stopped them only at their NEXT tick, so a merge finishing
in 380 MICROseconds left two fibers asleep for a further second under
`flushAfter = 1000`, each holding a timer entry. At a few thousand
merges a second that is thousands of live sleepers.

The measurement that found it is worth more than the fix: a
**one-millisecond** window, which does strictly MORE work because its
timer actually fires, measured 1.14x where the thousand-millisecond
one measured 1.29x. A shorter window costing less is not something a
correct implementation can do. Cancelling each flusher when its own
source finishes — it has already flushed its tail by then — takes the
premium to 1.11x, and the 11% that remains is two forks and two timer
registrations per merge, which is real work.

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
| `Chunks.foldLeft(Chunks.fromIterator(list.iterator, N))` | **11.6** |
| `ZStream.fromIterable(list).runSum` | 58.6 |
| `Source.of(list).toLazyList.foldLeft` (kept, ours only) | 162.9 |

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

**`runCollect` — was a trade, is now a win (close-the-gaps,
2026-09-06).** The first cut did `uncons` per element and rebuilt the
rest as a new program, which the Async handler then interpreted a
second time — a Free node here and a step there for every element.
Rewritten as the one tail-recursive walk `Writer.foldWith` already
uses, split on `TypeableK[Async]` (concrete) rather than
`TypeableK[Writer % A]` (an unchecked type test at an abstract `A`):
alternating A/B, three rounds, medians **200.4 → 119.6, 0.60x** —
now 25 % FASTER than `toLazyList.foldLeft`'s 158.5 in the same runs,
where the paragraph below had it 30 % slower. That paragraph stays as
the record of what was measured before the fix.

**`runForeach` — the same fix, the same day (runforeach-one-walk).**
It had the identical double walk; a tell now embeds `f(a)` with one
`Bind` and the walk continues inside it. Alternating A/B, three
rounds, medians: **159.7 → 99.9, 0.63x** on the collection lane — now
28 % faster than `toLazyList.foreach`'s 139.5 in the same runs. The
channel lanes did NOT move (209.1 → 213.3, 1.02x), and that is the
finding: `.drained` forwards an Async operation per element, so a
`Bind` per element remains whatever the walk does — the elementwise
channel row is bounded by the channel's per-element operation, not by
`runForeach`. The chunk-native row (20.4 → 20.2) does its work inside
the chunk and was predicted not to move.

**`runCollect`/`runForeach` — added for API parity, and it was an
honest trade, not a free one (as first measured).** `Source` gained `runCollect: Vector[A]
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
| `Chunks.foldLeft(Chunks.fromIterator(...))` | **11.3** |
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

**Closed 2026-09-05 on the producer, and again 2026-09-06 in the
interpreter.** `Channel.bufferChunked(64, size = 256)(list).drained.
runForeach` read **90.4 ±2.1** against `zioChannelForeach`'s **135.6
±6.3** after the producer fix — 1.5x ahead, from 264.5 before, and the
last row where zio led anything in this table.

Then `feed-linear-view` found the feed interpreting a `Free` program
twice per element just to take the head of a list, and the same row
reads **19.7 ±0.2** (ledger) — 4.6x again, 6.8x past `zio.Queue`.

**Verified independently 2026-09-06** on a box checked quiet
(`pgrep -f sbt-launch` empty, load 2.18), which is worth saying because
the row has moved four times: **18.9 ±0.2** against `zioChannelForeach`
at **112.4 ±1.0**, a second run agreeing with the ledger's 19.664 to
within 4%. The controls in that run agree with their recorded values
too — `okayCollection_chunk_fold` 10.8 against 11.0, `okayStep_elem_
lazyList` 89.9 against 90.4, `zioStep_elem_runSum` 274.1 against 276.9,
`zioCollection_chunk_runSum` 47.6 against 49.2 — so the 4.8x is in the
code and not in the machine.

The full re-measurement, same run, N=4000:

| lane | us/op | recorded |
|---|---|---|
| `okayChannelForeach_chunkNative_runForeach` | **18.9 ±0.2** | 19.7 |
| `okayCollectionForeach_chunk_fold` | **10.5 ±1.0** | 12.8 |
| `okayCollection_chunk_fold` | **10.8 ±0.1** | 11.0 |
| `zioCollection_chunk_runSum` | 47.6 ±0.5 | 49.2 |
| `zioCollectionForeach_chunk_runForeach` | 89.6 ±9.8 | 85.5 |
| `okayStep_elem_lazyList` | **89.9 ±0.9** | 90.4 |
| `okayStep_elem_unfold_lazyList` | 101.8 ±1.2 | — |
| `okayCollection_elem_lazyList` | 148.5 ±11.0 | 154.1 |
| `okayCollectionForeach_elem_lazyList` | 150.2 ±16.2 | 165.5 |
| `okayCollectionForeach_elem_runForeach` | 162.1 ±0.4 | 169.8 |
| `okayCollection_elem_runCollect` | 201.3 ±0.9 | 188.7 |
| `okayChannelForeach_elem_lazyList` | 206.2 ±2.1 | 219.1 |
| `okayChannelForeach_elem_runForeach` | 208.2 ±1.6 | 209.3 |
| `zioChannelForeach_chunk_runForeach` | 112.4 ±1.0 | 133.0 |
| `okayChannelForeach_chunk_fold` | 338.1 ±3.5 | 318.7 |

Two rows moved against us and are left as measured rather than
trimmed: `runCollect` at 201.3 is 36% slower than `toLazyList`'s 148.5
where the recorded pair said 30%, and the `_chunk_fold` warning lane
got slower still. `Source.unfold` costs 13% over the specialised
`range` (101.8 against 89.9), which is the tuple per step the API note
above predicts.

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
| right-nested (recursion) | **15.2** | | | | 696 |
| left-nested (foldLeft) | **22.5** | **29** | 116 | 225 | 7912* |

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
| 11.7 | 16.2 | **19.2** | 35.5 | 70.7 | 175 | 268 |

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
| **49.3** | 58.4 | 58.8 | 78.0 |

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

**And that second explanation is refuted too** (chunked-lexer-bookkeeping,
2026-09-09). The per-chunk bookkeeping was rewritten away — one
traversal into a growable array instead of `Vector.newBuilder` →
`result()` → `ChunkBuf.ofSpecialized` (which, with the token kind
abstract and so no ClassTag at that site, falls to `of` and sizes and
copies again) — and measured against the old loop in one run. It buys
**1.8% of allocation at chunk 64** (485 949 against 494 902 B/op) and
**nothing at 512** (475 369 against 466 574, i.e. slightly worse). Time
could not be measured at all: two four-fork rounds on the same code
disagreed by 1.5–2x in both directions (round 2 minima 49.1/91.0 for
new/old at 64, round 3 75.6/54.8), on a box whose 5- and 15-minute
load never fell below 25. The change was NOT kept — a hot-path rewrite
that cannot show a benefit does not belong in the tree.

What the byte counts do say is where lexing's cost really is, and it
is on BOTH paths equally: **~180 bytes per input CHARACTER** (453 KB
element-wise for a 2 495-character document). `okay.lex.Json`'s
scanner state is `S(mode, buf: String, start: P, cur: P)` and every
`step` does `s.copy(buf = s.buf + c, cur = s.cur + c)` — a fresh
String per character, quadratic in token length, plus a new `S`, a
new `P` and the `Tuple2` that `step` returns. That is the lever;
`lexer-state-allocation` in BACKLOG carries it.

**Half of that lever moved** (lexer-state-allocation, 2026-09-09).
The two `P(off, line, col)` case classes are gone from the state —
`S` carries the six ints flat — and the byte counts, which do not
care what the box is doing, say it across three separate runs:

| | before | after |
|---|---|---|
| element-wise | 453 266 | **425 832** (−6.1%) |
| chunked (64) | 494 902 | **467 873** (−5.5%) |

Semantics unchanged (spans, lexemes, `key`, `rebase`, the incremental
relex laws; 11 + 130 + 12 tests green). TIME was not measurable: four
attempts over the day landed on a box whose load ran 2 → 97, and two
rounds on the SAME code disagreed by more than the effect. The
change is recorded here as an allocation result and nothing more.

The OTHER half — `buf: String` grown one character at a time,
quadratic in the token's length — is BLOCKED, and the reason is
worth writing down: the obvious fix is to carry the token's start
offset and slice the input once at `finish`, and the chunked path
cannot do it. `Lex.chunks` sees one chunk at a time and a token may
span chunks, so there is no input to slice from. Any fix has to
serve both paths; a state that holds an offset for one and a string
for the other is two scanners wearing one type.

**Parsing, full vs incremental:**

| full parse | incremental reparse (one-member edit) |
|---|---|
| 93.7 | **43.4** |

2.2x under the full parse for a one-in-fifty edit — real, and
honestly below what O(damage) suggests: the relex dominates the
reparse, and the common-prefix/suffix token scans are O(tokens).
That is the next lever if incremental parsing gets a demanding
workload; the correctness property (untouched subtrees returned BY
REFERENCE) is what the layer exists for.

**Codecs — where the contract shows up as a number:**

| | write | read |
|---|---|---|
| **Okay CBOR** | **0.465** | **1.010** |
| circe (JSON) | 0.438 | 0.558 |
| **Okay JSON** (`Json.read`, lossless) | **0.426** | **0.664** |

(bench-refresh 2026-09-08, `TextBenchmark`. The `Json.readStrict` row
this table carried is measured by `CodecBenchmark` in ns/op and lives
in the two tables below, not here.)

**The 16x this table used to charge for losslessness is GONE, and it
is a landed change, not this session's weather.** The read row was 10.3
against circe's 0.623, and the paragraph here explained that gap as the
price of the contract. Since then `json-parse-fast-road` (131cedc2 —
"the default road through the JSON parser was the slow one, by 79x")
and `json-cst-batch-road` (b4172242 — "the lossless CST road was lexing
through the effect system, one char at a time") landed. The lossless
read now measures **0.664 against circe's 0.558** — 1.19x, where it
used to be 16x. (The morning run of 2026-09-08 read 0.657 against
0.689 and this paragraph said "at parity"; the evening run puts circe
19% ahead. Both are within the run-to-run spread of a sub-microsecond
lane and neither supports a claim finer than "the same order". The
16x is what is gone.)

So: read this as a price list for CONTRACTS whose prices have changed.
Our CBOR read is 1.5x circe's JSON read — the Schema fold on its own,
next to a hand-tuned parser. Our JSON write is now UNDER circe's. And
`Json.read` — chars → total scanner → total driver → LOSSLESS CST →
projection → Schema fold — still buys damage-as-data, byte-for-byte
losslessness and a HALF-ARRIVED document that still decodes (the LLM
case), and no longer charges for it against circe's parse straight
into its AST.

The paragraphs below were written while that gap was real. They are
kept as the record, but read them with their dates: in particular the
stage breakdown (chars → CST at 95% of 39.1us) describes the road
BEFORE 131cedc2, and nothing in this session re-ran it.

**Since 2026-09-07 there is a choice.** `Json.readStrict` goes
characters straight into the `Schema` with no tree — `JsonValue`'s
strict descent driving `Cbor.get`'s walk — and gives the SAME answer
as `read` on a complete, well-formed document (`TestJsonStrict` holds
them equal over a corpus) while refusing, as `Left`, anything it is
not sure of. Same run, two forks, load falling from 11 to 5:

| | ns/op | B/op |
|---|---|---|
| `textToOrderStrictStaged` — `Staged.strict[Order]`, no tree | **347.5** | **2 320** |
| `textToOrderStrictRuntimeStaged` — the same, generated at run time | 406.6 | 2 808 |
| `textToOrderStaged` — `parseValue` + `Staged.json[Order]` | 491.4 | 2 720 |
| `textToOrderCirce` | 834.4 | 3 416 |
| `textToOrderLossless` — `Json.read` | 1 004 | 5 816 |
| `textToOrderStrict` — `Json.readStrict` | 1 104 | 6 136 |
| `parseValueOnly` — the value parser alone, tree only | 247.2 | 2 208 |

(bench-refresh 2026-09-08: times are the minimum of three rounds,
bytes from a `-prof gc` pass in the same session. The two tables this
section used to print — one for the strict door, one for the staged
one — are merged here, because every lane is now from one run and
splitting them was only ever an artefact of two sessions.)

**Two things inverted since this section was written, and both matter
more than the numbers.**

**`Json.read` is now FASTER than `Json.readStrict`** — 1004 against
1104 — where the strict door was introduced precisely because the
lossless one cost 14 264. `json-parse-fast-road` (131cedc2) and
`json-cst-batch-road` (b4172242) took the lossless road down by 14x
and straight past the door built to avoid it. Allocation says the same:
5 816 bytes against the strict walk's 6 136. So the argument for
`readStrict` is no longer speed — it is refusal, that it says `Left`
to anything it is not sure of. That is a real reason to keep it, and
it is not the reason this page gives.

**`Json.read` is also within 20% of circe** (1004 against 834), where
this section priced it at 17x. The lossless CST — every byte kept,
damage included, a half-arrived document that still decodes — now
costs about a fifth over a parser that throws all of that away.

The staged road remains the fastest, at 347.5 ns and 2 320 bytes:
**2.4x circe with 32% less allocation**, and 100 bytes over the bare
value parse. That claim survives the refresh unchanged.

**0.92x circe in time, 1.48x its bytes; 19.2x faster and 25.3x less
allocation than the lossless road.** The strict Schema walk costs
about 3.3x the bare value parse — the field map, the erased parts and
`make` — which is what a reader that knows the type at compile time
does not need. The `Staged` macro gives it that reader as its third
target (`json-strict-staged`, the same day), and the same run then
reads, two forks, load 25 → 7:

| | ns/op | B/op |
|---|---|---|
| `textToOrderStrictStaged` — `Staged.strict[Order]`, generated over the strict reader, no tree | **323.1 ±15.6** | **2 320** |
| `textToOrderStaged` — `Json.parseValue` + `Staged.json[Order].decode` of the tree | 348.4 ±5.6 | 2 680 |
| `textToOrderStrict` — `Json.readStrict`, interpreted | 764.1 ±36.6 | 4 968 |
| `textToOrderCirce` | 792.8 ±18.9 | 3 416 |
| `parseValueOnly` — the value parser alone | 225.8 ±1.4 | 2 208 |

(SUPERSEDED — this is the 2026-09-07 run, kept because the paragraphs
after it argue from these exact rows. The 2026-09-08 measurement of
the same lanes is the merged table above, and it disagrees on one
thing that matters: `Json.readStrict` is no longer the fast door.)

**The staged strict reader is 2.45x faster than circe with 32% less
allocation, and 112 bytes over the bare value parse: it reads at the
cost of scanning.** (2026-09-08: 2.4x and 32%, unchanged.)

**A correction this section owes.** The second row — `parseValue`
plus the staged decode of the tree — has been in `CodecBenchmark`
since staged-codecs and reads 2.3x circe. It was never on the price
list above, and the list's sentence "need raw speed? use circe" stood
beside it for days. `json-fast-read` measured its interpreted door at
0.92x circe and presented that as the choice; the existing staged road
was already better, and that lane did not say so. So, stated once and
plainly: the choice is three doors, not two. `Json.read` for the
lossless contract; `Json.readStrict` for a strict read with no
derivation step; `Staged.strict[A]` (or `Staged.json[A]` over
`parseValue`) for a caller who derives once and decodes many times —
and that is the fastest JSON read in this library, by 2.45x over the
external reference.

(2026-09-08: the three doors stand, the ORDER of the first two does
not. `Json.read` now reads 1004 ns against `Json.readStrict`'s 1104,
so pick `readStrict` for its REFUSAL — it answers `Left` where it is
unsure — and not for speed. `Staged` remains the fastest by 2.4x over
circe. See the merged table above.)

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
| **662** | **465** |

**Where a symbol index's time goes**, split on the same 8.5KB file:

| | us | share |
|---|---|---|
| `Code.source` — lex, parse, tree | 413.1 | 63% |
| `Symbols.of` — the walk over it | 231.6 | 35% |
| `indexFull` end to end | 657.5 | |

The walk looked like the same defect as everything else in this
section: it rebuilds the whole `Index` on every identifier it sees — a
fresh case class, a copied path through the map, a `Vector` append —
and a file of a few thousand tokens is a few thousand of each. It was
rewritten to fill mutable buckets and build the `Index` once, and the
rewrite was **reverted, because it bought nothing**: 244.1us against
245.4 before, which is no difference at all.

So the cost is somewhere else in the walk, and the quiet machine
eventually said where: `indexFoldNoRefs` 209.3 against `indexFoldOnly`
231.6 (bench-refresh 2026-09-08; 189.6 ±5.4 against 235.0 ±14.9 in the
original run) — the identifier branch is only **10%** of the walk, and
**90% is the traversal machinery itself**: the recursion,
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
| after (re-measured 2026-09-08) | 39.2 | **83.1** |

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

That is 12.9 and 13.4 MB/s warm (2026-09-08). Cold, over a real tree —
`IndexReport` on this repository, 201 files and 898KB, including file
I/O and with no JIT warmup at all — the same work runs at 1.2 MB/s
and finishes in 744ms. Quote whichever matches your question; the
gap between them is warmup and I/O, not algorithm.

**Re-indexing after an edit** — one character changed in the 8.5KB
file:

| full re-parse | incremental reparse |
|---|---|
| 419 | **114** |

3.6x, and the same honest caveat as the JSON lane: below what
O(damage) alone suggests, because the relex dominates and the
prefix/suffix token scans are O(tokens). It is the ratio that makes a
live index of a repository the agent is EDITING affordable, which is
the whole reason this layer exists.

**Chunking — the price of parsing, and what it buys:**

| structural (parsed) | windows (unparsed) |
|---|---|
| 703 | **330** |

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
| **0.56** | 11.3 | 19.3 | 18.7 | 374 |

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

| lane | before `popMany` | after | 2026-09-08 |
|---|---|---|---|
| okayElementwise | 190.1 | 212.3 | **151.5** |
| okayChunked | 182.9 | **111.6** | **54.8** |
| zioElementwise | 304.0 | 336.8 | 340.9 |
| zioChunked | 113.7 | 149.5 | 130.7 |

(The third column is this page's current run, minimum of three rounds
under the `growing` default. Both okay lanes improved — chunked by
2.0x since the middle column — and both ZIO lanes are where they
were, which is what makes the okay movement readable as ours rather
than the box's.)

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
| okayStrong — drain-on-close as an INVARIANT | 256.1 | 124.7 |
| okayWeak — close discards | 164.3 | 58.1 |
| **okayLayered — the same strong contract as a LAYER** | **157.7** | **61.0** |
| zioStrong — `Queue[Option]` | 317.1 | 137.1 |
| zioWeak — `Queue` | 309.5 | 134.6 |

(bench-refresh 2026-09-08, N=4000, cap=1024.)

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

| lane | us/op (2026-09-04) | us/op (2026-09-08) |
|---|---|---|
| okaySendBulk + chunked receive | **66.9** | 63.2 |
| okayChunked | 105.9 | 58.4 |
| okaySendElem + chunked receive | 109.0 | **54.1** |
| zioChunked | 114.8 | 141.8 |
| okaySendElem + elementwise receive | 196.7 | 134.9 |
| okaySendBulk + elementwise receive | **280.4** | 138.4 |

**"Batch both ends or neither" no longer holds, and the reason it was
believed does not either (bench-refresh, 2026-09-08).** The bulk send
was 1.63x AHEAD of the element send against a draining consumer; it is
now **17% BEHIND** (63.2 against 54.1), consistently across three
rounds. What moved is not the bulk lane — it sits where it was — but
the ELEMENT lane, which halved and overtook it.

**The obvious explanation was measured and REFUTED.** The paragraph
below blames "room": a full ring makes every bulk attempt fail its
scan and fall back to a single send. That is true of the ELEMENTWISE
consumer and remains the right reading of the last row. It is NOT what
happens here. Counted directly against a chunk-draining consumer, same
N, Cap and Batch as the lane (300 repetitions, 22 090 `sendManyNow`
calls): the scan finds room in **97.3%** of calls, the mean claim is
**55.8 of the 64 asked**, 18 364 calls take the full 64, and **0.05%
of elements** reach the per-element fallback. The bulk path is being
exercised almost perfectly and is still slower.

**It has now been profiled, and the answer is that there is no cause
to find in the mechanism (sendbulk-profile, 2026-09-09).** Both lanes
are ~60% WAITING and their RUNNABLE time is dominated by
`Ring.popMany` on the CONSUMER side — 21% and 32% of it. Neither
`Ring.pushMany` nor its scan appears in either profile at all. Two
further explanations were built and measured:

- the scan touching every slot twice — not visible in the profile,
  and the send side is not where the time is;
- the per-element wake after a bulk claim (`sendManyNow` calls
  `wakeOne` once per element admitted, sixty-four times per batch,
  of which one wakes anybody). A variant that stops as soon as the
  waiter queue is empty made the bulk lane 8.5% WORSE and the
  element lane — which does not call `sendManyNow` at all — 7%
  better, so both movements are noise. These lanes spread 64 to 92
  across rounds; anything under ~40% here is not a measurement.

So: four explanations, four refutations, and the honest reading is the
one the paragraph above already half-states. **The element path caught
up**, and `sendManyNow` no longer earns its place on this shape. The
claim that it is 1.63x ahead is withdrawn rather than defended; the
primitive stays because a producer holding a batch of elements is a
real caller, and because nothing here shows it is WRONG — only that it
is no longer faster where this page said it was.

Against an ELEMENTWISE consumer the bulk send is still a loss, and
there the room explanation stands: a consumer taking one element at a
time keeps the ring full, every bulk attempt fails its scan and falls
back to a single send, and the scan is pure overhead on work that had
to happen anyway. A batched primitive is not a free upgrade; it is a
bet that the other end leaves room — and, on this evidence, a bet that
no longer pays even when it wins.

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
| unbounded, chunked | **56.0** | 439.7 | **7.9x** |
| unbounded, elementwise | **111.3** | 496.7 | **4.5x** |
| bounded strong, chunked | **124.7** | 137.1 | 1.10x |
| bounded strong, elementwise | **256.1** | 317.1 | 1.24x |
| weak, chunked | **58.1** | 134.6 | 2.32x |
| weak, elementwise | **164.3** | 309.5 | 1.88x |
| `StmChannel` (list), elementwise | **253.2** | 317.1 | 1.25x |
| `StmChannel` (list), chunked | **121.6** | 137.1 | 1.13x |

(bench-refresh 2026-09-08, N=4000, cap=1024, minimum of three rounds.
The `StmChannel` chunked row was ZIO's by 1.6% and is now okay's by
14%. The unbounded pair, the largest gap on this page, holds.)

**`bounded strong, chunked` was never 2.24x, and the A/B that says so
is worth more than the row (strong-chunked-tie, 2026-09-08).** This
table used to read okay 56.2 against zio 125.9 there; the refresh read
136.0 against 137.5 and recorded a suspected regression WITHOUT
diagnosing it, because the tree had changed compiler in between. The
A/B was then run across the exact boundary — `0e32ed6c`, the last
commit on Scala 3.7.4, against `4ce13ec7`, the first on 3.9.0, whose
migration commit changed ZERO files under `src/main`, so the channel
sources are byte-identical and only the compiler differs. Five
alternating rounds each:

| lane | 3.7.4 | 3.9.0 | |
|---|---|---|---|
| `okayStrongChunk` | **127.2** | 138.2 | +8.7% |
| `zioStrongChunk` (control) | 142.7 | 141.4 | −0.9% |
| `okayWeakChunk` | 65.1 | 57.0 | −12.4% |
| `zioWeakChunk` (control) | 133.8 | 138.7 | +3.6% |

**On 3.7.4 the lane reads 127.2, not 56.2.** The old number does not
reproduce on the compiler it was measured with, so it was an artefact
of its own session — that table was taken at `f=3 i=8`, a protocol
this page has not repeated. The pair is 1.12x in okay's favour on
3.7.4 and 1.02x on 3.9.0; it was never 2.24x, and there is no
regression to find.

What the A/B DOES establish is a real, modest and two-directional
compiler effect: 3.9.0 costs this lane 8.7% and gives the weak one
12.4% back, with both ZIO controls inside 4%. That is worth knowing
and is not what anyone was looking for.

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

## 17. Actors and the reactive bridge — the first numbers

`okay-actor` and `okay-reactive` had none. Both sit on the channel
measured through §14–§16: an actor's mailbox IS a `Channel[M](256)`,
`tell` is `mailbox.send`, and the loop reads `receiveBlocking()` one
message at a time on purpose — supervision must know which message was
the poisonous one. `ask` builds a `Reply`, which is a `Channel[R](2)`,
and races its receive against `Async.sleep(within)`. The reactive
bridge moves demand in half-window batches through a channel of
`capacity`. Each lane has a control so the module's price is a ratio
(`ActorReactiveBenchmark`, N=4000 elements or messages, 200 asks, 100
lifecycles; load 3–4, no sibling build):

| lane | us/op | B/op | against its control |
|---|---|---|---|
| `actorTell` — 4000 tells as one program, then one ask | 295.9 ±14.1 | 1 837 075 | **1.49x** `channelBuffer`, 1.31x the bytes |
| `channelBuffer` — the mailbox's own shape, no actor | 198.5 ±2.1 | 1 407 042 | control |
| `actorAsk` — 200 sequential round trips | 2594.4 ±47.3 | 1 108 493 | **13.0 us and 5.5 KB per ask** |
| `actorSpawnStop` — 100 lifecycles | 104.3 ±4.2 | 431 800 | 1.04 us and 4.3 KB each |
| `reactiveRound` — out through a `Flow.Publisher`, back through a `Flow.Subscriber` | 312.9 ±13.0 | 2 951 724 | **5.67x** `plainSource`, 3.43x the bytes |
| `plainSource` — the same `Source.range`, no bridge | 55.2 ±2.4 | 861 088 | control |

**What the actor's 1.49x is made of**, by JFR allocation samples on
`actorTell`: `okay.Slot` 142, `java.lang.Long` 141, `scala.util.Right`
95, `scala.Some` 89, the `Platform` block lambda 87, the `Async.Await`
anon 87, `Free$Pure` 81, `Free$Bind` 49. Read per message: one
`receiveBlocking()` is a `Slot`, an `Await`, its callback, and the
`Right(Some(m))` that `End = Either[Throwable, Option[A]]` wraps the
message in — five objects — and the `Behavior[Long, Msg]` boxes its
`Long` state on every step. That is the RECEIVE side's per-element
handshake, the mirror of the one `feed-offer-first` removed from the
send side the same day, and it is there because the loop reads one
message at a time. The 141 boxed Longs are the benchmark's own choice
of a primitive state and are the cheapest thing on this page to avoid:
an `AnyRef` state boxes nothing.

**`ask` at 13 microseconds** is a channel with a ring of two, a send,
a race, and a virtual-thread timer armed for every call whether or not
the answer comes in a microsecond. 5.5 KB per ask says most of that is
the timer's stack chunk. For an ask-heavy caller that is the number to
change; for request-reply at human timescales it is invisible.

**The bridge at 5.67x is the largest ratio on this page**, and 738
bytes per element the largest per-element cost measured today — for a
round trip that a plain `Source` does in 215. It is unprofiled here;
the shape (publisher fiber → channel of 256 → half-window demand →
subscriber → channel → source) has two channels and two demand
batches per window in it, and the profile is the next lane's first
move, not this one's guess.

Nothing on this page changed code. It names three next lanes with
their evidence: a receive-side offer-first for the actor loop, the
`Reply` timer, and the bridge's profile.

### 17a. The receive-side offer-first, measured in both regimes and declined

`actor-receive-offer-first` built the mirror of `feed-offer-first` for
the actor loop: a synchronous `receiveNow` on `Channel` (one `Poll.Got`
allocation against the handshake's five) and a loop that polls before
it parks. Laws held, 8 of 8. Then the A/B, old loop against new, in
the two regimes an actor lives in (`actorTellBacklog` is the new lane;
control `channelBuffer` 199–203 throughout):

| regime | old loop | poll-first loop | time | bytes |
|---|---|---|---|---|
| mailbox empty (`actorTell`: a trivial behaviour outruns a producer paying an Await per tell) | 343.7 ±3.2 / 1 842 119 | 410.3 ±7.2, 412.6 ±14.8 / 1 583 795 | **+19%** | −14% |
| mailbox full (`actorTellBacklog`: 4000 messages offered before the actor starts) | 114.5 ±0.7 / 811 257 | 95.0 ±1.8, 112.3 ±15.1 / 523 280 | −17% / ~0 | **−35%** |

A failed poll is one more read of the cache line the producer is
writing — about 17ns, and 4000 of them are the 67us the empty regime
lost. The full regime wins because the producer is idle and the line
is not contended. A responsive actor's mailbox is empty most of the
time, so the loop change is a regression in the common case and was
reverted; the benchmark keeps both regimes so the next attempt is
judged on both. The design that wins in both is filed as
`actor-receive-fused`: fold the try into `receiveAsync`'s own first
scan so a hit costs no second read, and merge the slot with its
callback into one object so a miss allocates one thing, not two.

### 17b. The bridge, halved — and only by both halves at once

`reactive-bridge-profile` counted the round trip per side before
touching it: the reader made **4001 awaits for 4000 elements** — one
handshake per element where `drained` takes 62 through `receiveMany`
— and the pump walked the source through `toLazyList`, a memoising
cell, a `State$Cons` and a thunk per element (135 of ~900 allocation
samples) for a re-observability a pump never uses. Two fixes, each a
pattern this document already records: the reader takes a chunk and
serves it, demand still following consumption; the pump takes the
linear view, as `feed` did. Each was measured ALONE first, then
together (`reactiveRound` against `plainSource`, control 55–58 us /
861 088 B/op stable throughout; laws 49/49 at every step):

| change | us/op | B/op | verdict alone |
|---|---|---|---|
| master | 312.9 ±13.0 | 2 951 724 | 5.67x the plain source |
| reader batched, alone | 358.3 ±10.3 | 3 030 674 ±142 890 | worse — counted, the batches ARE 52–60 per await, and buy nothing |
| pump linear view, alone | 364.7 ±46.2 | 2 727 369 | −7.6% bytes, time in the noise |
| **both** | **152.5 ±5.3** | **1 746 516** | **−51% time, −41% bytes — 2.63x the plain source** |

Alone, the batched reader drained a channel the pump could not keep
full, and every empty batch was a park on both sides — the ±143 KB on
its bytes is the park count varying. Alone, the linear view made the
pump faster but the reader still paid a handshake per element. Together
the pump runs AHEAD and the reader's batches fill. This is the third
time today a consumer-side batch measured worthless behind a slow
producer and reversed once the producer was fixed (`bufferChunked`
after `feed-linear-view`; the floor lane's premise; here). The rule it
teaches: measure a batch only after the side that fills it is fast,
and never withdraw one on a measurement taken before that.

What is left of the 2.63x: the round trip still carries `Writer$Say`
per element, `uncons`'s `Right`+`Tuple2`+`Some` on the pump, an atomic
`decrementAndGet` of demand per element, boxing for a primitive
element type, and the channel's ring push and pop. Those are the
bridge's representation, not a hole.

### 17c. The receive-side handshake, fused — allocation down in both regimes, time at parity

`actor-receive-offer-first` (§17a) lost 19% with an empty mailbox
because its try was a second scan of the cache line the producer
writes. `actor-receive-fused` puts the try INSIDE the scan the
handshake was going to do anyway, and makes the callback its own
slot: `Handoff[A]` carries the value, the fence and the waiter, and
IS the `End => Unit` the channel calls. `Channel.receiveInto(h)` is
`receiveAsync`'s first scan with an early return — a hit writes the
element into `h` with no `Right(Some(_))` and answers true; a miss is
byte-identical to the old path and answers false, and `CanBlock.await`
parks on `h`. `receiveBlocking` is those four lines. Measured old
against new in both regimes (control `channelBuffer` 222–223 us /
1 406 600 B/op stable):

| regime | old | fused | bytes |
|---|---|---|---|
| mailbox empty (`actorTell`) | 442.3 ±253.8 / 1 774 080 | 344.6 ±22.7 / 1 682 538 | **−5.2%** |
| mailbox full (`actorTellBacklog`) | 115.6 ±0.7 / 747 101 | 115.8 ±34.4 / 650 776 | **−12.9%** |

The old empty-regime time is void at that error bar (a burst of load
mid-run); the fused one sits on master's earlier 343.7. Time is at
parity in both regimes — the point — and bytes are down in both. A
receive that finds its element allocates a `Handoff` and a `Some`
where five objects went; one that waits allocates the `Handoff` where
two went. The −35% the poll-first loop showed in the full regime is
not reached, because the handoff is still one object per receive; the
poll-first's `Got` was the same one object, and its 19% loss elsewhere
is what this design exists to not pay. Laws: `TestHandoff` (a hit
answers true and never parks; a miss registers; the end and a failure
arrive through the handoff, synchronously or to a parked receiver;
the STM channel's default path stays correct; 3000 elements through a
ring of 2), all of `TestChannelLaws`, the poison laws; Native compiles.

### 17d. Small wins: the timer off its thread, and a chunks door for the element channel

Three small changes measured on one run (load 4–8 with a burst
mid-run, so the time bars are wide; the bytes are the claim):

| what | before | after |
|---|---|---|
| `actorAsk` — the JVM `Timer` armed per ask | 2594.4 us / 1 108 493 B/op (5.5 KB per ask) | 2150.1 ±770 / **879 176 B/op, −20.7%** (4.4 KB) |
| `elem_drainedChunks` — the `receiveMany` batches told as chunks | (elementwise read of the same channel: 255.6 / 1 413 443) | 91.1 ±55 / **79 469 B/op — 17.8x less**, 20 bytes per element |

`Timer.after` on the JVM started a virtual thread and slept it; every
`ask` armed one and, on nearly every ask, cancelled it when the reply
came a few microseconds later — 5.5 KB of stack chunk per ask for a
thread that never ran its callback. One scheduled executor now holds
every pending delay as a small task, and the callback gets its virtual
thread only when the delay FIRES. What a cancelled timer costs is the
task. Every `Timer` user gets it: `ask`, `Async.sleep`, `race`.

`drainedChunks` is the door §6c said was missing: `.drained.chunked()`
re-chunks what `Drain` already batched and read 318.7 against 209
elementwise. This tells each `receiveMany` batch as one `Chunk[A]`,
nothing re-done — 20 bytes per element against 350 for the same
channel read one at a time. It sits between `chunkNative` (19.66,
where `bufferChunked` pays the representation once per chunk on BOTH
sides) and the elementwise read, because the send side here is still
per element.

Two of this lane's items were declined by design and are recorded on
the board: `Source.unfold`'s pair per step is the caller's
`Option[(A, S)]`, and `Drain`'s copy per element is the re-observation
law's fresh cursor. A supervised `Stop` now drains and discards, so
`ActorRef.stopped` comes true (`actor-stop-strands`).

### 17e. The ask's wait as one operation — two fibers per ask gone, −65% bytes

`Reply.await` was `Async.race(box.receive, Async.sleep(within))`:
the right contest, staged as two fibers — `race` spawns each side —
for a wait whose two contestants are a channel callback and a timer
callback and need no fiber at all. It is now one `Async.await`: the
box's `receiveAsync` and `Timer.after`, whichever fires first wins by
an `AtomicBoolean` and cancels the other; the canceller cancels the
timer. `ask` no longer needs a `Scheduler`. Before and after, JMH
`-prof gc`, two forks, 200 sequential asks per op:

| | before | after | Δ |
|---|---|---|---|
| `actorAsk` | 2332.2 ±437 us / 880 429 B | 1557.0 ±201 us / **308 646 B** | −33% time, **−65% bytes** |
| per ask | 11.7 us / 4.4 KB | 7.8 us / **1.5 KB** | |

The 2.9 KB that left is the two fibers: two virtual threads' stack
chunks, two `Fiber`s, `race`'s atomics and its three closures, per
ask. What stays is the `Reply`'s channel of two, the send, the timer
task and the ask's own `Await`. Time error bars are wide on both runs
(the box at load 5–14) and the bytes are the number to read. The
actor suites hold (14, plus the Live ones on purpose), including the
ask-times-out law at `within = 200`.

The entry's other shape — a single-slot box for `Reply` in place of
the channel of two — is still open; it is worth 1.5 KB at most and
probably a few hundred bytes, and the channel answers close and
end-after-value for free.

### 17f. channel-elementwise-wakeups: the wakeup that is not there

The board's primary channel entry said the elementwise consumer
spends three quarters of its time parked and pays one unpark per
element once the producer saturates the ring. Its own harness,
re-taken (first time since the chunked feed), `-prof gc`, two forks:

| lane | us/op | B/op | per element |
|---|---|---|---|
| `okaySentinelElem` | 205.9 ±17.5 | 307 668 | 77 B |
| `okaySentinelChunk` | 55.0 ±6.3 | 318 318 | 80 B |
| `okayStrongElem` (StmChannel) | 250.1 ±47.4 | 1 521 385 | 380 B |
| `okayElementwise` / `okayChunked` (granularity harness) | 146.1 ±1.0 / 54.9 ±3.2 | 299 401 / 315 803 | 75 / 79 B |

Then the count the entry asked for, with a probe in `SentinelChannel`
(reverted, not landed) on the `okaySentinelElem` shape, 100 runs after
200 warmup, three processes:

| per element | senders woken | senders parked | receivers parked |
|---|---|---|---|
| | 0.000–0.001 | 0.003–0.005 | 0.000 |

Twelve to twenty producer parks per four thousand elements and a
consumer that never parks: there is no unpark per element to
amortise, so no wake policy can pay, and the entry closes measured.
Two more things the table settles. The elementwise and chunked lanes
allocate the SAME bytes per element (the `Long` box and the ring
slot), so the 3.7x between them is the path length of a
`receiveBlocking` — a `Handoff` per call, the scan, the shared
reads, ~37 ns over a chunk's share — not allocation. And the
`SentinelChannel` reads ahead of `StmChannel` on the elementwise axis
now (205.9 against 250.1), where the entry had it behind: the 208.9 →
268.7 regression it cites was real when written and is gone.

### 17g. receive-blocking-path-length: the head CAS, and a single-consumer ring — −25% elementwise

Where the ~37 ns per element of §17f go, by JFR `ExecutionSample`
on `okaySentinelElem` (one fork, ~420 samples, top frame):

| frame | samples | share |
|---|---|---|
| `AtomicLong.compareAndSet` — `Ring.pop`'s head CAS | 149 | 35% |
| `Ring.pop` (own code) | 60 | 14% |
| `AtomicReferenceArray.get` — the slot read | 59 | 14% |
| `ParkHandoff.<init>` — the handoff per call | 35 | 8% |
| `SentinelChannel.receiveInto` (own code) | 34 | 8% |
| the stamp read, the senders' queue head, `wakeOne` | 41 | 10% |

Half the consumer's time is the pop, and a third of everything is
one instruction: the compare-and-swap that moves the head. It exists
because the ring is multi-consumer (Vyukov's MPMC): two consumers
could claim the same position. With ONE consumer the head is that
consumer's private cursor, and producers never read it for a
decision — they wait on stamps — so a release store moves it. That is
a promise only the caller can make, so it is made at construction:
`Queues.strong[A].bounded(n, singleConsumer = true)`, `Ring`'s flag
behind it, `pop` and `popMany` choosing the store. A second concurrent
receiver on such a channel would take an element twice; the law that
needs contending consumers is recorded as not claimed for it, the
other ten hold. Same run, `-prof gc`, two forks:

| lane | multi-consumer ring | single-consumer ring | Δ |
|---|---|---|---|
| `okaySentinelElem` | 202.9 ±17.7 us / 302 473 B | **152.1 ±7.2 us** / 300 593 B | **−25%** |
| `okaySentinelChunk` | 56.0 ±5.3 / 311 717 B | 65.6 ±17.1 / 332 944 B | within error |
| `actorTellBacklog` (full mailbox) | 93.1 ±28.7 / 645 410 B | 93.1 ±11.5 / 645 415 B | identical |
| `actorTell` (empty mailbox), two runs | 382.0 ±75.5, then 595.9 ±181.8 | 594.9 ±444.9, then 423.8 ±28.9 | indistinguishable |

The elementwise channel takes the whole prize the profile promised
for the CAS, and its bytes do not move. The chunked lane already
paid one CAS per batch, so nothing changes there. The actor's mailbox
is the single-consumer shape by construction — the loop is its only
reader — and `Actor.spawn`'s default mailbox is built that way now;
in its two regimes the pop is not where the time is (the backlog
regime is identical to the byte, the empty regime is the park/unpark
between two virtual threads and flipped between two noisy runs), so
the default is chosen for what the ring is, not for a number it
moved. The next 22% of the elementwise consumer are the slot read and
the `Handoff` allocation; the slot read is the element arriving and
stays, and the handoff is 8% for one object per call.

## 18. Three platforms, one source — the first numbers off the JVM

Every lane in this document so far ran on the JVM. The library is
cross-built for JS and Native too, and the channel — a ring of CASes
— runs on real threads with no Loom on Native and in one thread through
the event loop on JS. `BenchCross` (src/test/scala-cross) times the
same four shapes on whichever platform compiles it, through
`Async.runAsync` so one source serves all three: a munit suite tagged
`Live`, outside the default gate, run on purpose with the build's
`--exclude-tags=Live` replaced by an include (the harness's header says
how; an include given after `--` runs nothing, which the first attempt
proved with three exit-0 runs of zero tests). N=4000, thirty warmup
runs, the median of twenty and the minimum, in microseconds:

| lane | jvm | js | native |
|---|---|---|---|
| `rangeFold` — `Source.range` through `runForeach` | 105.4 / 97.3 | 545.9 / 479.2 | 1193.4 / 1076.6 |
| `channelElem` — `Channel.buffer(1024).drained`, one element at a time | 1615.8 / 1315.1 | 1061.7 / 963.3 | 2338.9 / 2230.5 |
| `channelChunks` — the same channel through `drainedChunks` | 1092.0 / 945.9 | 348.8 / 315.7 | 1117.2 / 717.9 |
| `bindChain` — N nested flatMaps, no channel | 482.7 / 470.3 | 230.3 / 164.3 | 517.0 / 497.1 |

**Read the JVM column against JMH, not as JMH.** Thirty warmup runs
is not JMH's seconds of warmup, `runAsync` is not `runWith`, and this
run sat on a box at load 12–26. The JVM's channel lanes read 8–12x
their JMH figures (188 and 91 on a quiet box, §17d); `bindChain` read
483 where a first, quieter run read 189. The JVM's numbers live in
the JMH sections. JS and Native are what this section is for, and
they are stable: JS within 15% across two runs on every lane, Native
within 15% on three of four.

**What the ratios say, and they say it on both runs.** The chunks door
pays everywhere and pays most where a handshake costs most: on JS
`channelChunks` is 3.0x faster than the elementwise read, on Native
2.1x by median and 3.1x by minimum (4.6x on the first run), on the
JVM 1.5x here and 2.1x under JMH. Native's elementwise read is the
slowest number on the page because every one of its 64-per-await
handshakes is an OS-level wait with no Loom to make parking cheap —
so on Native, read channels in chunks. The pure interpreter
(`bindChain`) is where JS surprises: 164–230, on par with the JVM's
warm figure, V8 handling the closure-per-bind shape well; Native's
497–517 is 2–3x that, stable across runs, and it is the allocator.
This paragraph first said "Scala Native's GC paying for the same
`Free` nodes"; §18a measured it, and the collector is not where the
time goes — the allocation path is. That is the first
platform-specific cost this library has a number for.

A ruler, not a scale: comparable across platforms for one lane, and
against the JVM's JMH figure for the same shape; not a substitute for
either.

### 18a. native-interpreter-allocation: the collector, exonerated; the count, taken

The question was whether Native's 2–3x on `bindChain` is the
collector or the allocations themselves. Scala Native 0.5's immix
writes one row per collection (mark / nullify / sweep, ns) to the file
named by `GC_STATS_FILE`, the file is the whole process, so
`BenchCross` gained `BENCH_LANES=bindChain` to run one lane per
process; and `GC_INITIAL_HEAP_SIZE` fixes the heap. Two runs of the
one lane, N=4000, median of 20 over 30 warmups, microseconds:

| heap | median / min | collections in the process | collector time |
|---|---|---|---|
| default | 503.3 / 497.7 | 10 | 11.4 ms (mark 5.2, sweep 6.2) |
| `GC_INITIAL_HEAP_SIZE=2G` | 570.2 / 558.8 | 0 | 0 |

With the collector out of the picture entirely the lane is 13%
SLOWER, not faster: without a collection the bump pointer only ever
moves into pages the process has never written, and first-touch costs
more than reuse. The cost is the mutator's allocation path, and it
scales with what is allocated.

How much is allocated is the same on every platform, so the JVM
carries the count. `PerElementStepBenchmark.bind_runWith` is the same
program under JMH with `-prof gc`, two forks:

| lane | time | bytes/op | per bind |
|---|---|---|---|
| `bind_runWith` | 27.5 us | 540,984 B | 135 B |
| `bind_runAsync` (the terminal BenchCross uses) | 39.9 us | 608,184 B | 152 B |

135 bytes is about six objects: `Inject` (16), `Run` (16), the
`() => i` thunk with its `Long` (24), `Bind` (24), the
`x => go(i + 1, acc + x)` closure with two `Long`s (32), and — by the
seven bytes the account is short of otherwise — a boxed `Long` for
`x` through `flatMap`'s generic `Function1` (16). Native runs the same
graph at about 21 ns per object-and-step; the JVM's TLAB and escape
analysis run it at about 1. Fewer objects per bind is the only lever, and it
is a lever on every platform: `free-bind-node-count`.

Two readings the table forces. BenchCross's JVM column for this lane
(189 on a quiet run, 483 under load) is 5–12x the JMH figure: thirty
warmups of 4000 binds is not warm, exactly as the harness's header
says, and the column is read against JMH, never as JMH. And Native's
distance from the JVM is not 2–3x, that is its distance from JS; from
the JVM's real number it is 12–18x, all of it in allocation.

### 18b. free-bind-node-count: the floor of a bind without its effect

Before anything is fused, what fusing could buy at most: `pureChain`
is `bindChain` with `okay.pure(i)` in place of `async(i)` — `Pure` +
`Bind` + the closure per step, none of `Inject`, `Run`, the thunk, or
the handler round-trip `Bind(Inject(a), f)` makes. Same run, same
process, Native (BenchCross, N=4000, median of 20 / min, us) and the
JVM (JMH `-prof gc`, two forks):

| lane | native | jvm | jvm bytes/op | per step |
|---|---|---|---|---|
| `bindChain` / `bind_runWith` | 532.6 / 493.9 | 27.6 us | 540,984 B | 135 B |
| `pureChain` / `bind_pureChain` | 232.9 / 228.1 | 19.4 us | 380,984 B | 95 B |

The injection is 40 bytes and 30% of the bind on the JVM, and 56% of
it on Native — 300 us of the 533, or 75 ns per step against the pure
step's 58. The two terminals are not the same loop, and the table
says so: `runWith` on `Free` is already a direct loop (`runFree` in
Effects.scala — a tail-recursive match over the cases, the handler a
plain `H.handle(e)` call), so its 8 us of injection is the three
objects and one virtual call. `runAsync` — the terminal BenchCross
runs on every platform — is `Drive.apply`, which calls `fold` afresh
for every operation with a polymorphic handler value: `h(a)` builds
the `k => …` closure per step, the answer returns through `k`, and
the loop re-enters `fold` from the top. On the JVM that costs 12.6 us
of `bind_runAsync`'s 40.2 over `bind_runWith`'s 27.6; on Native,
which inlines none of it, the split between the objects and the
round-trip is not yet known — it is the next measurement. A fourth
`Free` case was priced and declined on the spot: 118 places outside
Free.scala match on `Pure` / `Bind` / `Inject` directly (Stm,
Condition, Chunks, Cont…), and every one would have to learn it. The
open lever is the loop, not the node: `Drive.apply` written as
`runFree` and `Stm`'s runner already are, a direct match with `Run`
and `Await` inlined — `async-direct-loop`, whose ceiling on the JVM is
the 12.6 us and whose Native number §18c takes.

### 18c. async-direct-loop: `Drive.apply` as a direct loop — a quarter off every platform's bind

`Drive.apply` (the `runAsync` terminal) now matches `Free`'s cases in
its own `while` — the rotation and the `Bind(Pure, f)` step as in
`fold`, the operation dispatched to a method that returns the next
program or null when the drive parked — instead of re-entering `fold`
with a polymorphic handler value per operation. The `Await` exchange
cell, cancellation at the next operation and the re-entry from a
callback are the same code moved. Before and after in one session,
BenchCross N=4000 median of 20 / min (us) and JMH `-prof gc`, two
forks:

| lane | before | after | Δ |
|---|---|---|---|
| `bindChain` native | 534.1 / 507.1 | 395.9 / 379.0 | −26% / −25% |
| `bindChain` js | 212.1 / 160.3 | 133.0 / 128.2 | −37% / −20% |
| `bind_runAsync` jvm | 40.2 us, 608,184 B | 29.2 us, 541,040 B | −27%, −11% |
| `pureChain` native (control) | 235.6 / 225.6 | 232.5 / 226.5 | — |
| `pureChain` js (control) | 61.7 / 60.5 | 70.9 / 69.8 | +15% / +15% |
| `bind_runWith` jvm (control) | 27.6 us, 540,984 B | 26.9 us, 540,984 B | — |

The bytes say what went: 67,144 B per 4000 binds is 16.8 per bind —
one 16-byte closure per operation, the `k => …` that `h(a)` built —
and `bind_runAsync` now sits 2.3 us over `bind_runWith`, which is the
`Promise` and the `Await` in the harness. On Native the round-trip
was 138 us of the 534 — a quarter of the bind, and 46% of the 300 us
the injection cost in §18b; the 163 us that remain are the three
objects, the `op` call and `f()`. JS took the most by median and the
least by minimum, and its control drifted 15% the other way in the
same run: read the JS minimum, −20%. Native's control did not move.

The §18 table above is the run it records; the Native and JS
`bindChain` cells read a quarter lower after this change, and the
JVM column still reads against JMH, not as JMH.

### 18d. stm-js-direct-bench: the two STM handlers on three platforms

`BenchStmCross` (src/test/scala-cross, the same harness rules as
§18): 4000 transactions in a chain on one fibre, no contention, under
`Stm.tl2` (versions, a CAS-owned commit) and `Stm.direct` (one thread,
no versions, no validation — the JS given), and a `control` lane that
is the same chain with `async(i)` in place of the transaction. Lane
minus control is the transaction. Median of 20 / min, us:

| lane | jvm | js | native |
|---|---|---|---|
| `control` — the bind chain alone | 100.4 / 92.7 | 206.5 / 150.5 | 385.0 / 368.7 |
| `tl2Modify` — one `Modify` | 110.3 / 106.1 | 405.4 / 349.5 | 1119.0 / 1106.6 |
| `directModify` | 174.8 / 115.9 | 357.8 / 294.2 | 1114.2 / 1107.5 |
| `tl2ReadWrite` — a `Read` then a `Write` | 709.8 / 692.2 | 1762.8 / 1689.9 | 7920.3 / 6934.1 |
| `directReadWrite` | 408.7 / 319.5 | 1360.3 / 1336.7 | 6409.4 / 4702.5 |

And the JVM reference, `StmBenchmark` under JMH (`runWith`, `-prof
gc`, two forks):

| lane | us/op | B/op | per transaction over the control |
|---|---|---|---|
| `control` | 27.2 ±0.2 | 540 984 | — |
| `tl2Modify` | 64.5 ±0.6 | 762 032 | 9.3 ns, 55 B |
| `directModify` | 64.2 ±0.3 | 762 032 | 9.3 ns, 55 B |
| `tl2ReadWrite` | 298.4 ±23.6 | 3 830 962 ±171 238 | 68 ns, 822 B |
| `directReadWrite` | 398.1 ±1.8 | 3 638 963 | 93 ns, 774 B |

Three readings. **A one-operation transaction is the same handler
twice**: both take the structural fast path (`fast(tx)`, a root
`Effect(Modify)`), and JMH has them identical to the byte — so the
harness's JVM `directModify` at 174.8 against `tl2Modify`'s 110.3 is
warm-up order, not a difference, and Native has them equal (1119 vs
1114). **On a real transaction the JS handler is ahead where it is
the given** — 23% on Node by median, 19% on Native — and on the JVM
the pair is inside its own noise, flipping between the two
terminals. **The cost is the transaction, not the handler**: a
Read-then-Write pays ~800 bytes and 70–90 ns on the JVM, ~300 ns on
Node and ~1.5 us on Native, in the `Log` (a reads buffer, a
persistent write map updated per write, a boxed version per read),
the installed cell, the transaction's own nodes, and the
`Async.await` staging of an attempt that commits without ever
parking. That last one is a shape, not a law — an attempt that
commits could answer `pure` and reserve the `Await` for a `retry` —
and it was tried next, §18e.

### 18e. stm-sync-commit-fastpath: tried, worse, reverted

The first attempt inside a `Run` (nothing at construction), a commit
answering `pure`, the `Await` only for a `retry`, in both handlers;
the seventeen STM tests green. Same lanes, `-prof gc`, two forks,
before (the §18d run, two hours earlier on the same box) → after:

| lane | before | after | Δ |
|---|---|---|---|
| `tl2ReadWrite` jvm | 298.4 ±23.6 us / 3 830 962 B | 317.6 ±2.6 / 4 326 962 B | +6%, +124 B per tx |
| `directReadWrite` jvm | 398.1 ±1.8 / 3 638 963 B | 450.9 ±5.5 / 4 246 963 B | **+13%, +152 B per tx** |
| `directReadWrite` js / native (median) | 1360.3 / 6409.4 | 1402.2 / 6000.5 | flat / inside the bars |
| `*Modify` (control) | 64.2–64.5 / 762 032 B | 64.2–64.4 / 762 032 B | unchanged |

The `Run`, its thunk, the `Bind`, the closure and the `Either` cost
more than what they replaced: the `Await`'s registration closure,
the drive's exchange cell and its `Got`, which the synchronous-answer
path settles in one compare-and-swap and a `getAndSet`. The
estimate that the two were a wash on bytes was wrong by 150 bytes,
and the atomics it expected to save did not show up as time.
Reverted in full; the lanes stay. The log — ~800 bytes per
Read-then-Write in a persistent write map, a tuple per read and the
installed cell — is the untouched number, and §18f takes it.

### 18f. stm-log-cost: the read set as arrays, the commit as one walk — −58% on `direct`, −23% on `tl2`

Profiled first. By JFR allocation sample the largest item of a
Read-then-Write transaction was the read set — an
`ArrayBuffer[(TRef[?], Long)]`: the buffer, its sixteen-slot backing
array, a tuple and a boxed version per read — and the second was the
commit's iteration: `written` built an `Iterator` from a REVERSED
copy of the write map's list and a `map`, twice per commit (install,
then wake), and `installTo` looked every value up again through an
`Option`. By CPU sample the iterators were a tenth. Two changes,
measured one at a time (`StmBenchmark`, `-prof gc`, two forks; the
`*Modify` lanes and `control` unchanged throughout):

| lane | §18d baseline | (a) read set as two arrays | (b) + the commit as one walk |
|---|---|---|---|
| `tl2ReadWrite` | 298.4 ±23.6 us / 3 830 962 B | 321.4 ±2.2 / 3 610 034 | **229.3 ±0.9 / 3 322 034** |
| `directReadWrite` | 398.1 ±1.8 us / 3 638 963 B | 400.4 ±8.1 / 3 386 035 | **167.5 ±3.6 / 2 298 033** |
| per transaction, over the bind | 68 ns / 822 B ; 93 ns / 774 B | | **51 ns / 695 B ; 35 ns / 439 B** |

(a) took 55–63 bytes per transaction and no time: small objects are
what a TLAB is for. (b) took the time — −29% on `tl2`, **−58% on
`direct`** — and 72–272 more bytes: the JFR's tenth was an
under-count, because `direct`'s commit was two iterator walks plus a
map lookup per install, and `tl2`'s one walk plus the same lookup
inside `Held.install`. Now `TMap.foreachUnordered` walks the list
forward with no copy and no iterator (`foreach` keeps its promised
insertion order for the callers that read it), `Log.eachWrite` is
the one typed walk a commit makes, `Held` carries the value it will
install, and `own` answers a typed null instead of an `Option`. What
a Read-then-Write costs now: 51 ns and 695 bytes under `tl2`, 35 ns
and 439 under `direct`, against 9 ns and 55 for the `Modify` fast
path. What remains is the program's own nodes, the `Long` boxing a
generic cell implies, the `Slot` a wrapped cell installs, and the
`Await` staging §18e showed is cheaper than its replacement. Laws:
every STM suite, `TestTMap`'s insertion-order law included. A
confirmation run on the landed code read 245.1 ±7.4 / 171.3 ±1.6 us
at the same bytes.

The other platforms, `BenchStmCross` after both changes (median,
us): Native `directReadWrite` 6409 → **4777** (−25%) and
`tl2ReadWrite` 7920 → 6718 (−15%); JS 1360 → 1462 and 1763 → 1850,
+5–7%, which is inside the run-to-run drift the same harness showed
on unchanged code (+3%) — no win on Node, and none lost that the
drift can distinguish. The iterators cost most where a JIT could not
remove them: the JVM's escape analysis evidently did not, V8's
evidently did.

## 19. okay-script — what a page costs (the first numbers)

The container has been built out for four days (markdown pages as a
JSP-level web framework, specs/okay-script.md) and had never once
been measured. Its own question is not the library-versus-library one
the sections above answer: a `.md` page is compiled by the REAL Scala
compiler at runtime, so the number that matters is what that costs
ONCE against what a request costs after it.

**Not JMH, and stated as a decision.** Every sample here is
milliseconds to seconds, dominated by dotc; each JMH fork would pay
every compile again to measure the same thing with more ceremony. The
harness (`MeasureScript`, `Live`-tagged, in okay-script's own suite)
takes medians of n samples with the warmup discarded and prints the
table below; its assertions are sanity bounds only — a page renders, a
warm render beats a cold one — never a millisecond threshold, which on
a loaded box is a red build that says nothing about the code.

Host: 14 cpus, load average 8–14 (a working machine, not a quiet
lab), JVM 21. Three runs; the spread between them is the last column's
business, and it was small enough that the medians below are one run's,
not a doctored average.

| what | median | note |
|---|---:|---|
| cold render, first page of the process | 870 ms | includes dotc's own warmup |
| cold render, prose only | 102 ms | compile + invoke, warm JVM |
| cold render, a code block | 152 ms | compile + invoke |
| cold render, front-matter + yaml + code | 160 ms | the `Meta` plumbing too |
| warm render (cached compile, re-invoked) | 0.062 ms | what a request actually costs |
| hot reload (mtime changed, recompiled) | 111 ms | the edit–refresh loop |
| static file, 200 (read + ETag) | 0.048 ms | size+mtime ETag, no digest |
| static file, 304 (validated) | 0.021 ms | not read at all |
| memory held per compiled page | 87 KiB | 20 pages, heap delta after GC |
| renders/second, 1 thread | 58 096 | 20k renders in 0.34 s |
| renders/second, 2 threads | 98 475 | 40k in 0.41 s |
| renders/second, 4 threads | 100 561 | 80k in 0.80 s |
| renders/second, 8 threads | 105 852 | 160k in 1.51 s |

Run to run, the cold numbers moved by about 10% (92–102 ms prose,
140–165 ms with code) and the warm one by 5% (0.061–0.065 ms); the
concurrency rows moved more (58–62k single-threaded, 99–116k at four
threads) because the host's own load did.

**What the table says.**

- **A page is compiled once and then it is free.** 152 ms to compile,
  0.062 ms to answer — a ratio of about 2 500. The compile-once-
  invoke-many split (`Page`, hot-reload by mtime) is not an
  optimization, it is the whole reason a runtime-compiled page can
  serve traffic at all.
- **The first page of the process costs six of the next ones** (870 ms
  against ~150), because dotc warms itself up in it. A server pays it
  once, on the first visitor — which is exactly the argument for
  compiling the whole directory at boot instead, filed as the next
  task rather than assumed here.
- **Metadata is free.** Front-matter, a `yaml` block and headings cost
  the same as a plain code block (160 vs 152 ms, inside the run-to-run
  spread): the `Meta` tree is literalized into the synthesized source,
  so it is compile-time work in a compile that was happening anyway.
- **A 304 is half a 200 on a static file** (0.021 vs 0.048 ms) — the
  ETag is size and mtime, so a validated request never reads the file.
- **87 KiB per compiled page** is the classloader, the compiled
  classes and the temp directory's handle. A hundred pages is ~9 MiB;
  a thousand-page site is a heap decision, not a surprise.
- **Concurrency saturates around 100k renders/s at four threads** on
  this host — 1.7x from one thread to two, then flat. That is the
  shape a per-page lock gives when the work under it is 60 us: the
  lock is not the ceiling (it is only held across the mtime check),
  the ceiling is the shared `Application`/`Sessions` maps and the host
  itself under load 8. For scale, 100k renders/s is roughly 8.6
  billion a day; a page's own work — a database call, an LLM turn —
  will decide long before this does.

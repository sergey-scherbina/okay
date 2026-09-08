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

## merge-scaling-shape (2026-09-03): linear, so the kernel rewrite is off the table

The two lanes above closed with one question left standing. The
remaining cost is `!.resume`'s Bind rotation, and the textbook fix
for THAT is reflection without remorse (Van der Ploeg & Kiselyov
2014): replace `Bind(m, k)`'s single continuation with a
type-aligned queue of them, so appending is O(1) and the rotation
disappears as a concept. It is a real technique with a real cost —
type-alignment in Scala 3 wants either a cast (against this
project's own rule) or a heavy GADT, and it changes the shape 42
sites depend on through `resume`'s three-form invariant, plus
`widen`, `relay`, `<|>`, `runFree`, `foldCont`, `foldIn`, on three
platforms.

But that technique removes QUADRATIC behaviour on left-nested
binds. It does nothing for a constant per-element cost. So the
question that decides whether any of that risk is worth taking is
not "how expensive is rotation" — it is "does the cost per element
GROW with the element count". `ScalingBenchmark` asks exactly that,
by sweeping `n` and reading the numbers per element rather than as
totals, with the bare `LazyList` walk as the control for the
platform's own scaling.

| per element | 500 el | 1000 el | 2000 el | 4000 el | over 8x |
|---|---|---|---|---|---|
| `rawLazyListDrain` (control) | 11.3ns | 11.4 | 11.0 | 10.6 | x0.94 |
| `sourceSingleDrain` | 41.2ns | 39.6 | 41.5 | 40.6 | x0.98 |
| `channelMerge` | 142.3ns | 121.9 | 127.9 | 131.8 | x0.93 |
| `sourceMerge` | 303.5ns | 299.7 | 300.7 | 291.6 | x0.96 |

**Every lane is flat across an 8x range** — the per-element cost
does not grow, it drifts DOWN slightly (warm-up amortizing over a
longer run). The Bind tree the Writer path builds is linear. There
is no quadratic to remove, so reflection without remorse would buy
nothing here, and the kernel rewrite has no measured justification.
That avenue is closed with data rather than left open as a
someday-maybe — which was the whole point of asking before
rewriting. (The shape is what `ofLoop` builds: right-nested
`flatMap`, where rotation is cheap. Left-nesting is what makes the
technique pay, and this codebase already learned that lesson once
from the other side — the 1000x kyo numbers were a left-nested
`foldLeft` artifact, at parity right-nested.)

**What the sweep exposes instead.** Decomposing the flat numbers:
the Writer layer costs ~30ns per element ALONE (41 against the
control's 11), and ~160ns per element INSIDE the merge (292 against
`channelMerge`'s 132) — the same layer, roughly **5x more
expensive** in the merged shape. That is not a tree-shape fact, and
it is the quantified form of what channel-cas-contention found
qualitatively (a slower step widens the window a competing fiber's
CAS can land in; measured there as 28.1% -> 34.3% fail rate at
matched capacity). The lever it points at is not a cheaper
interpretation step but FEWER of them inside the contended region —
which is what the library already offers as `Chunks.merge`, one
queue operation per chunk rather than per element, and which the
same benchmark family measures at 10.7us for 2x500 against
`sourceMerge`'s 299.7us. The per-element `Source` path pays ~300ns
per element for per-element program semantics; the batched path is
the answer where throughput is what matters, and it already exists.

**Nothing landed but the benchmark.** `ScalingBenchmark` is kept
(unlike channel-queue-reversal's exploratory file, which was
removed because its premise did not hold): this one's premise held
and it answered its question, and it is the scaling control any
future "should we rewrite the kernel" question should have to pass
first.

## free-row-variance (2026-09-03): the upcast that is not free, measured

merge-scaling-shape closed the kernel-rewrite question and named one
cheap lever still standing: `Source.merge` calls `Writer.widen` once
per source, and widen REBUILDS every Free node of the walk — for a
reason `Effects.scala` stated as a fact, "Free is invariant in its
signature". If that invariance were removable, both calls would
collapse into subtyping and a whole per-element pass would vanish.

**The invariance is removable.** A spike settled the type-level half:
`enum Free[+F[+_], A]` passes the variance check — F occurs only in
covariant positions (`Inject`'s field, `Bind`'s `Free`-typed field,
and its continuation's RESULT) — and an isolated test confirmed the
consequence that matters, that `Box[Row[Int], A] <: Box[Row[Int |
String], A]` holds pointwise at concrete rows, exactly the shape
`Source.merge` needs.

It is not free to adopt. Matching `Bind(Inject(e), k)` under a
covariant row captures a fresh subtype per level, so the interpreter's
own rewrites stop typechecking in place. Two were recovered cleanly
and without a cast, by naming the existentials in a helper: the
associativity rotation in `Free.fold` and in `!.resume` (the hottest
path in the library) both went through a `rotate(a, f, g)` whose three
arguments reach their parameters by ordinary subtype coercion. About
six more walker sites — `Effects.widen`, `Generate.uncons`,
`Logic.msplit`, `Writer.map`/`widen` — resisted inference, each
needing its own hand-typed helper. A generic `up[H[+_] >: F]` upcast
does NOT work: an abstract type-constructor bound gives no coercion,
though a concrete row does.

**And then the prize was measured, before paying that price — which
is just as well, because the prize is negative.** In isolation widen
costs what it looks like: 20.430 -> 24.126us at 500 elements (+18%,
7.4ns/element), 77.465 -> 98.343us at 2000 (+27%, 10.4ns/element).
Inside the merge it does the opposite. The same merge built without
the two widen passes, at one element type so the union collapses and
widen is the ONLY difference:

| 2x2000 elements | with widen | without widen |
|---|---|---|
| run 1 | 1162.4 ±11.4 | 1240.1 ±10.1 (+6.7%) |
| run 2 (quiet box) | 1141.8 ±6.7 | 1202.6 ±14.6 (+5.3%) |

Bars non-overlapping in both runs, same direction. **Removing widen
makes the merge slower.** The walk is not only a re-injection, it is
a NORMALIZATION: it resumes each node and hands `Channel.merge`'s
`feed` an already head-normal, right-nested tree. Without it `feed`'s
per-pull `resume` pays that rotation itself — per pull, inside the
contended region, where this same lane already measured the Writer
layer costing ~5x what it costs alone. Widen moves the work OUT of
the contended region and does it once; that is worth more than the
pass costs.

**Declined, nothing landed but the benchmark.** The covariance spike
was reverted; `Free` stays invariant in its row — now as a choice with
a number behind it rather than an unexamined constraint, and
`Effects.widen`'s doc comment says so. `WidenBenchmark` is kept: its
premise held, and it is the guard on this conclusion. The general
lesson is the one this whole arc keeps arriving at from new
directions: an upcast that is free at the type level is not free
operationally, and where work is DONE matters more than how much of
it there is.

## rowlift (2026-09-08): moving an operation into a wider row, for free

free-row-variance answered "can the walk be deleted?" with a measured
no. This section answers the question that survived it: the walk is
right for `Source.merge`, but is it right for a SINGLE operation?

**The problem, as it appears in code.** A constructor builds at its
own row — `State.get[Int] : Int ! (State % Int)` — and a program
usually has a wider one. The only spelling the library had was
`!.widen[A, F, G]`, which asks for the COMPLEMENT: the part of the row
you are NOT talking about. That is what made the tracked demo
(`okay-jdbc/.../UsersDemo.scala`) awkward and what makes a helper
against an unknown row hard to write — you cannot name a complement
you have not been told.

**Six spellings, measured.** `RowLiftBenchmark`, N=1000 operations, so
one 16-byte node per operation reads as 16 000 B/op:

| lane | B/op | what it does |
|---|---|---|
| `viaEffect` | 272 016 | `effect[R, A](op)` — construct AT the row, the floor |
| `viaDirect` | 272 016 | `direct { op.!? }` — the macro emits Inject at the block's row |
| `viaAt` | 272 016 | cast under an `In` witness |
| `viaLiftAt` | 288 000 | lift the OPERATION, then inject: +1 node |
| `viaWiden` | 288 016 | today's spelling: +1 node |
| `viaAtWalk` | 304 000 | the same postfix, done by walking: +2 |

The escape-analysis hypothesis was tested here and refuted: the
intermediate Inject that `widen` and the walking `.at` build does not
get scalarised away — it is constructed and destructured inside one
inlined region, the textbook case, and it still costs bytes.

**Decision: `RowLift.at`, one cast, under a witness.** `+` is a union
(`[A] =>> F[A] | G[A]`) and unions erase, so a `Free[F, A]` already IS
a `Free[R, A]` whenever F is a member of R. `In[F, R]` is the proof of
that side condition — `self` / `left` / `deeper`, the minimal
hierarchy — and the single `asInstanceOf` lives in `at`, beside the
invariant that licenses it.

- **The witness has no runtime existence.** The first cut gave `In` an
  `inj` method, so each instance had to be an object; one cached
  `IdIn` served them all and each `given` cast it into place — three
  casts spent to avoid an allocation, and `inj` was the erasure fact
  stated a second time. As `opaque type In[F[+_], R[+_]] = Unit` the
  instances are `()`: nothing allocated, no cast in any instance, and
  opacity is what stops a caller conjuring a proof. One
  `asInstanceOf` in the whole design.
- **The trap, since the error message points away from it.** An opaque
  type is TRANSPARENT inside its defining scope: write the uses in the
  same object and `In[F, R]` is literally `Unit` there, implicit
  search goes to `Unit`'s companion, and the compiler says "No given
  instance of type In[...]" while suggesting the imports that are
  already in scope. Move the uses out and the givens resolve with no
  import at all — the companion of an opaque type IS its implicit
  scope. `ProbeOneCast` is laid out that way for this reason.
- **Ergonomics.** Partially applied, `type Has[F[+_]] = [R[+_]] =>>
  In[F, R]` makes the witness a context bound: `def bump[R[+_] :
  Has[State % Int] : Has[Writer % String]]`. The target row is named
  once; the complement never is.

**`plus` names only the addition.** `at[R]` asks for the whole target
row, which is right at a call site and noise inside a helper: the row
you are already in is in the type, so repeating it is repeating
yourself. `p.plus[R] : A ! (F + R)` says only what is being added —

    Users.find(id).plus[Abort]   :  Option[String] ! (Users + Abort)

— and needs no witness at all, where `at` needs one: membership is by
CONSTRUCTION here, `F + R` being built out of F, so there is nothing
for a proof to establish. Both go through the same single cast, and
`viaPlus` measures at the floor with the rest (272 016 B/op).

Which to reach for is only about what is shorter to SAY: `plus` names
the addition, `at` names the target. One effect added: `plus`. Several
operations landing in one row: `at`, since there each operation's
complement differs while the target does not. `at` is REQUIRED only
where the complement cannot be named at all — an abstract row known
only by membership (`Fail.scala`'s `abort[A].at[F]`). An earlier
version of this section said an interpreter's `Tracked + F` was such a
row; it is not, and both spellings compile there.

**The prohibition that comes with it.** `.at` does NOT replace
`!.widen`, and a change that swaps one for the other on a streaming
path is a regression, not a cleanup: free-row-variance measured the
walk as a NORMALIZATION worth 5-7% on `Source.merge`. `.at` is for
single operations, which are already head-normal and have nothing to
normalize. The two coexist on purpose.

**Refuted along the way**, kept so the next attempt starts later than
this one did:

- **A macro** emitting the Inject at R directly. Scala binds an
  extension's receiver to a val proxy BEFORE the splice, so the macro
  sees `Ident("p$proxy1")`; marking the receiver `inline` fixes that
  (`HIT: Free$.Inject$.apply`), but the operation it extracts refers
  to a proxy from `effect`'s own inlining and re-emitting it fails
  with "a reference to value a$proxy16 was used outside the scope
  where it was defined". Rewriting a subtree that arrived from an
  inline def is not generally possible — which is why `direct` builds
  its Inject from its own pieces. Worse, a macro that cannot see its
  term fails SILENTLY into its fallback: the lane read 288016,
  identical to widen to three decimals, and looked like a result.
- **`<:<` instead of `In`** — and it proves more than expected: the
  compiler establishes `F[X] <:< R[X]` by itself, at concrete types,
  any row shape, any depth, with no instance hierarchy written. But
  the polymorphic form `[X] => () => (F[X] <:< R[X])` is not
  summonable (implicit search diverges), and the operations inside a
  program carry different X, so a per-X proof cannot serve.
- **Currying the constructors** (`State.get[S]` returning an applier
  that takes the row). `direct[F]` gets away with this because nobody
  reads `direct[F]` as a value; a constructor is different — the day
  `State.get[Int]` stops being a program, every call site that reads
  it as one breaks. That is the migration the row parameter was
  trying to avoid, just moved.

## signature-covariance (2026-09-08): what `F[+_]` is actually for

`Free`'s row is `F[+_]`, so every signature in this library is
covariant in its answer type. The question came from a call site, not
from theory: interpreting `Save extends Users[Unit]` inside
`[X] => (e: Users[X]) => ...`, the GADT match proves only `X >: Unit`,
never `X = Unit` — because a COVARIANT `Users[Unit]` is a `Users[X]`
for every `X >: Unit`. So the branch must widen `Unit ! R` to `X ! R`,
and every interpreter carries a `.map(_ => ())` that looks like noise
and is not.

**Measured, by spiking it.** Rewriting every `[+_]` bound in
`src/main` to `[_]` — which does NOT make the library's own
signatures invariant, only permits invariant ones — the whole core
compiles with exactly TWO real failures:

1. `TypeableK`'s generic instance, `given [F[+_]](using
   Typeable[F[Nothing]])`. It answers `Option[x.type & F[Nothing]]`
   where `Option[x.type & F[A]]` is wanted, and only covariance closed
   that gap. Under an invariant bound it needs a cast.
2. `<|>`, where `case T(e)` infers the unapply's type argument as
   `Nothing` and covariance made that fine. `T.unapply[A](e) match` —
   passing the argument instead of inferring it — fixes it with no
   cast.

That is the whole cost in the kernel. Everything else was mechanical:
`[+_]` to `[_]` across 23 files in main and ~15 in test, none of it
interesting, and the library's own effects stay covariant and keep
working.

**And the payoff is real.** With the bound relaxed, an invariant
signature — `enum Users[A]` — runs end to end (`InvSpike`: a program,
an interpreter into `State`, the right answer), and the compiler says
of the Save branch: "X is a type in method stored **which is an alias
of Unit**". Exact refinement. The widening disappears, provided the
answer the branch produces is a `Unit` (with `State.modify` answering
the new STATE, as it does, a `.map(_ => ())` is still needed — that
one is about the combinator, not about variance).

**Two conditions, jointly.** Spiked again to settle it, because the
question keeps being asked as if variance alone were the answer.
Removing covariance does NOT delete the `.map(_ => ())` from an
interpreter's Save branch: with `State.modify` answering the new STATE
the branch still produces an `S ! R` that has to be thrown away, and
the compiler says so ("Found: Map[Long,String], Required: X ... which
is an alias of Unit" — the variance is gone from the message, the
mismatch is not). Nor does a `Unit`-answering `modify` delete it on
its own: under covariance `X` stays `>: Unit` and something must
widen. With BOTH — an invariant signature and a combinator that
answers `Unit` — the branch is bare:

    case Users.Save(id, name) =>
      State.modify[Map[Long, String]](_ + (id -> name)).plus[F]

compiles and runs (`InvSpike2`). So the widening is the sum of two
independent choices, and each can be paid for separately.

**And there is a third route that costs nothing at all: give the
operation an answer worth computing.** With `Save` declared
`Users[Option[String]]` — the name it replaced — the branch ends in a
`map` that does real work, and the widening rides along inside it for
free:

    case Users.Save(id, name) =>
      State.get[S].plus[F].flatMap: store =>
        S.get(id)(store) match
          case None => pure(None)
          case was  => State.modify[S](S.put(id, name)).plus[F].map(_ => was)

That version of the demo contained ZERO occurrences of `map(_ => ())`
(counted, not remembered), with the library untouched: no variance
change, no new combinator. The widening only looks like noise when the
operation answers `Unit`, because then there is nothing for the `map`
to be doing. The demo answers `Unit` on purpose — see
`okay-jdbc/.../UsersDemo.scala`, which says why the worse model is the
better demonstration — but a real signature usually has something to
say, and then the question does not arise.

**THE COMPLETE ANSWER (the whole library made invariant).** Every
signature's answer type flipped — `State[S, A]`, `Writer[+W, A]`,
`Reader[R, A]`, `Throws[E, A]`, `Choose[A]`, `Take[V, A]`, `Async[A]`,
`Resource[A]`, `Delim[A]`, `Flush[A]`, `Tx[A]` — on top of the relaxed
bounds. Main compiles, tests compile, 83 tests pass across ten suites
(Stm, Logic, Throws, Fail, SeqEffect, State, Effects, Delim,
Condition, DeriveEffect). Two things needed changing beyond the
mechanical churn, and they are the entire answer to "what is
covariance in an effect FOR":

1. **`TypeableK`'s generic instance.** `Typeable[F[Nothing]]` answers
   at `F[Nothing]` where `F[A]` is wanted, and only covariance closed
   that. Without it the instance needs a cast — and it is now the one
   thing a signature can avoid needing, since `derives Effect` builds
   the same test from a `ClassTag`.

2. **An operation that never answers, declared once at `Nothing`.**
   `case Retry() extends Tx[Nothing]` is a `Tx[A]` for every A only
   because `Tx` is covariant. Invariant, it is written
   `case Retry[A]() extends Tx[A]` — one type parameter, and arguably
   a worse statement, since it now claims to answer an A it never
   produces. Two operations in the library are of this shape (`Tx`'s
   `Retry`, `Condition`'s `Leave`).

That is all. Not the union rows, not `Pure`, not the handlers, not the
constructions at `Nothing` — those were measured separately and are
inference. `<|>` needed an explicit type argument, which is not a
cost, it is a better line.

**Can the `Typeable` fallback simply GO?** Measured too, since it is
the one thing standing between this library and invariant signatures.
With it deleted, `sbt compile` — every module's main sources — is
green. Across the whole build's tests, five errors in two files:

- `okay-sql/TestSqlPure.scala` needs `import okay.given_TypeableK_Async`.
  The instance exists; the test was quietly getting the fallback
  instead. Mechanical, and arguably a better line.
- `TestDeriveEffect`'s `summon[TypeableK[Db + Writer % String]]` — a
  COMPOSITE row. That capability really does go, and no replacement
  can be FOUND: a structural `given both[F, G](using TypeableK[F],
  TypeableK[G]): TypeableK[F + G]` compiles but never matches a
  concrete row, because solving F and G from `Db + Writer % String` is
  the same higher-order unification the compiler declines everywhere
  else in this file. A composite test would have to be passed
  explicitly.

Nothing in the library needs a composite one: `Handler.union[F, G]`
tests only its LEFT side, so nesting to the right keeps every tested
signature atomic — `union[Tool, Context + (Model + Async)]` asks for
`TypeableK[Tool]`.

**So the trade, exactly.** Covariance buys one instance and one
declaration shorthand. It costs exact GADT refinement: matching an
operation declared `Users[Unit]` proves only `X >: Unit`, so every
interpreter branch that answers `Unit` carries a widening. Which of
those matters more is a judgement about what this library is FOR, and
it is the operator's.

**Contravariance was tried too, since the question comes up: `enum
Users[-A]`.** It is not a variant of the choice, it is the wrong
direction, and the compiler says so in one line — the GADT then proves
`X <: Option[String]` and `X <: Unit`, UPPER bounds, and an
interpreter branch has to PRODUCE the answer:

    Found: Option[String]   Required: X
    where: X ... with bounds <: Option[String]

`Option[String]` is not an unknown subtype of itself; the branch
cannot be written. That is the semantics showing through: the answer
index is an OUTPUT of the handler, so it is produced narrow and
consumed wide, which is covariance. Contravariance would let an
operation that answers `Any` stand where one answering `Unit` is
expected, and nothing could satisfy it.

**THE DECISION: keep covariance.** Three reasons, in the order they
matter:

1. It is the RIGHT variance for what the index means. The handler
   produces the answer; the program consumes it. Produce narrow,
   consume wide.
2. It is the only way to say "this operation never answers":
   `case Retry() extends Tx[Nothing]` is a `Tx[A]` for every A.
   Invariant, that is `Retry[A]()`, which CLAIMS to answer an A it
   never produces — a worse type, not a smaller one.
3. What it costs is a widening in interpreter branches that answer
   `Unit`, and the cheapest fix for that is not variance at all: give
   the operation something to answer. Measured — the version of the
   demo whose `Save` answered the name it replaced contained zero
   `map(_ => ())`, with the library untouched.

The measurements above stand as the record of what the alternative
costs, which is what makes this a decision rather than an assumption.
The `Typeable` fallback stays with it: it is the instance covariance
is for, and removing it buys nothing once covariance is kept.

**Not taken, then.** The trade is: one cast in `TypeableK`'s
generic instance (in a repo whose rule is no cast without necessity),
plus churn in every module, against `.map(_ => ())` in interpreters
and exact GADT types for anyone who wants them. Recorded here rather
than done, because the choice is the operator's and because the
measurement — two kernel edits, not a rewrite — is the part that was
worth finding out.

Two smaller facts fell out and are worth keeping:

- It was assumed that covariance is what lets `Choose(Seq.empty) :
  Choose[Nothing]` stand for `Choose[A]`, and `Throws(e)` for
  `Throws[E, A]`. It is NOT. Spiked with `case class Choose[A]` and
  `case class Throws[E, A]` — invariant — on top of the relaxed
  bounds: main compiles, the tests compile, and 28 of them pass
  (TestLogic, TestThrows, TestFail, TestSeqEffect). The type argument
  comes from the EXPECTED type at each construction site, which
  inference propagates into the constructor call; variance was
  contributing nothing there. So `Nothing` never had to be
  substituted, and the "conveniences at construction" cost of dropping
  covariance is zero.
- `derives Effect` (ClassTag-based) needs no covariance at all, so the
  one place that does need it is now the one place a signature can
  avoid declaring.

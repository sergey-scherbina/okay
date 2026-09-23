# Continuations — what they are here, what they can become

## Overview

The operator's question, 2026-09-16, after `Cont` became a facade over
`Free` (specs/freer-base.md, theory ch. 11): *what do we have with
continuations now, what can we do with them, can the Free
representation be compiled into something faster, do continuations
take part, and can their role grow?* This spec is the answer and the
plan. It fixes three things: an inventory of the continuation
machinery as it stands, the one architectural fact that bounds what
"compiling a program" can mean here, and four roads ranked by expected
gain, each with the lane that would settle it and the number it owes.

The standing rule applies to every road: a prediction here is a
hypothesis, the lane measures it, and a refutation with numbers is a
fine outcome (`performance` skill; history.tsv keeps the refuted ones).

## What exists (the inventory, 2026-09-16)

One mechanism, three layers, everything above them a use of the same
continuation.

- **`Cont[A, S, R]`** (Cont.scala): `(A => S) => R` with answer-type
  modification, defunctionalized as the freer tree at the signature
  "a function of the continuation". A shift is a leaf; `S`, `R` are
  phantom on the facade; one rotation (`Free.resume`), one trampoline
  (`Delay`), absorption exactly once (`Once`). Runner `Cont.step`.
- **`Delim`** (Delim.scala): multi-prompt control — `prompt`, `push`,
  `shift`/`shift0`/`control`/`control0`, `abort`, `reset` — with one
  machine owning the prompt stack so a capture may cross intervening
  delimiters, and a prompt carrying its own answer type so several
  answer types share one row. Foreign operations suspend the machine.
- **Handlers as continuations** (Handler.scala): `F !> S = F ==> ([X]
  =>> X /> S)`. Three shapes on one line — comonadic (`runWith`, each
  continuation used exactly once and at once: `runFree`), translating
  (`translate`, answers with a program), Cont-valued (`handle`: abort
  as `Throws`, multi-shot as `Choice`, forwarding). `relay` is the
  tail-resumptive fast road; `handle` forwards on the tree and enters
  `Cont` only for a claimed operation; `Cont.onAnswer` keeps an
  answering handler off the trampoline.

Everything user-facing is one of those in different clothes:

| feature | the continuation underneath |
|---|---|
| generators (`Generate`: `Loop`/`take`/`put`; `LazyList`, `Producer`, `Teller`) | a `shift` capturing "the rest of the enumeration" at each element |
| fair search (`Logic`, `Choice`, `msplit`) | the continuation invoked once per alternative — multi-shot |
| conditions (`Condition`: `signal`, restarts, `resume`) | an exception that is a prompt; the handler decides whether the continuation runs |
| `Async.await` on Loom, JS and Native | the continuation as data a callback resumes later; the same tree on three platforms |
| `Monadic.reflect`/`reify`, the `direct` block | Filinski's construction over this `Cont`; the macro lowers a block into binds on it |
| `PState`, `Loop`'s open recursion | answer-type modification as typestate |
| deep recursion in a `direct` block (2026-09-15) | a self-call deferred into a `Delay` node |
| `Resource`, `bracket` | a scope whose finalizers ride the continuation |

What this gives a user, in one list: multi-shot is free because a
program is an immutable tree (Loom cannot offer it at all); effects
compose as a row with no transformer stack; the same program runs on
three platforms; search, generators, conditions and asynchrony are one
primitive, and the core's size says so.

## The fact that bounds "compile the program"

**A freer tree is a coroutine, not a syntax tree.** Its continuations
are JVM functions, opaque. A built program shows its head — `Pure |
Inject | Bind(Inject, k)` after `resume` — and nothing else until it
is fed an answer. Whole-program optimization of a *built* `A ! F` is
therefore impossible in principle, and this is not a shortcoming of
the implementation but the price of a continuation being arbitrary
code. Compilation lives where it is possible, and every such road
already exists:

1. **At construction.** `Fused`/`Control[Func]`: an inline
   handler-passing program partially evaluates at compile time into
   flat closures (specs/staged-effects.md: `effInline24` 1.9x over
   interpretation). `Eager`: pure binds apply at construction.
2. **On the interpreter's side.** Handler fusion: `Fused.stateWriter`
   walks the tree once for two effects instead of twice
   (specs/handler-fusion.md, the 13.7 µs floor); `relay` and
   `onAnswer` are the same idea — learn that a continuation is needed
   once, and do not reify it.
3. **First-order DSLs where whole-program optimization matters.**
   `Pipeline.optimize`, `Tables.optimize` (query plans), `Schema`
   (codecs, including `staging.run` at run time). This is a deliberate
   split: what must be optimized as a whole is represented as data
   with no closures in it.

And the measured ceiling on the fourth road, "run without a tree":
`Eff` and the CPS lanes ran at 0.58–0.86x of the fused tree loop
(handler-fusion stage B, memory `free-tree-is-not-the-cost`). The tree
is not the cost; the handler's work and the allocation *shape* are,
and the 2026-09-15 arc removed the shape costs it found (`Defer(t,
pure)`, forwarding through `Cont`, the JIT's inlining line).

## The four roads, ranked

### 1. Fused multi-effect handling — one walk for N handlers

**Today.** A row of `N` effects handled by `N` `handle`/`relay`
layers is `N` walks: every layer re-emits an operation it does not
own on the residual row (`Inject(e).flatMap(x => loop(k(x)))`), so a
forwarded operation costs one node per layer it passes and the
program is rebuilt `N − 1` times. `handle-forward-fast` made each
layer as cheap as `relay` (1 753 945 B/op to the digit on the 10k
lane) but not fewer layers.

**The road.** One walker over the whole row with one `split` per
operation against a vector of handlers — `Fused.stateWriter` written
for an arbitrary row rather than by hand for `State + Writer`. A
comonadic handler answers in place (as `runFree` does today); a
Cont-valued one gets the same `k` the tree already holds, so abort
and multi-shot keep their meaning — the continuation is the same
object either way, which is what the one-tree design makes possible.
`O(ops)` nodes instead of `O(N · ops)`.

**Interface sketch.** `Handlers[R]`: a typed vector of `F !> S` for
the members of the row `R`, built by the compiler from the parts as
`Handler.union` builds a comonadic one; `!.runAll(prog)(using
Handlers[R])`. Rows are unions, so the split is a chain of
`TypeableK` tests in declared order — the same tests the layers make
today, once instead of `N` times each.

**What is already known, and it caps the prediction.**
specs/handler-fusion.md measured nested handlers and found them paying
1.1–1.3x, not the 2–3x the nesting suggests: each inner pass consumes
its own operations and emits a residual, so pass `k` walks fewer nodes
than pass `k − 1` (~1 667 visits for two handlers over 1 000 ops, not
2 000), and "where the time actually is" is INSIDE one pass — the node
visit, the split, the continuation call. Fusing passes leaves every
one of those in place. So the prize of road 1 is the re-emission
allocation and that 1.1–1.3x, not more; anyone expecting a third is
reading the shape, not the measurement.

**Lane and number.** `HandlerBenchmark` gains a four-effect lane
(`Ask + State + Writer + Produce`, 10 000 ops) run three ways: the
four nested `handle`s, four nested `relay`s, and `runAll`. Prediction,
stated so it can be refuted: allocation down by the re-emission nodes
(up to three per forwarded op on a four-row), time down within the
1.1–1.3x handler-fusion measured; the fused `State + Writer` floor
(13.7 µs) is the control that must not move. Risk: the walker's
bytecode size (the inlining line; `resume` is at 323 bytes and every
`split` arm expands inline) — measure with `-XX:+PrintInlining` before
deciding the shape. If the lane reads under 1.1x, road 1 is a
refutation and the spec says so.

### 2. Handled `direct` blocks — known handlers, no tree

**Today.** `direct` lowers a block into binds on the tree; `Fused`
runs an inline handler-passing program with no tree at all; the two
do not meet. A user gets the second only by writing an inline program
by hand (`def prog[C[_,_,_]](h: Interpr[Row, C, R]): C[A, R, R]`).

**The road.** When the handlers are statically known at the block's
call site — `Fused.runCtrl[Func, …](s)(direct { … })` — the macro
emits the block as a `Func` program over `Control` instead of a tree:
each mark becomes `h(op)` applied through the carrier's `flatMap`,
which at `Func` is closure composition the compiler partially
evaluates. Effects at the price of closures for user code, not only
for hand-written inline programs. Multi-shot and abort still work,
because `Func` is `(A => S) => R` and a handler that calls `k` twice
does so at `Func` too; what is lost is stack safety on a left-nested
chain, which is `Func`'s known contract and the reason `Cont` is the
default.

**Lane and number.** The `direct` version of `rightSW` beside the
hand-written `rightCtrl[Func]` in `FusionBenchmark`; parity to the
byte with the hand-written program is the goal, and the tree version
of the same block is the baseline (13.7 µs / 122 641 B). Scope: the
macro's `pipeline` gains a second emission target; the lowering is
unchanged.

**Measured before building — staged-block-lanes, 2026-09-22.** The
lane above prices the wrong shape: `rightSW` is one operation per
recursive step, and stage B already showed that shape loses to the
tree (0.86x). What a `direct` block inside a loop actually is: a
STATIC block of k operations, then one recursive bind. So
`FusionBenchmark` gains the same 1 000 operations grouped 10 per
iteration, six ways — the tree prebuilt (`blockFreeR`), the tree built
and run (`blockFreeBuildR`, the user's price), the shipping nested
runners on the tree (`nestedBlockR`), the block over `Func` and `Cont`
with the handler passed as a value (`blockFuncR`/`blockContR`: binds
static, dispatch at run time — what the macro emits with an opaque
`h`), and the CEILING `blockFuncStagedR`, every operation written as
its shift directly (binds AND handler static — what the macro could
emit only when it sees the handler at the call site). Prediction,
stated so it can be refuted: staged-effects' 1.9x was measured
against `runIn[Cont]` at ~18 ns/op, and the fused tree walk is
13.7 ns/op, so the same shape against the real baseline predicts
about 1.25x for the ceiling and under 1.1x for the opaque-handler
form. Thresholds, before the run: ceiling under 1.2x over
`blockFreeR` — road 2 is refuted and the box below closes; 1.5x or
more — the emission target is worth building; between — the number
is filed and nothing is built.

**Measured (7e228aa1, two rounds × two forks, per-lane minima, load
2.9–3.5, `-prof gc`; history.tsv rows `sbl-*`):**

| lane | µs | B/op | vs `blockFreeR` |
|---|---|---|---|
| `blockFreeR` — tree prebuilt, fused fixture | 11.67 | 123 712 | 1.00 |
| `blockFreeBuildR` — tree built and run | 11.70 | 123 792 | 1.00 |
| `nestedBlockR` — the shipping runners | 14.96 | 154 448 | 0.78 |
| `blockFuncR` — Func, handler as a value | 13.13 | 125 392 | **0.89** |
| `blockContR` — Cont, handler as a value | 17.26 | 172 648 | 0.68 |
| `blockFuncStagedR` — the CEILING | **7.53** | **82 968** | **1.55** (1.99 over shipping) |

The prediction (~1.25x) is refuted upward: the ceiling clears the
1.5x build threshold against the fixture and is 2x over the runners
a user actually has. And the lane beside it says WHERE the win is:
with the handler passed as a value, static binds buy nothing — 0.89x,
the same bytes as the tree. The whole 1.55x is the handler being
known at the call site, so that each operation compiles to its shift
arm and no `split` runs. So the emission target that is worth
building is not "`direct` over Control with an opaque `h`" — that is
the refuted form — but "`direct` over Control with the handler
INLINED per operation", which needs the handler to be a compile-time
value the macro can read (an `inline` `Interpr`, or a per-effect
inline arm the macro selects by the operation's class). That is a
design of its own, filed as `direct-staged` with these numbers; the
prebuilt-vs-built row is a free finding on the side — a Free program
is re-materialised by every `k(x)`, so prebuilding it saves nothing
on a loop-shaped program.

### 3. Typestate on the facade — freer-base stage 2

**Today.** `PState` moves a state type through `S`/`R`; `Delim`'s
`NoPrompt` was proven expressible as a compile error (the stage-2
probe, 4af08745: a prompt's identity does reach the type level); the
lane is not started.

**The road.** One more opaque facade over `Free[F, A]` — `Prog[F, A,
S, R]` — whose smart constructors seal the transitions, with `A ! F`
as the diagonal. Because it is a facade and facades are never
matched, the existential leak that refuted the indexed enum cannot
reach it. Session-typed protocols over an effect row; a resource
whose "open"/"closed" is an answer type; `Delim` refusing an absent
prompt at compile time. Expressiveness, not speed — the tree is the
same tree.

**Lane and number.** `Delim` first (the probe's own example), then
one protocol from a module that has a real one (`okay-pg`'s
connection states, or the cluster's worker lifecycle). The number
owed is zero bytes and zero time on every core lane, since nothing
about the nodes changes; the deliverable is the compile error.

### 4. Continuations as data — the horizon, measured before started

**Today.** `dataflow` ships a *plan* as a value across machines, not
a suspended program, and it is right to: a `Bind`'s `k` is a JVM
closure and cannot leave the process.

**The road.** Defunctionalized continuations — every `k` a case of a
serializable ADT with an interpreter — would let a suspended program
migrate, checkpoint, or be inspected past its head. It is a different
contract for every combinator in the library and a different cost
model (an ADT node per bind where a closure is), so it is a spike, not
a lane: one effect, one program, measured against the closure version
on the same lane, and a written verdict before anything else moves.
Recorded so that nobody mistakes "possible" for "planned".

## Behavior

- [ ] Road 1 lane: the four-effect lane exists and is measured three
      ways before any fused walker is written (the "lane first" rule
      of delay-node).
- [ ] Road 1: `!.runAll` over a `Handlers[R]` vector, TestHandleForward's
      abort/multi-shot/order assertions passing through it unchanged.
- [x] Road 2 lane: the `direct` block beside the hand-written `Func`
      program, parity to the byte — BUILT as `Direct.staged`
      (specs/direct-staged.md, 5a3af433): 7.50 µs / 85 368 B against
      the hand-written 7.69 / 84 568, 2.24x over the same block as a
      Free block on the shipping runners.
- [x] Road 3: `NoPrompt` is a compile error on a `Prog` facade; one
      module protocol typed — BUILT 2026-09-23 (freer-base-stage2):
      `okay.Prog`, `Delim.Stacked` (the three throwing shapes refused
      by the compiler, the escaped prompt included), okay-sql's `Tx`
      (nested begin / orphan commit / a program left open do not
      compile). specs/freer-base.md, "Stage 2 — BUILT".
- [ ] Road 4: a spike with a verdict, not an implementation.

## Decisions

- **Order: 1, 2, 3, 4 — with road 1 capped by handler-fusion's
  number.** Road 1 continues handle-forward-fast with its lanes
  already built and returns a number in a day, but the number it can
  return is 1.1–1.3x, so if road 2 is wanted for user code it is the
  one with the larger ceiling (1.9x measured for the hand-written
  inline program); road 3 is expressiveness on a base that is now
  stable; road 4 is a horizon.
- **No road removes the tree.** The measurements say the tree is not
  the cost; the roads that pay are the ones that walk it fewer times
  (1), skip building it where the shape is static (2), or say more
  about it at compile time (3).
- **Every road keeps the one-tree invariant**: `Cont` stays
  `Free[Shift, A]`; a fused walker or a staged block emits the same
  nodes or none, never a second kind.

## Results

**Road 2 built** (specs/direct-staged.md, 2026-09-22): `Direct.staged`
2.24x on the State+Writer block, parity with the hand-written `Func`
program; then specs/direct-stagers.md (2026-09-23): `Stager.All` over
Reader+State+Writer+Throws, the Reader+Throws block 2.56x.

**The staging survey, 2026-09-22** (the operator: "what else can be
fused or staged beyond handlers and direct blocks?"), the inventory
after handler-fusion, direct-staged and generators — recorded here
because a survey that lives in a chat is not a record:

| already staged | mechanism | number |
|---|---|---|
| tagless programs | `staged[M]` inline over `Control` (staged-tagless) | 1.9x at `Func` |
| effects | `runIn[Func]` REFUTED; the inline program shape wins (staged-effects) | 263 vs 429 ns |
| handlers | `Handler.flat`, the arm chosen at compile time (handler-fusion) | 1.08–1.24x; arc closed |
| direct blocks | `Direct.staged` + loops v2 + stagers (direct-staged, direct-stagers) | 2.24x / 2.56x |
| codecs | three CBOR decoders, JSON (codecs, schema-fold) | closed |
| optics | `Fuse` (optics) | byte-for-byte with the hand-written update |
| stream pipelines | `Pipeline.optimize` + chunked compile (staged-pipelines) | at/under Iterator — no production caller |

What remained, ranked by (a production caller) × (a measurable
interpretive layer) × (a plausible lever), and what became of each:

1. **Stagers for the rows people write** — BUILT (direct-stagers).
2. **The derived test as a constant `instanceof`** — BUILT
   (typeablek-instanceof, 2026-09-23): 0.91–0.96 on every walker with
   a test under `split`, 1.01 on the flat lane that filed it.
3. **`Gen` chain fusion** — backlog `gen-chain-fusion`, triggered by
   `generators-jmh`'s number.
4. **A router trie** — backlog `router-trie`, triggered by a router
   with dozens of routes (in-repo routers hold 1–3).

Where staging is pointless, and why: `Proc`/workflow (the durable
log's I/O is the cost, not dispatch); `Sql` (a string and parameters,
the driver does the work); road 1 here (`!.runAll` over a vector of
handlers, capped 1.1–1.3x by handler-fusion's own data).

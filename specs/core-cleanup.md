# Core cleanup — Free, Cont, Effects after the freer-base arc

## Overview

A review of the three files the effect system is made of
(`Free.scala`, `Cont.scala`, `Effects.scala`, 2026-09-15), asked as
"what can be simplified, removed, improved and sped up". The answer
splits in two by risk, and this spec is the SAFE half: everything
here is a removal of code nothing calls, a duplicate folded into its
original, or a one-hop rewrite whose result is the same node the
old two hops produced. The speed half — a `Delay` node, which
touches the most shape-sensitive loop in the library — is a lane of
its own with a benchmark of its own (BACKLOG `delay-node`), because
nothing in this spec may move a Fib lane and that one might.

How "nothing calls it" was established: `grep` over every module's
`src` (not the main checkout's `find`, which zsh word-splitting
silently turned into zero hits the first time — memory
`zsh-no-word-split`), then the compiler: the removals were made and
`Test/compile` over every module run, so a caller the grep missed
would have refused the tree.

## Interface

Removed:
- `TypeableK.unapply` — the extractor form. `test` is the whole
  interface. No `case T(x)` over a `TypeableK` exists anywhere;
  every runner refines through `split` and matches the constructor.
- `typeableKByClass` — an alias of `typeableK` with a paragraph on
  it; the paragraph moved to `typeableK`, the two comments that
  named it (Reader, Pipe) now name `typeableK`.
- `fromFree` — `reflect` with a second name. `reflect` keeps the
  tree fold (no Cont on the way), the two tests move to `reflect`.
- `Effects.foldIn` / `runIn` — the carrier-generic fold on the
  encoding. specs/staged-effects.md measured it no faster than Cont
  and kept it "as semantics, with no performance claim"; the only
  callers were that spec's tests and two benchmark lanes. `Interpr`
  and `interpr` STAY: `Fused` is built on them and that is the
  staging that did pay.
- `Free.run(using Monad[F])` and `Free.run(f: F ==> M)` — no caller
  in any module.

Changed:
- `Effect.derived` builds `Effect.ByClass[F](cls)` directly (one
  class, one `isInstance`); it was `Effect.of(TypeableK.derived)`,
  a wrapper delegating to an anonymous class — two virtual calls
  under every `split`. `Effect.of` stays for the two tests that are
  not by class (`Instances.of`, `Tag.of`), which now implement
  `test` instead of `unapply` — and lose a cast each, as do
  `typeableK` and `writerK`.
- `reify` is `convert[M, Free]`; `Eff.foldIn` is gone with `foldIn`.
  One copy of the "rebuild each operation in the target" handler
  where there were four.
- `!.translate` splits with `split`, not `<|>`: no `Either` per
  operation on the road `interpret`, `tracing`, Instances, Tag and
  Tables take. The recursion there is through closures, not a
  `@tailrec` loop, so the inlined arms do not touch an inlining
  budget the way they would in `relay`.
- `Free.resume` and `Cont.step`: `Bind(Defer(t, f), g)` forces the
  thunk in ONE hop, `Bind(t(), f(_).flatMap(g))`, where it built a
  `Defer` the next iteration matched and forced. Same node, one
  allocation and one dispatch fewer per left-nested defer.
- The orphaned doc block over `Free.resume` (the first half of
  `fold`'s comment, left behind when `fold` moved below) is gone.

## Behavior

- [x] `Test/compile` over every module, and the gate, green with no
      warnings — the proof that nothing removed had a caller.
- [x] `TestDeriveEffect`, `TestRowIdentity`, `TestInstances`,
      `TestTag` unchanged and green: the class test answers the same.
- [x] `TestEffects` "initial and final" and `TestFused` on `reflect`
      where they read `fromFree`; `TestReflect` unchanged.
- [x] `TestHandleForward` and `TestCont`'s mutual-recursion tests
      (the `Bind(Defer)` shape) unchanged and green.
- [x] Control lanes NOT slower: `relayPrebuilt`, `handlePrebuilt`,
      `statePara`, `fib100` — this spec claims no speed, so the only
      number it owes is "nothing moved".

## Decisions

- **`!.Effect` (the `Inject` alias) keeps its name** although it
  collides with `okay.Effect` (the `derives` marker) — a file that
  imports `!.*` must write `derives okay.Effect`, ten of them do.
  specs/freer-base.md settled the name against the 154 match sites
  that use it, and this lane does not reopen a settled decision.
- **`Interpr`/`interpr`/`Control[Func]` stay.** They looked
  test-only from the trait's side, and `Fused.runCtrl` is their real
  user in main code.
- **The `Delay` node is not here.** Its mechanism is real — a
  `Defer` whose continuation is `pure` rotates into a left-nested
  `Bind` and the `.flatMap(pure)` tail travels down the whole
  deferred subprogram (`hff-defer-cost` priced that shape at 59 of
  61 µs in `handle`, and `tailcall`, `Eff.flatMap`, the codecs'
  `Cont.defer(...)(Cont.Pure)` and `handle`'s capturing arm all still
  build it). But a fifth case in `resume`/`step` is a change to the
  loop whose bytecode shape has cost 1.44x and 10% in two directions
  already; it needs its own lane, its own benchmark, and `-f 3`.

## Results

Landed 2026-09-15. Gate: 4426 test results, 0 failures; `Test/compile`
over every module and `okayJVM/Jmh/compile` in the fresh worktree with
zero warnings. The compiler found exactly what the grep had missed:
seven test lines and one benchmark lane spelling
`unapply(x).isDefined`, which is `test` — and no caller at all for
`fromFree`, `foldIn`/`runIn`, `typeableKByClass` or either `Free.run`.

Controls, master vs branch, one window, `-f 3 -wi 3 -i 5 -prof gc`,
box at load 7-8 both times (rows `cc-*`):

| lane | master | branch | B/op master | B/op branch |
|---|---|---|---|---|
| fib100 | 1987.6 ± 59 ns | 1981.3 ± 58 ns | 19 936.014 | 19 936.014 |
| fib1000 | 28 388 ± 271 ns | 28 584 ± 548 ns | 290 954.2 | 290 954.4 |
| relayPrebuilt | 153.6 ± 5.3 µs | 151.9 ± 1.3 µs | 1 753 945.06 | 1 753 945.05 |
| handlePrebuilt | 168.7 ± 16.6 µs | 170.9 ± 26.8 µs | 1 753 945.17 | 1 753 945.18 |
| statePara | 28.17 ± 0.2 µs | 29.12 ± 2.3 µs | 343 386 | 343 367 |

Every time delta is inside its own bar; allocation is identical to the
byte on the four lanes that go through `resume`/`step`/`split`, which
is the claim this spec made: nothing moved. (`handlePrebuilt` reads
169-171 here against 154 in `hff-*` on a quiet box — both sides, same
window, load 7-8; the ratio to `relayPrebuilt` in-run is 1.10/1.12,
not the 1.03 of a quiet box, and that is the load, not the lane:
`jmh-load-not-just-forks`.)

### delay-node — LANDED 2026-09-15 (the speed half)

`Free.Delay(thunk)` beside `Defer`, `Free.delay`/`Cont.delay`;
`!.tailcall`, `Effects[Free].tailcall`, `handle`'s capturing arm and
`Cbor`'s skip build it. `resume`: `Delay(t) => t().resume`,
`Bind(Delay(t), g) => Bind(t(), g).resume`; `Cont.step` the same two.
`!.?` forces it like a `Defer`. `Effects.tailcall` became a plain
`def` so the Free instance can override it (Eff and Eager keep the
`defer(thunk)(pure)` default).

Two lanes were written FIRST and measured before the node existed,
`-f 3 -wi 3 -i 5 -prof gc` (rows `dn-*`):

| lane | before | after | B/op before | B/op after |
|---|---|---|---|---|
| tailcallChain (10 000 hops) | 127.7 ± 1.1 µs | **24.1 ± 0.2 µs** | 1 359 969 | **400 016** |
| handleCapture (100 captures over the 10k tree) | 214.1 ± 1.9 µs | **159.1 ± 2.7 µs** | 2 486 713 | **1 764 361** |

5.3x and 1.35x. `handleCapture` now reads what `handlePrebuilt` reads
(159.0 in the same run): a capture costs 104 B over an answer and
nothing in time. `tailcallChain`'s 400 016 B is exactly one `Delay`
(40 B) per hop — the 96 B per hop of the old shape were the closure,
the `Bind` and the `Pure` the rotation built.

Controls in the same run: fib10 184.2, fib100 1960.9, fib1000 28 422,
relayPrebuilt 156.8, handlePrebuilt 159.0, statePara 27.9; B/op
IDENTICAL TO THE BYTE against master on every one (fib100 19 936.013,
relay/handle 1 753 945). `relayPrebuilt` read 2% over the morning's
master number, so it got a same-window A/B, three rounds alternating:
the box was carrying a VM at 575% CPU and rounds 2-3 had bars of
±28-59 (master relay read 217 in round 3), so per-lane MINIMA are the
reading — master 153.6, branch 152.9; fib10 master 183.9, branch
184.2 in the clean run. Equal. The two extra type tests on `resume`'s
fall-through path cost nothing the bars can see.

Behavior of the node, by tests already in place: TestEffects'
`isEven(1000000)` chain (stack safety of `tailcall`), TestHandleForward's
three stack-safety tests (the capturing arm), TestCont's mutual
recursion, the Cbor suite for the skip. Gate: see CHANGELOG.

### defer-eff-removal — LANDED 2026-09-15 (operator: "да ок")

Two removals the review raised as questions and the operator answered.

**`Defer` is gone; `Free.defer(t)(f)` is `Bind(Delay(t), f)`.** With
`Delay` in the tree the pair node was derivable, and `resume` and
`Cont.step` each lose two cases. The prior number (eff-stack-safety,
2026-09-09: `Bind(Suspend)` vs one `Defer`, +16 B/bind and ~3% on
`Eff`'s right-nested lane) was on the encoding that is the other
removal, so the hot payer went with it. What still builds the pair is
the codecs' trampolines past `NativeThreshold` (24), and no lane
reached that road until this one wrote `parseDeep` (a document 2 000
deep, compare/CodecBenchmark) and measured it FIRST:

| lane | before | after |
|---|---|---|
| parseDeep | 62.0 ± 1.0 µs, 827 973 B | 65.1 ± 0.2 µs, 906 948 B |

+5% and +40 B per deferred level, on a road only pathological input
takes. That is the price, and it is recorded rather than netted away.

**`Eff` is gone** — the Church encoding, its `Monad`, `Effects[Eff]`,
`toEff`, `Fused.runEff`, the `effSWr`/`effSW` lanes, the seven tests
that said "Free and Eff agree" (they say "Free and Eager agree" now,
which is the same claim at a second instance) and four doc passages.
Nothing outside those ever built one; the two facts it existed to
prove — the interface is honestly tagless, and a Church program can
be stack-safe — are in specs/handler-fusion.md and
specs/eff-stack-safety.md with their numbers.

**What the removal did to the JIT, and the fix that came with it.**
`Free.resume` went from 495 bytes to 323, under HotSpot's
`FreqInlineSize` of 325, and became "inline (hot)" in every loop that
calls it — the memory `inlining-threshold-two-faces` in one line.
Same-window A/B on a quiet box (load 1.6-2.9), two rounds each:

| lane | master | Defer removed | + handle's arms extracted |
|---|---|---|---|
| relayPrebuilt | 151.5 / 150.7 | **142.5 / 142.3** | **142.0 / 144.4** |
| handlePrebuilt | 153.8 / 154.5 | 175.6 / 179.1 | **145.5 / 146.0** |
| handleCapture | 152.5 / 152.4 | 171.6 / 173.0 | **146.8 / 146.7** |

`relay`'s loop is 244 bytes and gained 6% from the paste. `handle`'s
was 388 — already "hot method too big" — and a 323-byte loop pasted
into it cost 15%. `-XX:+PrintInlining` said exactly that (rows
`de-inl-*`). The move `handle-loop-inlining` tried on 2026-09-15
morning — the terminal case and the capturing fallback into their own
methods, `relay.last`'s shape — measured NOTHING then, because
`resume` at 495 bytes was never inlined into anything; the same move
now takes `handle` to 145.5, under where master was. Three faces of
the rule were known; this is the fourth: **a change to the callee's
size re-decides every caller, and a caller's shape that was neutral
can become the fix.** Allocation identical to the byte throughout
(relay/handle 1 753 945, handleCapture 1 764 377, fib100 19 936.013).

Behavior: gate green (see CHANGELOG); TestCont's mutual recursion and
the codec trampoline suites (TestJsonTrampoline, TestCborTrampoline,
TestCborSkipTrampoline, TestJsonStrictTrampoline, TestJsonRawTrampoline)
cover `Bind(Delay, f)` where `Defer` was.

### split-over-either — LANDED 2026-09-15, mostly a refutation

Fifteen walkers, 39 `<|>` sites, converted to `split`; `Resource`
keeps `<|>` because its arms `return` out of a `while`, which a lambda
cannot. `Delim` had to write `okay.split` — its object has a `split`
of its own, over the segment stack. Eight `Say` matches in Bind arms
needed the `(w0: @unchecked) match` idiom `Writer.run` already uses
(the checker cannot see `Say` is the only constructor under an
existential answer type).

Measured as a batch, before and after, `-f 3 -prof gc`, B/op first
because the box carried a sibling's load (7-30) through both runs:

| lane | B/op before | B/op after |
|---|---|---|
| fib10 / fib100 / fib1000 | 1 984.001 / 19 936.013 / 290 953 | identical |
| delimGenerator / delimPushOnly / writerTell | 934 311 / 350 040 / 142 024 | identical |
| offerReceive1k / sendReceiveProgram1k | 26 420.9 / 514 541 | identical |
| viaWiden / interpretedTree | 287 984 / 235 616 | identical / 235 608 |
| okayChoice / okayWriter / okayProducer / okayChunks | 14 318 107 / 2 078 073 / 1 318 433 / 129 832 | identical |
| okaySourceMerge | 1 072 482 | **1 027 412** (−4.2%) |
| elementwise 16/500, 16/2000, 64/500, 64/2000 | 1 057 026 / 4 549 336 / 1 052 235 / 4 430 252 | **1 037 608 / 4 303 523 / 993 851 / 4 150 077** (−1.8 … −6.3%) |

**The `Either` per operation was a theory, and on every walker but the
Source/Pipe road it was already gone:** escape analysis scalar-replaces
a `Left`/`Right` that is matched in the same method, and these loops
match it in the same method. Where it survived is where the walker's
arms cross a closure boundary the JIT does not see through — the
merge's pull under Async, `elementwise`'s stage pairing — and there
the bytes moved. Time, same-window A/B on a box at load 4-7, two
rounds: okaySourceMerge 101.7/103.8 → 100.8/101.7 µs, elementwise
and okayProducer inside their bars (one round-2 outlier on each side
at a load spike). Core lanes' time at the same load: fib10 172.3 →
172.3, delimPushOnly 24.3 → 23.3, offerReceive1k 17.3 → 16.7, the
rest inside bars.

Kept anyway, all fifteen: one splitting idiom across the library
(`split` is the documented kernel; `<|>` now has one user), no lane
worse, four lanes lighter. What this closes: BACKLOG's
`split-over-either` as a per-walker programme — there is no second
walker worth a lane of its own.

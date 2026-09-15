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

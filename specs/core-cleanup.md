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

- [ ] `Test/compile` over every module, and the gate, green with no
      warnings — the proof that nothing removed had a caller.
- [ ] `TestDeriveEffect`, `TestRowIdentity`, `TestInstances`,
      `TestTag` unchanged and green: the class test answers the same.
- [ ] `TestEffects` "initial and final" and `TestFused` on `reflect`
      where they read `fromFree`; `TestReflect` unchanged.
- [ ] `TestHandleForward` and `TestCont`'s mutual-recursion tests
      (the `Bind(Defer)` shape) unchanged and green.
- [ ] Control lanes NOT slower: `relayPrebuilt`, `handlePrebuilt`,
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

(filled at landing)

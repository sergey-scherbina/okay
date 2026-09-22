## direct-staged - Direct.staged: a direct block with the handler known at the call site

Road 2 of specs/continuations-roadmap.md, built. `Staged[Row, R, A]`
(core, Staged.scala) is `Func` with the row and the answer in the type;
a `Stage` object's `stage` is an INLINE MATCH over the row's
constructors, and `Stage.StateWriter[S, W, A]` is the canonical one.
`Direct.staged(sw) { … }` (okay-direct) compiles the same block text as
`direct { … }` — combinators, raw operations, `if`, `for … do`,
recursion — with every marked operation emitted as `sw.stage(op)` on
the operation as written, so the compiler picks the arm: no `split`,
no tree. Measured (`StagedBenchmark`, minima): 7.50 µs / 85 368 B
against the hand-written 7.69 / 84 568, and **2.24x** over the identical
block as a Free block on the shipping runners (16.8 / 164 928).

The macro seam is one lift: `DirectRow.rowOf` gains `stagedRow` (the
row read off the opaque `Staged`), `liftOp` emits `st.stage[X](op)`
instead of `Free.Inject(op)`, and a marked LEAF program (`State.get`,
which inlines to `Free.Inject(Get())` under proxies) is taken apart
before `compile` flattens it. For a staged block the hoisted monad val
keeps the given's precise type and the binds are built by `Select` on
it, so the instance's `override inline` flatMap reduces — the
difference between 152 568 and 85 368 B on the same block. Every other
block keeps its road byte for byte: the precise type broke `ctxMonad`
(Erasure "bad adapt") and a Free block that rebuilds a lambda
(LambdaLift), filed as `direct-inline-bind-free` with those two as its
first laws.

One thing every block gains: `val _ = m.!?` used to bind the mark,
match its value into `pure(())` and bind that again — five objects per
statement; it now binds straight into the rest. The benchmark's Free
block went 203 328 → 164 928 B for it.

Laws (`TestStaged`, 5): agreement with the Free block on 300 generated
data sets by state, log and answer; a concrete run; a 2 500-iteration
loop on the default stack; the compound-program refusal; the
foreign-monad refusal. okay-direct's suite 326 green. Limits stated in
the spec and docs/direct-style.md ("Layer 2½"): v1 refuses compound
marked programs, a Stage object is per row and layout, and a staged
block is not stack-safe on a left-nested chain.

## indexed-effects-measure-2 — the arc priced, three fixes, one JIT lead

One measurement pass after stages 7-9 (`scripts/history.sh
indexed-effects-measure-2`, specs/indexed-effects.md "The measurement
pass"): the one machine is 0.985-1.010x the old on four Delim lanes;
`stateThreaded` with the shared Get is 0.915x its old self and now
0.973x the untyped `stateEffect` (244 832 B); `Tx.Data` is 1.33x the
deleted facade at 4 ns per statement (the protocol tree). Landed here:
`Delim.Stacked.machine`'s typed `Op` arms in their own method (`step`
1282 -> 672 bytes), `State.handleIndexed` forwarding the node it holds
(`forwardedI`, -16 B per forwarded op), `TypeableI.derived` (a constant
`instanceof`; `Tx`, TestFreerPara and docs/typestate.md on it), the
forwarding lanes `stateForward`/`stateIndexedForward`. Found and
filed (backlog `freer-rotation-closure-jit-modes`): `stateLexDeep` and
`stateIndexedForward` are multi-modal per JVM fork, and PrintInlining
names the fast mode as the fork where `Freer.resume`'s rotation closure
is not inlined into the loop.
Commits: see `git log --grep indexed-effects-measure-2`.

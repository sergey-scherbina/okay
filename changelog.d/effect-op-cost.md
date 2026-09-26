## effect-op-cost - a fieldless operation is one shared node

- `State.get` and `Reader.ask` answer ONE shared `Inject(Get())` /
  `Inject(Ask())` (`SharedOps`) instead of a fresh pair per call: 32 B an
  operation fewer. One erasure cast each, in the accessor, with the
  reason beside it.
- The `Direct.staged` macro reads a shared node as its operation
  (DirectRow's shared-node table), so staged blocks stage them as before.
- Measured, min of three alternating rounds: 10 000 Reader asks 73.3 ->
  60.6 us (1.21x), 880 -> 560 KB; a State+Writer block 14.2 -> 13.1 us;
  `stagedDirect` and `stagedHand` unchanged to the byte.
- Found on the way, in bytecode: a node inside `object State` made that
  module impure to the inliner and a hand-staged `State.Get()` stayed
  live; a macro quote using the companion's `apply` left a live call per
  staged block. Both fixed; specs/effect-op-cost.md has them.

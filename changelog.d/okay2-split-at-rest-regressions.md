## okay2-split-at-rest-regressions - stage 45's produceFold cost is not its split; the okay-persist remainder filed

The follow-up to okay2-delim-perf's stage 45 pricing. Operator: "Делай то
что нужно для okay2 только сразу всё" (do what okay2 needs, all of it at
once).

**What was measured.** Stage 45 against its parent read produceFold
1.09x slower. The A/B that isolates its split is now done: on master,
`Producer.fold` with the pre-stage-45 shape restored (`Split.split`, two
closures, a `Left` and a tuple a step) is 1.27-1.33x SLOWER than its
`Split.at` loop, at +96 B an operation, in three alternating rounds
(history.d okay2-split-at-rest-producefold).

So the split is not the cost. The parent comparison carries the whole
commit, and master runs the lane within a few percent of the parent. The
backlog item stays, at low priority, for `writerMap`'s +8 KB, which has
not been isolated yet.

**Filed:** `okay2-persist-rest`. okay2-jdbc-writes ported okay-persist's
log only; the file and replicated stores, the wire, Raft and the durable
workflow are not on okay2.

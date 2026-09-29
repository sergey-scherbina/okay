- [ ] resume-late-merge-order — PRIORITY: HIGH (a broken guarantee in
      the default gate). `TestMergeOrder` "Channel.merge: each side
      arrives in exactly the order it sent" went RED on master
      (2026-09-29, source-zip-lost-pairs' `affected master staged`,
      round 18 of the default 20). BISECTED with OKAY_MERGE_ROUNDS=400:
      5da64e406 green 400/400; f50932cbe (adaptive-elementwise-small-ring,
      `Drive.resumeLate`: a late answer from a foreign thread sends the
      fiber home) RED at round 9; master RED at round 20. The shape: one
      side's run of about a ring's capacity (16) arrives AFTER the next
      run, e.g. evens ...1628, 1662..1696, 1630..1660, 1698...: a
      producer's elements from two continuations interleaved, which is
      what a fiber resumed twice (at home AND where the answer arrived)
      would give. Owner: resume-late-small-ring-cost's session was told
      in the room. THE LANE: fix resumeLate or revert f50932cbe, then
      TestMergeOrder at 400 rounds green. (2026-09-29)

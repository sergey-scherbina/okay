- [x] handle-forward-fast — DONE (2026-09-15), and BOTH of its
      numbers were wrong in the entry that proposed it. `Effects.handle`
      now re-emits a FORWARDED operation on the tree the way `relay`
      does and enters `Cont` only for one the handler claims.
      RESULT: `handlePrebuilt` 223.3 -> **154.2 us**, and the row that
      matters, allocation 2 869 306 -> **1 753 945 B/op**, which is
      `relay`'s number TO THE DIGIT (the build-on-every-call pair
      agrees too: 2 154 017 on both sides). The 1.51x gap is 1.03x.
      WRONG #1, the entry's mechanism: the `shift` per forwarded
      operation was only a THIRD of the gap.
      WRONG #2, and this is the keeper: the other two thirds were
      introduced by this lane's own first version. Trampolining every
      handled operation through `Free.defer` cost 59 us of the 61 —
      a `Defer` whose continuation is `Pure` rotates into a LEFT-nested
      `Bind`, left-nesting is the one shape `resume` rewrites, so each
      handled operation taxes every operation after it. Found by
      removing the node and measuring (row `hff-defer-cost`): 151.3 us,
      and only the 100k-handled test fails, by StackOverflow. The
      shipped version keeps the node ONLY for a handler that really
      captures; one that does not answers with `Cont.Pure`, and the
      loop goes on from the answer with a tail call
      (`Cont.onAnswer`, inline, no `Option`, no closure).
      The order of work was the entry's own condition and it held:
      TestHandleForward's seven tests were written first, passed
      against the definition, and four of them were watched to FAIL
      against a deliberately wrong forwarding arm before the real one
      was written. Disqualifier checked: `fusedSWr` 13.2 us /
      122 640 B against the recorded floor's 13.7 / 122 641.
      Rows `hff-*`.

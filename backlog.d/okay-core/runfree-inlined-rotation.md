- [ ] runfree-inlined-rotation — the FIRST attempt is REFUTED and the
      refutation is the useful part. Diagnosing with
      -XX:+PrintInlining found the real cost of free-one-rotation:
      `Free.resume` is 352 bytes against FreqInlineSize 325 and
      inlines into NO caller. Shrinking it (the two cold `Defer`
      shapes into their own method, 352 -> 306, confirmed to go from
      "hot method too big" x8 to "inline (hot)" x8) made every lane
      WORSE — effCont24 1.435, fusedSWr 1.063, and relayPrebuilt
      1.065, worse than the 1.039 it was meant to fix. Allocation
      identical throughout. WHY, and do not retry it: `resume` is a
      LOOP, and inlining a loop into callers that are themselves loops
      (`fold`, `runFree`, relay's `loop`) nests loops and costs more
      than the call. The threshold was protecting these lanes. Rows
      `rfinline-*`.
      STILL UNTRIED: the entry's original idea, `runFree` keeping its
      own inlined rotation while `resume` stays as it is — which is a
      third copy rather than a smaller shared one, and after the above
      it must be measured before it is believed. Also worth knowing
      before anyone starts: relay's 3-4% buys four rotation copies
      folded into one, and that trade was already accepted once.

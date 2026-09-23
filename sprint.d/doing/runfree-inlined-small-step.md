- [~] runfree-inlined-small-step — runfree-inlined-rotation's side
      finding: a hand-fused `runFreeInlined` (resume's rotation
      folded into runFree's own loop, no call to `.resume`) read
      0.904x on `effCont24` (`effSteps[Free](24)(produce(0))`, a
      24-step program) — bytes byte-for-byte identical, three quiet
      rounds agreeing — while the SAME code read 1.02–1.05x WORSE on
      every 10 000-op row-handling lane (`relayPrebuilt`,
      `handlePrebuilt`, `handleCapture`), which is why the parent
      entry was refuted rather than shipped. TRIGGER: someone
      profiling a small-step `Cont`/`Free` program (tens of ops, not
      thousands) who wants to know whether the win generalizes —
      sweep the step count (4, 24, 100, 1000) on the same shape
      before deciding whether "small step count" is the real
      variable or `effCont24`'s own construction (rebuilt per
      invocation, unlike the prebuilt row lanes) is. NUMBER first.

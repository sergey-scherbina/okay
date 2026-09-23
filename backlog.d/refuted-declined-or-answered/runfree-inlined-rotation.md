- runfree-inlined-rotation — DONE 2026-09-23, REFUTED THE ORIGINAL
  ASK, with a side finding kept open. The entry's untried idea —
  `runFree` keeps its OWN copy of `resume`'s rotation, folded
  directly into its own tailrec loop, so it never calls `.resume` at
  all; `resume` itself and every other caller (`fold`, relay's loop,
  `State.run`, `Writer.run`) untouched by construction — was built
  as `runFreeInlined`/`runWithInlined` (private, measurement-only)
  and alternated against the shipping road on three quiet rounds,
  per-arm minima: `relayPrebuilt` 1.022x, `handlePrebuilt` 1.030x,
  `handleCapture` 1.054x — all WORSE, on the exact lanes the entry
  was written to help (relay's 3-4% cost). The first attempt's own
  explanation (409c06e2: "resume is a LOOP, inlining a loop into a
  caller that is itself a loop nests loops and loses more than the
  call") generalizes past the JIT-threshold mechanism that attempt
  used: HAND-WRITING one bigger loop is the same control-flow shape,
  and it lost the same way. Bytes identical on the clean pairs
  (handleCapture's own +0.24 B/op is noise at that scale, not a
  structural difference). CODE REVERTED — `runFreeInlined` never
  shipped even privately.
  SIDE FINDING, NOT what the entry asked, kept open with its own
  trigger: `effCont24` (a 24-step `Cont`-carrier program, not a
  10 000-op row) read 0.904x with the SAME code, bytes byte-for-byte
  identical, all three rounds agreeing. One benchmark shape is not
  enough to act on — filed as `runfree-inlined-small-step` with the
  number and the question (does a small step count change which way
  the nesting cuts?), not built. Rows `rfir-*`.

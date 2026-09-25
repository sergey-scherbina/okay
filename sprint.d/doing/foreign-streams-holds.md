- [ ] foreign-streams-holds — the next two extensions of the engine
      typeclass (operator, 2026-09-25: "Продолжай стриминг и холдс").
      `Holds[M]`: an object HELD in the interpreter — a model fit once —
      as a recipe materialised once per pooled interpreter and passed to a
      map (`flow.mapHeld[B](module, fn, held)`). `Streams[M]`: a STATEFUL
      per-partition stage — `open`/`step`/`finish` on the far side, the
      state a held object in ONE interpreter kept for the partition's life
      (`flow.streamIn[B](module, "open", "step", "finish")`); a death loses
      the state and the partition recomputes elsewhere, the cluster's own
      fault model. Python, R and the JVM; specs/foreign-map-reduce.md stage 4.

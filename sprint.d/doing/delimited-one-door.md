- [ ] delimited-one-door — the operator's ask (2026-10-02, cont-js-depth's
      design conversation): ONE door into the machine. `Delimited` gains a
      run to head form (`runHead`) and `run` is it under a boundary;
      `Frames.run`/`enterAt` are closed (the machine's own), every caller —
      Cont's strict-k bridge, Shift's nested runs and `Stacked`, TestKont,
      KontBenchmark — goes through the interface; the reference implements
      `runHead` and the differential oracle drives programs through it.
      And `Delimited` is a `ParaMonad` (`pure` in its order, `flatMap` =
      `bind`). (2026-10-02)

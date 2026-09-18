- [x] dataflow-source-log — BUILT, and the entry outlived it by a day.
      Checked against the files 2026-09-18: stage 11's boxes are ticked
      or explained in specs/dataflow.md, and the tests are in
      okay-cluster — `TestSeek` (a resumed job opens AT the journal's
      epoch and reads `total - Σpositions`), `TestStagingTopic` (the
      topic writer), `TestStaged` (the coordinator dies between the
      append and the journal commit, at four epochs, and the output
      holds every pane once — with the dedup state being the OUTPUT,
      read back from the tail by a process with no memory).
      `dataflow-windowed-seek` MEASURED the windowed case and
      `dataflow-horizon-seek` (dbfeb48e) built the road it chose.
      WHAT THE ENTRY ASKED FOR AND DID NOT GET, deliberately:
      `Sink.stagingTo(topic)` as a METHOD. okay-cluster depends on
      okay-persist in TEST scope only, on purpose — `Checkpoint` is
      two methods over bytes and the STORE belongs to the caller — so
      a `stagingTo` would drag a store into a compile graph that stops
      at okay-codec. The seam that exists is `Sink.staging(...)(move)`
      with the topic writer in whatever module owns the store, which
      is forty lines and where a store belongs. specs/dataflow.md
      marks that box `[~]` with this reason rather than open.
      WHAT IS ACTUALLY LEFT of stage 11 is one box and it is not a
      decision: the same exactly-once run on `KafkaStore`, which needs
      a BROKER. It stays in the spec, Live-tagged like every other
      suite that reaches outside the JVM.

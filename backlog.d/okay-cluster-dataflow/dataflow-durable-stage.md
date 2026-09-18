- [x] dataflow-durable-stage — CLOSED by being answered smaller than
      it was asked. The staging contract already requires the writer
      to record the epoch in ONE write with its rows; a writer that
      cannot be atomic cannot have exactly-once, and that is a
      property of the store rather than of the engine. Nothing was
      built to justify the lane. What the lane DID produce is the
      defect it turned up: a resume of a run that was already over
      retired the tail panes a second time out of one partition's
      half — 29 of 3 204 wrong — because a finished run did not record
      that it was finished. It does now (`Folded.done`).

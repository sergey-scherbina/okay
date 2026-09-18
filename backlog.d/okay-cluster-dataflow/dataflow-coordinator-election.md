- [x] dataflow-coordinator-election — LANDED as stage 10, and it was
      a lane rather than a line for the right reason: the wiring is
      small, and the FENCE it forced is the part that mattered. A
      deposed coordinator now stops at its next commit instead of
      writing over its successor.

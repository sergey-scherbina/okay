- [x] dataflow-coordinator — LANDED as stage 8. `Wire.state` makes
      the fold a value, `Checkpoint` is where it goes, and a second
      `Cluster.stream` over the same journal picks the run up. It was
      an assembly: the seam binds to okay-persist's compacted log in
      eight lines, and the reason it is a `save` call rather than a
      barrier protocol is that the epoch loop is lock-step.

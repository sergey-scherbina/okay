- [x] dataflow-netem — LANDED (TestNetem, default gate). Tolerance 3
      carries 20% loss with certainty, the knee is at 30%, half the
      runs die at 50%. And the finding: with burial OFF a 70% wire
      still finishes every run — on a lossy wire the loss never ends
      a run, the burial policy does, because three lost packets are
      read as a dead machine. `tolerance` is a parameter of
      `Cluster.run`/`stream` now; telling the two failures apart BY
      THE ENGINE needs a real wire (stage 12).

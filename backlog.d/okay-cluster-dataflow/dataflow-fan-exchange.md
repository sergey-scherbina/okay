- [x] dataflow-fan-exchange — CLOSED by its own condition. It said to
      consider this only after dataflow-complete-panes, because that
      lane might remove the merge instead of parallelising it. It did:
      122 679 accumulators reach the coordinator where ~2.9 million
      did, three orders of magnitude under the exchange's crossover.
      There is nothing left for an exchange to buy here, and a stage
      that wants one can still say `Finish.Shuffle`.

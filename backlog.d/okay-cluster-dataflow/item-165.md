- [ ] (SUPERSEDED, kept for the reasoning) dataflow-complete-panes: 84%
      of the Wrocław run was the sliding stop-window sink, and not
      because the windowing is slow: the job makes 362 983 stop panes,
      every partition holds most of them, and the coordinator merges
      ~2.9 million accumulators on ONE thread.
      `OkayLane.parallel` does not parallelise that merge, it avoids
      it — it emits at the slice every pane no other slice can touch
      (`p.start > hi(i-1) && p.end <= hi(i) - back`) and hands back
      only the handful that span a boundary. The engine already
      computes `hi`: it is the prefix-maximum array gathered for the
      watermark seeding. What is missing is `back`, the greatest
      backwardness, which is two more columns in the same pre-pass
      (each partition's local backwardness and its minimum event
      time). The bar is MeasureWroclawFlow's table: 121 ms against
      the hand-written 22.

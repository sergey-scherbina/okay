- [ ] dataflow-ceiling — publish the honest number at which a cluster
      becomes the right answer: throughput and state size past which
      the embedded engine should not be used. Flink never publishes
      one; we can measure ours, and the measurement IS the product
      claim (ROADMAP P13 item 3). Needs: a sweep of key cardinality
      and window count on the Wrocław job until the box is the
      bottleneck, with the bottleneck NAMED (heap, GC, cores).

- [ ] indexed-effects-1-shared-get — stage 1 of specs/indexed-effects.md:
      `PState.Threaded.get[S]` as ONE shared node for every `S`, the
      `SharedOps.getNode` cast (`Get` has no fields, a node is
      immutable); TestState asserts `get[Int] eq get[String]`. No
      benchmark (the arc's rule); the expected byte count is in the
      spec's Deferred measurements.

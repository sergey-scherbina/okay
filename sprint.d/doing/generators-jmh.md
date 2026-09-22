- [~] **generators-jmh** — price the Gen pipeline: `map`/`filter`/`take`/
      `toList` over `unfold` vs hand-written `Writer.loopWith` +
      `Writer.foldUntil`, and vs `Source.runCollect` on the same 10 000
      elements; B/op under `-prof gc`. Expected parity with the hand-written
      road (`map` IS `Writer.map`, readers ARE `FoldUntil`); the question is
      whether the value class and `splice` add a frame per element. Trigger:
      the generators lane landed 2026-09-22 without it (specs/generators.md,
      Results).

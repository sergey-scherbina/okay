- [ ] handle-frames-loops — specs/handle-frames.md stage 3: the bespoke
      handler loops lazy and upgrading, one per lane, each with its red
      nested-depth test on 128 KB and Scala.js and a fold-vs-frame
      differential with a call count: Writer (10 `.resume`s), Gen (9),
      Generate (7), Resource, Lexical, Throws, Supply, Once, Maybe, Logic,
      Prob, Sim, Refs, Chronicle, ... and the modules' 31 files. An
      unconverted loop stays correct and nests as before. (2026-10-02)

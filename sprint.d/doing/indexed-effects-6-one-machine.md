- [ ] indexed-effects-6-one-machine — the follow-up stage 4 named: the
      unstacked `Delim.run`/`runNested` route their program through
      `Delim.Stacked.at` onto the typed machine, and the unstacked
      machine (Segs, Frames, Cut, split/copy, reify, loop, step — ~300
      lines of Delim.scala) is deleted. ONE machine: typed operations at
      their own types, unstacked ones embedded at the two claims, the
      forwarding variant (`runNested`) kept for embedded captures. Every
      Delim consumer (okay-ui, okay-agent, okay-llm, collect/resumable,
      the Delim suites) runs on it: the gate is the full `affected
      master staged`, both stages. No benchmark (the arc's rule); the
      cost of `at`'s node per operation is the measurement lane's first
      question.

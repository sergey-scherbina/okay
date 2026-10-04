- [ ] foreign-effects-members-rest — what foreign-effects-members left
      (specs/foreign-effects-in-tree.md, stages 1, 2 and 4): cats-effect
      laws on `p.toIO` over a mixed IO + Async program; cancellation both
      ways through `toIO`/`toZIO`; ZIO's `catchAll` (the whole program's,
      so it needs the rest of the program, not a step); stage 4, `direct`'s
      `.?` putting an IO/ZIO in the tree instead of awaiting it — a
      semantic change, ask the operator first.

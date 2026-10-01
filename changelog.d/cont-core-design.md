## cont-core-design - the continuation core is λ$: `$` and `shift0` over segments, everything else derived; three optimizations measured back in

The operator's ask (2026-10-01): clean the continuation design down to
what is necessary and make that correct; optimize after, with numbers.
Spec: specs/cont-core.md (Results lists every step and its commit).

- **The core is λ$ exactly.** Two operations, `Dollar0` (`ret $ body`)
  and `Shift0`; `Stack = Done | Run | Dollar | Cat`, `Frames = End |
  Frame`; one machine start, "a focus over a stack". `reset` is
  `pure $ ·`, `shift` is `shift0` under a `reset`, `abort` drops `k`.
  The prompt the machine sees is `Cont0.Delimiter[Y, I]`: it carries
  its delimiter's index, so finding the delimiter by identity types
  the captured `k` and the stack under the body (the machine's
  `rebase` claim gone; the index claimed at the door that knows it).
- **Gone:** the re-entry count (`Shots`, `Enter`, `dollarResumed`),
  the `under`/`strict`/`bare` flags, `Kept`, the fused delimiter,
  five entries, `plainly`, `Cont0.identity`, `Rev.reverse`/`splice`,
  the runner's stack reader (`StackSwitch.more`, `Cont.Gauge`,
  Native's `ThreadInfo`; `StackRoom` and okayJdk22 kept). And from the
  API: `control`/`control0` (no module used them), `Lexical.shallow`,
  `ShallowClauses`, `Delim.Stacked.Plain`.
- **Changed behaviour:** a `Lexical.tail` installation in a row WITH
  `Delim` is installed deep — the state in the continuation — and
  answers what `deep` does under a multi-shot capture from outside,
  where it threw `MultiShotAcrossTail`. Cont's strict `k` counts levels
  and switches stacks at the first room on every platform (no exact
  road).
- **Kept as an optimization over one leaf:** `ContMacro` (tail bodies
  as values, answer-using bodies as programs over a lazy `k`); the
  facade's leaves are one private `leaf`.
- **Measured back in** (history.d rows per step): `Cat` (resumption
  O(1); its first cut lost and is recorded), `nearest` (also at a
  resumed `k`'s head, `inline` after C2 refused it at one arm),
  `enterAt`. Against master: statePara 1.04-1.08x, fib100 1.03x,
  stateDeep 1.03x, stateLexDeep 0.99x, writerTell 0.95-0.98x,
  dollarResume 1.11-1.16x, layered 1.17x, contAnswer 1.21x, generator
  1.23-1.26x, bare install/pop 1.24-1.28x (the price of the unfused
  delimiter; backlog cont-core-remaining-costs).
- Docs: book chapter 11 is "Two captures" (and why `control` left);
  cont-stack.md's room table counts on every platform; the appendix,
  typepedia, tutorial, guide, many-instances and the history chapter
  follow. Board: frames-array-stack parked with its probe numbers.

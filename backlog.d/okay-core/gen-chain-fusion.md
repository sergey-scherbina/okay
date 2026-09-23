- [ ] gen-chain-fusion — a `Gen` chain (`map`/`filter`/`take`/`drop`,
      specs/generators.md) is one element-wise `resume` walk PER STAGE:
      `g.map(f).filter(p).take(n)` walks every element three times, each
      stage re-telling into the next. `Pipeline.optimize` (okay-stream)
      already has the rules — map∘map, filter∘filter, take pushdown —
      and they transfer to `Gen` at CONSTRUCTION (a run-time rewrite,
      no macro); inside a `generator` block a comprehension over a `Gen`
      could go further, compiled by loops v2 into ONE loop. TRIGGER:
      `generators-jmh` (sprint, the sibling's) reporting a per-stage cost
      above noise against the hand-written `Writer.loopWith` road; if it
      reads parity, this entry is answered without code. NUMBER: the
      3-stage chain over `unfold` at 10 000 elements before/after, B/op.
      Filed from the staging survey (2026-09-22, ranked 3 of 4).

- [ ] tod-schema-guided-retrieval — Labruna, Bonetta, Magnini (RANLP
      2025, "Task-Oriented Dialogue Systems through Function
      Calling", MultiWOZ 2.3): let the model call a schema-guided
      query that fetches only the needed KB entries, instead of
      putting the whole KB in the prompt; accuracy up, tokens and
      time down, the gap widening as the KB grows. Their BASELINE is
      not ours — we never put a KB in a prompt, and the demo's
      central claim is that nothing reaches the projection except
      through a tool. What IS new for us: deriving the RETRIEVAL tool
      from the store's own `Schema` instead of hand-writing one tool
      per query shape (okay-sql's typed layer, okay-match's
      registry), so a new domain field becomes a queryable slot with
      no new tool code. Pairs with a KB-size sweep — tokens per turn
      and latency, full-KB against schema-guided — in
      docs/benchmarks.md, because at the demo's current KB size the
      effect is invisible by construction.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

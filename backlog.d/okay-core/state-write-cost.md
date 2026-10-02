- [ ] state-write-cost — PRIORITY: LOW, the price of State as Get and Update
      (state-get-update + builtins-through-forms, 2026-10-02; the operator:
      "замедление не страшно — потом будем оптимизировать"). 1000 get+set
      take 13.96 µs and 198 KB, where the old Get/Set/Modify/Update hand loop
      took 11.78 µs and 182 KB (1.19x, +16 B a write). The split, measured: the
      two operations account for 1.14x, because a set is `Update(Put(s))`, a Put
      and a pair where Set was one node. The form's loop (`Handler.stateOf`)
      adds 1.04x over the hand loop on the same two operations. Candidates,
      each to be measured alone: a `Put` arm in the loop
      (`case Update(Put(s))` answers with no pair), and the clause's pair
      scalar-replaced where the JIT does not do it today. TRIGGER: a profile
      with State writes in it, or the 1.19x on a consumer's lane.

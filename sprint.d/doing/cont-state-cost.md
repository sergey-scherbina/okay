- [ ] cont-state-cost — WHAT IS DONE (stage 42, 2026-10-07): the `Answer`
      arm feeds the next frame, the handler resolved once per capability —
      `stateAnswering` 17.6 µs against the classic's 18.2, `writerTell`
      12.5 against 27.7, `handlePrebuiltAnswering` 66.8. LEFT: a row
      program's `Cap.perform` (Free.scala) builds a `Target` per operation.
      Caching it per context needs `Cap` to carry its context as a VALUE
      (`c: C` with `t: Target[E, c.Here]`, `perform(op)` over it, `C` a
      singleton at every use so `cap.c.Here` is `c.Here`) and `Has.lift` to
      take the inner context as one, typed `In[?, c.type]`, so that two
      paths of one type meet. Add a JMH lane for a row program first
      (none exists), then measure.

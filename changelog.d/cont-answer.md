## okay-cont: an operation answered in place is one node

Performance, lane cont-answer (specs/freer-min.md, stage 36). The kernel's
eighth node, `Answer(op, c, by)`: an operation whose handler answers in
place (`Answered`, found through the context's types) is one node the
machine evaluates when it gets to it — where `Perform.answered` built a
`Delay` of a closure of a `Return`, three objects. `stateAnswering` 21.9 →
20.3 µs (classic 18.5), `writerTell` 18.5 → 14.8 (classic 27.7),
`handlePrebuiltAnswering` 133.5 → 128.5 (classic 130.4).

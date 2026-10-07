## okay-cont: an answer in place feeds the next frame; the handler resolved once

Performance, lane cont-state-cost (specs/freer-min.md, stage 42). The
machine's `Answer` arm hands the value straight to the next frame — no
`Return` made, no second pass of the loop — and the node carries the
answering handler itself, resolved once per capability (`Answered.at` in
`Perform`), not a chain of `outside` calls per operation. `stateAnswering`
20.7 → 17.6 µs (the classic `stateEffect` 18.2), `writerTell` 14.8 → 12.5
(classic 27.7), `handlePrebuiltAnswering` 128.5 → 66.8, general
`handlePrebuilt` 171 → 165, `handleForward` 200 → 194.

## handle-frames-forms — relay and translate nest without the host stack too

handle-frames' stage 2: `Effects.relay` (the answer form, so
`Handler.answer`) and `Effects.translate` (the into form, so
`Handler.into`, `interpret`, `tracing`) answer a run object a running
machine steps into as a frame, and upgrade when they meet a nested run.
100 000 nested of each run on Scala.js, where they overflowed; the frame
agrees with the fold on the answer and on how many operations the clause
was asked (a restart mutant reds it). relayPrebuilt 1.01x, handlePrebuilt
0.99x. The bespoke loops (Writer, Gen, Generate, ...) are queued as
handle-frames-loops. specs/handle-frames.md.

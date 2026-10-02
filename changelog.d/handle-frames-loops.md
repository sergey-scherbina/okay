## handle-frames-loops — the bespoke handler loops nest without the host stack

handle-frames' stage 3: one state frame (`HandleFrames.stateful`: state,
operation, `resume`, `ret`) and every loop that threads a state on it —
Writer (run/collect/censor, foldUntil, map, expand, listen, widen),
Supply, Refs, Once, Chronicle, Producer (fold, foldUntil, each),
State.zoomWith, Lexical.walk, Maybe.prune, okay-agent Memory, okay-py
PyStream.holding, Source.fromProducer/toProducer. Each is lazy (a run
object), a frame when a machine meets it, and upgrades when it meets a
nested run: 100 000 nested of each on Scala.js (TestHandleFramesLoops).
Lexical's "machine outside the walk" multi-shot, which escaped loudly,
now answers deep's answer. Left as folds on purpose, each correct and
nesting as before: Throws.rows and Resource (a JVM try per step), Logic's
search, Gen's walks, okay-stream's Pipe pairs and the interpreters into
Async, the value-answering runners. specs/handle-frames.md.

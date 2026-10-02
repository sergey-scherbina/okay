## handle-frames-loops — the bespoke handler loops nest without the host stack

handle-frames' stage 3: one state frame (`HandleFrames.stateful`: state,
operation, `resume`, `ret`) and every loop that threads a state on it —
Writer (run/collect/censor, foldUntil, map, expand, listen, widen),
Supply, Refs, Once, Chronicle, Producer (fold, foldUntil, each),
State.zoomWith, Lexical.walk, Maybe.prune, okay-agent Memory, okay-py
PyStream.holding, Source.fromProducer/toProducer. Each is lazy (a run
object) and a frame when a machine meets it: 100 000 nested of each on
Scala.js (TestHandleFramesLoops). AND THE RULE CHANGED for every form,
the landed ones included: a fold no longer hands itself to the machine
at the first nested run — that put `State.run(Writer.run(p))` on machine
frames, 7.4x (SplitBenchmark.mixedList). It forces the nested run as a
fold, `HandleFrames.Limit` (32) deep, and only there runs it as a frame
on a machine; mixedList 0.95x, writerShip 0.76x, stateSmall 1.02x,
handlePrebuilt 1.02x, relayPrebuilt 0.89x (its walk an object per run).
Lexical's "machine outside the walk" multi-shot, which escaped loudly,
now answers deep's answer. Left as folds on purpose, each correct and
nesting as before: Throws.rows and Resource (a JVM try per step), Logic's
search, Gen's walks, okay-stream's Pipe pairs and the interpreters into
Async, the value-answering runners. specs/handle-frames.md.

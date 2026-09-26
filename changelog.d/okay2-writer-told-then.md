## okay2-writer-told-then — okay2's Writer views no longer lose a told value to a throwing continuation

The Scala 3 core's fix (source-merge-via-ready's `Writer.toldThen`)
ported to okay2: `Writer.uncons`, `Writer.unconsIn` and the two linear
iterators (`Stream.feedStream`, `Stream.writerStreamIn`) applied the
continuation after a told value as they handed the value over, and a
throw from it took the value along — a program telling 1, 2, 3 and
then throwing came out as 1, 2. `Writer.toldThen` still applies it at
that moment (pull counting depends on it) but holds a throw back as
the rest, a `Delay` that throws when next stepped. `Gen.Stepper` was
already lazy and is untouched. `okay2.TestWriterToldBeforeThrow`, four
views, watched failing on every platform first.

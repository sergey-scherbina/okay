## fs2-effectful — effectful okay streams in fs2, and fs2 at an okay program

Operator ask, 2026-10-02 (cats-depth audit). `Fs2Streams` in okay-fs2: a
`Source` as an fs2 stream in any cats-effect `F` (an interrupted stream
cancels the source's pending `Await`), an fs2 IO stream as a `Source`
parking no thread (cancelling the okay side cancels the fs2 fiber), a
`Stage` as a `Pipe` that pulls only what it asks for, and an fs2 pipe
over a `Source`. An fs2 stream with `parEvalMap` compiles AT
`CatsEffect.Program`. A Pipe as a pull-driven Stage is declined in the
spec (the pipe owns its input). 8 tests; specs/fs2-effectful.md.

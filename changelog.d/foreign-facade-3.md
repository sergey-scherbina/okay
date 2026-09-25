## foreign-facade-3 - Streams[M]: a Flow through a frame function one frame per chunk, derived for every language with Frames

Stage 3 of specs/foreign-facade.md. `Streams[-M]` takes a cluster
`Flow[A]` through a frame function in frames of `batch` rows and answers
a `Flow[B]` (`Road.flow`); it is DERIVED for every module type with
`Frames` (`Streams.viaFrames`), so Python, R and the JVM have tier 3
without a line per language — the next frame goes when the last
answered, which is the back-pressure, and neither side holds more than
a frame. The conformance body `streams` proves the bound by counting: a
`Frames` that records what it is handed sees ten frames of at most
1 000 rows for 10 000 rows, every row back in order (TestFacade); 20 000
rows over python3 in frames of 4 096 (Live). The carrier is `Flow`, not
a bare `Source[Chunk[A]]`: tier 3 is `mapIn`'s own road given a name
(Results).

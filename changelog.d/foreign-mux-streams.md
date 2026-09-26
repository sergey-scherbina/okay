## foreign-mux-streams — streams the far side drives, under credit (2026-09-26)

Part 3 of foreign-mux-duplex. A Go or Rust function on a multiplexed wire
sends a stream with `okay.Emit` / `okay_emit`, running ahead of the Scala
consumer by at most the credit it granted; each chunk taken grants one
more. `Py.stream[O](address, credit)` reads it as a source inside
`Py.releasing`, which cancels a stream a consumer stopped early. New
operations `Stream`, `Pull`, `Cancel`: routed by the pool to the stream's
worker, journalled by `Durable`, not replayed by the supervisor. Tests on
every muxed Go and Rust row: order, the bound counted on the far side, the
early stop that makes the far function return. docs/one-language.md, "A
stream the far side drives".

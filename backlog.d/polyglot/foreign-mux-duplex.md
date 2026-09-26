- [ ] foreign-mux-duplex — the wire multiplexed by `id`, with credit-based
      streams in both directions (specs/foreign-one.md, the first text of
      stage 5; narrowed by Decision 17). A reader per link matching answers
      by id, several requests in flight on one worker, `{"stream","chunk"}`
      /`{"stream","credit"}` messages, `okay_poll` in process, `Durable` and
      the supervisor keyed by `(id, seq)`. Trigger: a caller that needs a far
      side to run ahead of its consumer, or two programs interleaved in one
      Go/Rust process — until then a source is a pulled held iterator
      (`Py.source`) and parallelism is the pool's.

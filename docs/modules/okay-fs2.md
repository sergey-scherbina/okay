# okay-fs2

Streams cross CHUNK FOR CHUNK — both sides are chunked, nothing is
re-buffered.

- `toFs2` — a pure fs2 stream via `unfoldChunk` over `Chunks.pull`;
  chunk boundaries preserved (4,4,2 stays 4,4,2); laziness crosses.
- `fromFs2` — THEIR backpressure primitive: the fs2 side offers into
  a bounded `cats.effect.std.Queue` (offer suspends the IO fiber — no
  thread blocks on their runtime); our side takes by parking a
  virtual thread. Each side waits its own native way. An infinite
  fs2 stream under a small capacity is fine: take a little, the rest
  stays suspended.

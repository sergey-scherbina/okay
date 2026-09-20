## merge-knows-its-producers - the seams that know their producers stop guessing

The operator's question: a channel is a stream written from one side
and read from the other — can it be plain streams? Half already is
(`pipe`/`through`: two programs alternated by the interpreter, no
buffer, exact order), the other half cannot be (two fibers need a
meeting cell that outlives both continuations, and a `Free` tree is a
value), and what the framing buys is this: a seam between streams
KNOWS ITS SIDES. `Channel.merge` has exactly two producers and
`Channel.buffer` exactly one, yet both built `Channel[A](capacity)`,
the `growing` default — a buffer whose reason to exist is a producer
count nobody knows, and whose one-shot swap is where a producer's own
order can break once (BUGS.md `growing-stale-route`, the documented
trade of 2026-09-18).

`Channel.forProducers(n, capacity)` is the one place the choice is
written: two fixed parts from the start for a merge
(`relaxed.parts(2).each(capacity)` — no adoption, no swap, each source
on one ring for its whole life), a plain ring for a buffer. `merge`,
`mergeChunked`, `mergeFlushing`, `buffer`, `bufferChunked` go through
it; `-Dokay.channel.known=growing` / `OKAY_CHANNEL_KNOWN` keeps the
old arm for A/B. `Channel.apply`'s default is untouched — it still
does not know its producers.

THE LAW, AND THE OLD SHAPE FAILING IT. `TestMergeOrder` states exact
per-side order for `Channel.merge`, `Source.merge` (elementwise and
chunked) and `Channel.buffer`, a fresh consumer thread per round.
Under the growing arm with 20 burners and 1 500 rounds it FAILED at
round 540 — the three-sightings defect reproduced by a gate test, not
a probe — and the sized arm held the same run (spec Results).

THE A/B (spec Results, history.tsv `ckp-*`; two alternating rounds,
own JVMs, a no-channel control that held within 5%): `Channel.merge`
2×500 −4% with the sign both rounds; `Source.merge` chunked 2×2000,
the capacity sweep at 256/1024 and both `Channel.buffer` rows at
parity; the elementwise `Source.merge` swings both ways as it always
has. ONE LOSS, named: `Chunks.merge` 2×500 reads 3–8% slower, because
its ~62 pushes never trip growing's every-64th-push sampling, so the
old arm stays a single ring there while two parts pay a scan per pop
— the price of exact order on a merge too short to contend.

Also: `Queues.fifo`'s doc said the growing default "did NOT happen";
it did (default-retable, 2026-09-08), corrected; docs/queues.md's
choosing-for-order table has the merge/buffer row; `Source.merge`'s
doc states the exact promise.

Files: okay-stream/src/main/scala/Channel.scala, Source.scala,
Queues.scala; okay-stream/src/test/scala-jvm/TestMergeOrder.scala;
specs/channel-known-producers.md; docs/queues.md; src/jmh/history.tsv.

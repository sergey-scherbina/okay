## foreign-host-streams — the stream the host feeds, and the duplex transform (2026-09-26)

The other half of the multiplexed wire's streams. `Py.stream(...).feeding(in,
chunk)` sends an iterator's elements, in chunks, into a Go or Rust function
that reads them with `okay.Next` / `okay_next` at its own pace — each chunk
it takes grants the host one more, so the host runs ahead by at most the
credit — while the call's own stream comes back; one call is then a
full-duplex transform. The input is sent by a feeder thread, not by the
program that reads the output: one program doing both could wait on the
far side while the far side waits on it (specs/foreign-one.md Decision
27). Tests on the muxed Go and Rust rows: the host held at its credit,
counted on the host, and a dedup both ways at once. docs/one-language.md,
"A stream the host feeds, and both at once".

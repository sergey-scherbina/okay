# okay-stream — streams, channels, chunks and the buffers under them

Until 2026-09-18 all of this lived in the core. It moved out because
the dependency graph said it could: the whole control layer — `Cont`,
`Free`, `Effects`, `Monad`, `Delim`, `State`, `Direct`, `Par`,
`Resource`, `Logic`, `Throws`, `Validated`, `Static` — never named a
channel, a source or a chunk in code. Every apparent reference was a
comment.

So this module is the data half, and the core is the control half; a
program that only computes need not depend on the machinery for moving
bytes.

## The pieces

| | |
|---|---|
| `Chunks` | chunked collections — the unit both the local fold and the distributed one move |
| `Channel` | the queue between two threads of control, with the backpressure |
| `Source` / `Writer` | a stream as a program that TELLS: `Unit ! Writer % A + Async` |
| `Stage` | await and tell — a transducer, which is what every framing, parser and protocol in this library turns out to be |

## Further

| | |
|---|---|
| [`docs/modules/okay-stream.md`](../docs/modules/okay-stream.md) | the pieces and what moved |
| [`specs/core-modules.md`](../specs/core-modules.md) | why the split, and what the dependency graph actually said |
| [`specs/chunked-streams.md`](../specs/chunked-streams.md) | the design of the chunked layer, with its measurements |

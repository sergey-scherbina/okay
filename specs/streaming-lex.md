# okay-lex — streaming tokenization

## Overview
Tokenization as its own small module: a lexer is a `Stage[In, Token, S]`
(see stage-pipeline.md) — a state machine that consumes input elements
and emits zero or more tokens per step, with the state carrying
whatever crosses a chunk boundary (the half-eaten prefix of a token).
TOTAL by design: every input tokenizes; what cannot be recognized
becomes an Error-channel token, never a failure. Serves two masters
with one interface: scanner-style lexers for syntaxes (uniml dialects)
and dictionary-driven segmenters (BPE/SentencePiece live in okay-llm,
implemented against this same Stage shape).

## Interface
```scala
final case class Span(offset: Int, line: Int, column: Int, length: Int)

enum Channel: case Syntax, Trivia, Comment, Embedded, Error

final case class Token[+K](kind: K, lexeme: String, span: Span,
                           channel: Channel = Channel.Syntax)

/** a lexer: a stage from input elements to tokens; S is its state,
 * exposed for incremental relexing */
type Lexer[K, S] = ... // Stage[Char, Token[K], S] built from a step function:
trait Scan[K, S]:
  def init: S
  def step(s: S, c: Char): (S, Chunk[Token[K]])   // zero or more tokens out
  def flush(s: S): Chunk[Token[K]]                // end of input: finish the tail
  // the same step, writing into a sink instead of answering a pair;
  // the default delegates to `step`, so this is additive
  def stepInto(s: S, c: Char, out: Growable[Token[K]]): S

/** a scanner written on the sink road: implement `stepInto` and get
 * `step` (final) for the callers that still want the pair */
trait ScanInto[K, S] extends Scan[K, S]

def lexer[K, S](sc: Scan[K, S]): Stage[Char, Token[K], S]
```

### The two roads, and why there are two
`step` answers `(S, tokens)`, which allocates a `Tuple2` for EVERY
input character and a `Vector` for every token — priced at ~19% and
~23% of lexing's ~171 bytes per character (docs/benchmarks.md §10).
`stepInto` hands the scanner the collection the driver is filling
anyway, so a scanner that overrides it allocates neither.

It is ADDITIVE by construction: `stepInto`'s default is `step` plus a
`++=`, so every scanner written before it existed keeps working and
costs exactly what it cost before, and `ScanInto` (where `stepInto`
is abstract and `step` is final on top of it) makes the mutually
delegating pair that would loop for ever impossible to write.

The drivers — `Scan.all`, `Scan.chunks`, `Scan.relex`, and the
hand-rolled loops in `Yaml.cst` and `Markdown.parse` — all read
`stepInto`. `Scan.stage` keeps `step`: a `Stage` emits tokens
one at a time through the pipeline, so it wants the pair.

Chunked variant over `Chunk[Char]` (a tight while per chunk) is the
performance path; `lexer` derives both from one Scan.

## Incremental relexing (in the contract from day one)
- The lexer state at every chunk boundary is a value (Scan is pure);
  a session may retain (offset, S) snapshots.
- Relex after an edit = resume from the nearest snapshot at or before
  the edit, run until the emitted tokens re-converge with the old
  token stream (same offset, same state) — tokens outside the damaged
  region are reused, not recomputed.
- Snapshots are the caller's choice (an editor keeps them, a batch
  run keeps none); the module provides the resume-and-reconverge loop.

## Behavior
- [x] a token spanning a chunk boundary is emitted once, correctly —
      the Scan state crosses boundaries as a value, the token lands
      in whichever chunk completes it (agreement across chunk sizes
      1..64 is the proof: a split or doubled token would break it)
- [x] totality: arbitrary bytes/chars produce a token stream (Error
      channel), never an exception
- [x] flush emits the buffered tail token(s) at end of input
      (an unterminated string becomes an Error-channel Str token)
- [x] spans are exact (offset/line/column) across lines
- [x] the JSON lexer (the proving dialect) round-trips: concatenated
      lexemes of all channels == the input, garbage included
- [x] incremental: after an edit, relexing reuses tokens outside the
      damaged region (probe: under half the input re-stepped; the
      key/rebase pair on Scan is what makes position-carrying states
      comparable across the shift)
- [ ] the sink road answers exactly what the pair road answers, for
      every scanner and every character (a scanner overriding
      `stepInto` and one that does not, over the same input)
- [x] chunked lexing agrees with element-wise lexing — Scan.chunks:
      chunk of chars in, chunk of tokens out, one tight while per
      chunk, the same Scan deriving both paths

## Out of scope
- parsing (okay-parse), codec semantics (okay-codec)
- BPE dictionaries (okay-llm; it implements Scan)

## Decisions
- **Separate module, and small** — the user's module policy: the
  smaller the module the better, rare exceptions aside.
- **Scan as a pure step function, Stage derived** — the state must be
  a value for incremental relexing and for chunk-boundary carry; the
  coroutine form is generated, not hand-written per dialect.
- **Error is a token channel, not an effect** — Throws never appears
  in a lexing pipeline; totality is the design invariant.
- **The sink road is an interface change, taken only once it had
  consumers** (scan-step-allocation, 2026-09-10). BACKLOG had refused
  it in September for a reason worth keeping: the lossless road served
  the tests and a damage fallback, and an interface exists for its
  callers. By the time it was taken, six main-source consumers read it
  — `Yaml.cst`, `Markdown.parse`, okay-rag's window splitter and code
  chunker, okay-llm's streaming structured parse, and the agent's BPE
  token count on every message — and the two hot scanners (`Json`,
  `Bpe`) are the two that moved.

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
def lexer[K, S](sc: Scan[K, S]): Stage[Char, Token[K], S]
```

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

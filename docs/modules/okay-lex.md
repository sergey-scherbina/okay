# okay-lex

> Total streaming tokenization: every character becomes a token,
> errors are a channel, the state is a value — which is what makes
> lexing chunked, snapshottable and incremental.

Depends on: `okay` (core). Pure Scala — cross-built for JVM and JS;
the suite runs on both.

## Guide

**A lexer is a pure step function.** `Scan[K, S]` consumes one
character at a time and emits zero or more finished tokens; `S` is an
ordinary immutable value. That single design choice carries the whole
module:

- *chunked lexing* is free — the state crosses chunk boundaries by
  being passed along, so a token spanning two chunks is emitted once,
  in whichever chunk completes it;
- *snapshots* are free — keep `(offset, S)` pairs and you can resume
  lexing from any of them;
- *incremental relexing* is resume + reconverge — run from the
  nearest snapshot before an edit until the state equals the old
  run's state at the corresponding offset; from there the old tokens
  are reused with shifted spans.

**Totality.** Nothing throws. Unrecognizable input lands on the
`Error` channel as tokens; an unterminated string is finished by
`flush` at end of input. The lossless law holds for every scanner:
the concatenated lexemes of ALL channels equal the input, garbage
included.

**Positions and reconvergence.** States that carry absolute positions
would never compare equal across an edit's shift — that is what
`key` (a position-erased fingerprint) and `rebase` (shift the
positions inside a state) are for. Reconvergence is only accepted
PAST the next newline so columns stay exact; when no convergence is
found the relex simply runs to the end — never wrong, at worst not
incremental.

## Tutorial

Lex a string in one go (`Scan.all` also collects snapshots):

```scala
import okay.lex.{Scan, Json as JsonLex}

val lexed = Scan.all(JsonLex.scan)("{\"a\": 1, oops}")
lexed.tokens.map(_.lexeme).mkString == "{\"a\": 1, oops}"  // lossless law
lexed.tokens.filter(_.channel == Channel.Error)            // the garbage, as data
```

The same scanner as a pipeline stage (chars await in, tokens tell
out, lazily) or over chunks (the performance path):

```scala
through(chars(text))(Scan.stage(JsonLex.scan))       // a token producer
Scan.chunks(JsonLex.scan)(Chunks.fromIterator(text.iterator, 64))
                                                     // Chunks[Token[K]]
```

Edit and relex incrementally — tokens outside the damage are reused:

```scala
val old = Scan.all(JsonLex.scan)(oldText, snapshotEvery = 64)
val re  = Scan.relex(JsonLex.scan)(old, oldText, newText,
            editStart, editEndOld, editEndNew)
// re.tokens == Scan.all(JsonLex.scan)(newText).tokens, with fewer steps
```

Write your own scanner: implement `init/step/flush` (plus
`key`/`rebase` if the state carries positions). `JsonLex.scan` and `Bpe`
in this module are two of the shipped examples — a JSON lexer and an
LLM tokenizer are the same machine — and okay-codec adds YAML,
Markdown and XML scanners over the same interface.

## API reference

| member | signature | meaning |
|---|---|---|
| `Span` | `(offset, line, column, length)` | an exact source position |
| `Channel` | `Syntax / Trivia / Comment / Embedded / Error` | what kind of material a token is |
| `Token[+K]` | `(kind, lexeme, span, channel)` | one token; `K` is the dialect's kind enum |
| `Scan[K, S]` | `init: S`; `step(s, c): (S, Vector[Token[K]])`; `flush(s): Vector[Token[K]]` | the lexer as a pure step function |
| | `key(s): Any` | position-erased fingerprint (reconvergence equality) |
| | `rebase(s, offΔ, lineΔ): S` | shift positions inside a state |
| `Scan.stage` | `(sc) => Stage[Char, Token[K], S]` | the scanner as a coroutine stage |
| `Scan.chunks` | `(sc)(chars: Chunks[Char]) => Chunks[Token[K]]` | chunk in, chunk out, one tight while per chunk |
| `Scan.all` | `(sc)(input, snapshotEvery = 64) => Lexed[K, S]` | lex everything, snapshotting |
| `Scan.Lexed` | `(tokens, snapshots, state)` | the result incremental relexing resumes from |
| `Scan.relex` | `(sc)(old, oldInput, newInput, editStart, editEndOld, editEndNew, snapshotEvery)` | resume, reconverge, splice |
| `Json.scan` | `Scan[Json.K, Json.S]` | the proving dialect (kinds: braces/brackets, Colon, Comma, Str, Num, Bool, Null, Ws, Bad) |

## Gotchas

- An infinite source of characters that never COMPLETES a token
  (e.g. endless `'0'` into a number) never emits — alternate the
  input in laziness tests.
- Reconvergence needs a newline after the edit; a one-line document
  relexes fully (still correct).
- Scanner states must be compared through `key`, never `==`, once
  they carry positions.

## Performance note (measured, honest)

Chunked lexing is SLOWER than element-wise on an in-memory string
(50.7us vs 42.8 on a 2.5KB document, after two optimisations that
took it from 55.2). The reason is NOT what was first written here:
per-character boxing was the hypothesis, and unboxing the storage
(`Chunks.ofChars`) plus reading the primitive array directly bought
only 8% of a 23% gap. What remains is per-CHUNK bookkeeping — a
builder, a token chunk and a tree node for every input chunk —
against one builder and no chunk machinery element-wise.

So choose by need, not by folklore: `Scan.chunks` is for STREAMING
and constant memory over a source you cannot materialize (a socket, a
huge file); `Scan.all` is for a string you already hold. Both derive
from one Scan, and they agree token for token. Full numbers and the
probe: [benchmarks](../benchmarks.md).

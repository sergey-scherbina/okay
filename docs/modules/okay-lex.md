# okay-lex

Total streaming tokenization.

- `Scan[K, S]` — a pure step function (`init/step/flush`); the state
  is a VALUE, which is what crosses chunk boundaries and snapshots.
  `key` (a position-erased fingerprint) and `rebase` (shift the
  positions) are what make incremental relexing possible when states
  carry absolute positions.
- Totality: every character lands in a token — unrecognized input on
  the Error CHANNEL, an unterminated string finished by flush. The
  lossless law: concatenated lexemes of all channels == the input.
- `Scan.stage` — the scanner as a pipeline Stage (chars in, tokens
  out, lazily). `Scan.chunks` — the chunked performance path: a chunk
  of chars in, a chunk of tokens out, one tight while per chunk, the
  SAME Scan (the state crosses boundaries as a value, so a token
  spanning chunks is emitted exactly once, where it completes).
  `Scan.all` — snapshots every N chars. `Scan.relex` — resume from
  the nearest snapshot, relex the damage, RECONVERGE past the next
  newline and reuse the old tail with shifted spans; no convergence
  means a full (still correct) relex.
- `Json.scan` is the proving dialect; okay-llm's `Bpe` implements the
  same interface (a tokenizer is a Scan, whatever its dictionary).

```scala
// element-wise and chunked agree at every chunk size:
Scan.all(Json.scan)(input).tokens
Chunks.fold(Scan.chunks(Json.scan)(Chunks.fromIterator(input.iterator, 5)))
```

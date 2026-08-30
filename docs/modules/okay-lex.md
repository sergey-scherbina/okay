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
  out, lazily). `Scan.all` — snapshots every N chars. `Scan.relex` —
  resume from the nearest snapshot, relex the damage, RECONVERGE past
  the next newline and reuse the old tail with shifted spans; no
  convergence means a full (still correct) relex.
- `Json.scan` is the proving dialect.

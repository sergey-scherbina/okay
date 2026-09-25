## xml-tokens-stream - Xml.tokens: scan's tokens from a Reader, 63x less allocated

Found measuring okay-watch's start (its perf-start-peak): 80% of every byte
allocated while three sanctions lists (55 MB of XML) were read was
`Xml.scan.stepInto` — per character an `S.copy`, a `P`, and `buf + c`,
which copies the whole lexeme per character; in Text mode
`buf.forall(_.isWhitespace)` also rescanned the run per character.

- `Xml.tokens(reader, chunk)(emit)`: the same tokens as `scan`, on one
  StringBuilder and six ints, for a document read once. `scan` stays what
  incremental relexing needs (value state, `key`, `rebase`).
- `scan`'s Text mode reads the first character's class instead of
  rescanning the run (a run is one class by construction).
- TestXmlTokens: equal to `scan` on 3000 random inputs over the characters
  every mode turns on, at chunk sizes 1, 2, 3, 7 and 64 KB; a document with
  every construct and an unterminated tail.
- Measured (history.d, not JMH, median of 5 alternating): on OFAC, EU and UN
  (4 807 352 tokens) 5174 → 1641 ms and 10 334 → 164 MB allocated.
- Gate: `affected master` 4569 GREEN, no warnings; after the last rebase
  onto core-only changes, okay-codec and the history/snippet checks (290).

Landed as b9d0ee58c.

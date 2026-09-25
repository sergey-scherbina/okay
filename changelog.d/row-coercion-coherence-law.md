## row-coercion-coherence-law - widen and normalize proved one tree over every core signature, by Bisim

- `TestRowCoherence` is a law: `Bisim.check(!.widen(p), !.normalize(p))`
  is `Same`, with ended paths. It covers State, Reader, Writer,
  Writer + Stop, the deferred shapes, a 200-deep fold, State + Writer,
  two `Tag` keys over one signature and a `Writer.byValue` pair. The
  widened programs also run as the originals did.
- Mutant: an `into` that swapped two `Tag` keys (the row "reordered")
  was caught with the path `left performed Tag(big,Get()), right
  performed Tag(small,Get())`.
- okay2's twin: its widen is subtyping and returns the program itself,
  so one road, pinned with `eq` and a run.
- docs/equivalence.md explains coherence (Biernacki & Polesiuk, LMCS
  2018).

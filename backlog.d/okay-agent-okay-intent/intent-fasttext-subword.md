- [ ] intent-fasttext-subword — subword embeddings TRAINED on the
      corpus plus a linear head, i.e. fastText's actual algorithm in
      plain Scala. Bridges chargrams (language-agnostic, no network,
      60%) and the probe (86.7%, needs a server): a trained
      representation that still ships as an array. Only worth it if
      `intent-embedding-choice` says the server is the problem.
      GATED 2026-09-07, the gate measured: "only worth it if the
      server is the problem" — intent-4b-with-more-data found both
      embedders flat from 32 examples with the same slope, and every
      no-network tier since (chargrams 65, TF-IDF 61.7, the static
      table at 68.3 with triples and PCA) meets the same ceiling from
      a different road: the limit is register and context, not the
      server. A trained subword head would be a fourth road to it.
      Opens if a fixture at least twice this size shows a late slope.

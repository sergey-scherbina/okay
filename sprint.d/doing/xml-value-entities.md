- [ ] xml-value-entities — `Xml.value` (the XML → Json projection,
      refine-fpml-prover) copies text and attribute lexemes as written,
      so `<indexName>S&amp;P/ISDA…</indexName>` reads as the string
      `S&amp;P…` and okay-fin's cross-format law (FpML vs CDM, whose JSON
      says `S&P`) failed on it (2026-09-29). THE ASK: `Xml.unescape` —
      the five predefined entities and numeric references (`&#NN;`,
      `&#xHH;`), an unknown entity left as written — applied by `value`
      to text and attribute values; the lossless tree untouched, `Xml.text`
      untouched (its tests pin lexemes). okay2 in step. (2026-09-29,
      from okay-fin's corpus law)

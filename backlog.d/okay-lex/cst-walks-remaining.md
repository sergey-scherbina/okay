- [ ] cst-walks-remaining: the Cst walks still recursing per nesting
      level after `cst-walk-stack-safe` (okay-parse) and
      `xml-projection-stack-safe` (okay-codec's Xml). A survey on
      2026-09-25 (`git grep "kids\.(map|flatMap|foreach)"`) found
      `Yaml.values` (okay-codec/Yaml.scala:286-287), `Split.tokens`
      (okay-rag/Split.scala:93), and `Symbols.walk`/`toks`
      (okay-rag/Symbols.scala:84, :154). `Json.into` is already
      trampolined. Each needs the same fix as the other two: a pre-order
      walk on an explicit stack, with a depth test that overflows first.
      YAML nests by indentation and a code CST by blocks, so both can go
      as deep as their input does. (2026-09-25)

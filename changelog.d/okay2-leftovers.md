## okay2-leftovers - the last small core types; the port's closing list

The core's `Safe`/`Unsafe` aliases and its comonad handler are now in
okay2. A row whose operations form a `Comonad` is run by `extract`, with
no handler written (`Handler.comonad`). The lane also records what stays
Scala 3 only (specs/okay2.md stage 30, docs/okay2.md §8):
- the direct-style macros;
- the context-function types;
- `Member`;
- the operation values for Clojure/Frege;
- the JS/Native platform files.

2 tests.

Docs: docs/okay2.md sections 3 and 8.

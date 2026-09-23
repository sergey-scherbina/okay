## edn-codec - EDN, Clojure's data notation, as an okay codec

okay-codec gains `Edn`, a third format beside JSON and CBOR through the
same `Schema`. `Edn.write`/`Edn.read` and the `Edn` tree with
`parse`/`show` keep what JSON flattens: keyword keys for a product,
integers exact to 64 bits (`123N` beyond), `\c` characters, sets, and a
variant named by a tag, `#Shape/Rect {:w 2.0 :h 3.5}`. Text is read and
printed with an explicit stack. A `Schema` is encoded by JSON's `Step`
algebra and decoded on JSON's two roads, so a 20 000-deep document and a
5 000-link recursive value run on the default stack. A mutant without
the `Cont` road overflows it.

The tests pass 8/8 on JVM, JS and Native. Checked against Clojure itself
in okay-clojure: `clojure.edn/read-string` reads okay's text, and
`Edn.read` reads Clojure's `pr-str` into the typed value. Docs:
docs/modules/okay-codec.md, docs/jvm-languages.md; specs/codecs.md. The
spec section followed the code, and says so.

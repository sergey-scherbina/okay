## okay2-xml - the XML/HTML nesting prover in okay2-codec

This lane ports okay-codec's `Xml` into okay2-codec (spec stage 43,
docs §32). It is the lossless XML/HTML dialect over okay2-lex-parse,
and it nests by NAMED tags:
- a mismatched close closes the elements it skips and reports each one;
- a close with nothing open becomes an error leaf;
- void elements and `/>` never open a frame;
- comments and CDATA are kept whole;
- `render(cst(s)) == s` holds for every string;
- an incremental reparse equals a full parse.

The ported suite is TestXml. It runs on the JVM, Scala.js and Scala
Native.

`text` and `elements` walk the tree on an explicit stack. A test at
20 000 levels checks this, and a mutant with the recursive form throws
StackOverflowError there. The Scala 3 originals recurse per level, so
they have the same defect. It is filed as `xml-projection-stack-safe`.

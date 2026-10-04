## okay2-codec-text - EDN, YAML, Markdown and the staged codecs in okay2-codec

The second and last lane porting the rest of okay-codec's codecs to okay2
(okay2/backlog.d/modules/okay2-codec-dialects, spec stage 55, docs/okay2.md
section 33). `Edn` is Clojure's data notation: an iterative reader and
printer, and a Schema through EDN with keyword keys, exact integers,
tagged sums and `#okay/bytes`. `Yaml` is the indentation dialect: a
lossless CST whose projection lands in `Json`, so `Yaml.read` decodes
through the JSON algebra. `Markdown` is the reframing dialect: crossing
emphasis is closed and reopened, and unclosed emphasis is an error node.
`Staged.json`, `Staged.cbor` and `Staged.strict` generate a codec at the
call site and agree with the folds on every fixture.

What differs from Scala 3: `Staged` is a Scala 2 blackbox macro
(`StagedMacro`) that reads the class as `SchemaMacro` does, where Scala 3
reads a `Mirror` in a quote. The JSON leaves go through small run-time
helpers, and the CBOR and strict products are made from their slots by a
cast per field in generated code, as `SchemaMacro` does. `JsonStrict.Reader`
gained `enter` and `leave`. EDN's decoder reads leaves through one
`Schema.Visit` shared by both roads.

Ported suites: TestEdn, TestYamlMarkdown (the YAML and Markdown halves of
TestCodec), TestTextLaws (the YAML and Markdown laws of TestLaws, plus a
JSON/CBOR/EDN round-trip property), TestStaged, TestStagedCbor,
TestJsonStrictStaged, and TestTextDepth on the JVM (TestYamlDepth and
TestCborEdnDepth on a 256 KB stack).

This closes okay2-codec-dialects. The okay-codec files that list never
named (JsonOptic, Policy, Journalled, Stubs, TsTypes, Wire, Staging and
their JVM helpers) are filed as okay2/backlog.d/modules/okay2-codec-tooling.

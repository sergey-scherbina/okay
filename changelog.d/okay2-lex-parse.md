## okay2-lex-parse - lossless, total lexing and parsing for okay2

First of the modules the operator asked for (json, xml, sql, http):
okay-lex and okay-parse as `okay2-lex`/`okay2-parse`, cross on JVM,
Scala.js and Scala Native. `Scan`/`ScanInto` (the lexer as a pure step
function, the sink road), its drivers (`stage`, `chunks`, `fold`,
`aggregate`, `all`, incremental `relex`), `Mealy` as an arrow, the total
JSON lexer; the instruction language, the total builder into a lossless
`Cst`, `Parse.full`/`reparse` with node-boundary snapshots, and the JSON
driver in both surfaces. The Scala 3 suites ported (TestLex, TestMealy,
TestParse, TestLaws with ScalaCheck). Found on the way: `Cst.lexemes`/
`errors` recursed per nesting level and `errors` overflowed at 20 000
levels where the parse did not — okay2 walks on an explicit stack; the
Scala 3 core's half is filed as `cst-walk-stack-safe`. Spec stage 40,
docs §32.

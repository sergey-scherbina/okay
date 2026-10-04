## okay2-codec-optics - JsonOptic, Policy and Journalled in okay2-codec

The first lane of okay2/backlog.d/modules/okay2-codec-tooling (spec stage
57, docs/okay2.md section 33). `JsonOptic` puts okay2-optics on `Json`.
`at` is the lawful lens over `Option[Json]`. `field`, `index` and
`caseOf` are affines, `values` and `entries` are traversals, and
`creating` is a lawful lens through `Iso.non` that creates missing
parents. `path` reads a dotted key against the Schema, bounded by
`MaxSegments`. A zipper plate comes with `removeChild`/`insertChild`.
`Policy` is a projection policy whose `touches` describes exactly what
`project` removes. `Journalled[F <: Row]` is what a journal needs of an
operation.

What differs from Scala 3: the traversals are `Walk`s. Scala 2 infers
`andThen`'s constraint from an expected type that is in view and then
refuses the prism, so each composition is bound to a `val` before it is
returned. `creating` is a fold. `Journalled`'s cast-free instance is a
method per case, because Scala 2 does not refine `A` on a pattern.

The ported suites are TestJsonOptic, TestCreatingPath, TestJsonZipper,
TestPolicy and a new TestJournalled, all on JVM, Scala.js and Scala
Native, plus TestJsonOpticDepth on the JVM (33 tests).

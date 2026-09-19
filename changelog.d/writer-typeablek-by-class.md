## writer-typeablek-by-class - Writer's default TypeableK is the class of Say

`Writer`'s row split used to test the told VALUE's class through a
`Typeable[W]` — the finer question that routes `Writer % String +
Writer % Int` in one row — as the DEFAULT, and every call site whose
W was parameterised (`Chunk[Byte]`, `Either[Bad, A]`, an abstract `O`)
paid for it with an E092 "cannot be checked at runtime" warning that
this build treats as red: 23 `@nowarn("msg=cannot be checked at
runtime")` across ten modules, each repeating the same caveat, plus
four hand-written `given Typeable[Chunk[Byte]]` (okay-http's `Http`,
TestHttp, TestNetty, TestJetty) written only to dodge it. No module
held two Writers in one row.

Now `writerK` tests the class of `Say` alone — total, since `Say` is
Writer's only constructor and class-distinct from every other
signature's operations — and summons nothing. The finer test is
`Writer.byValue.writerK`, an opt-in (`import okay.Writer.byValue.given`)
declared `TypeableK.ByValue` so `Distinct` reads it; without the import
`Distinct` refuses a two-Writer row, which Distinct.scala names as the
safe direction (an unmarked instance is refused and fixed by one
import, where the reverse would pass a row that misroutes).
TestRowIdentity and TestDistinct import it; TestDistinct also uses the
import in the open, because the macro-time search that actually needs
it does not count as a use for the unused-import lint.

Removed: the 23 annotations with their comments, the four givens, and
one dead `nowarn` import. What remains under that message is the
`Ask[Nothing]`/`Claim + Produce` relay sites in TestHandleForward and
HandlerBenchmark — a different test, not Writer's. Docs:
Writer.scala's own doc, Effects.scala and Distinct.scala's paragraphs
naming the one ByValue instance, docs/many-instances.md.

Filed alongside, from the same wrap-up (backlog.d/okay-core):
chunks-foldwriter-docs-condense, feed-stream-given-name,
producer-effectful-stream-iterator, okay-watch-pointer-bump-check.

Files: src/main/scala/Writer.scala, Effects.scala, Distinct.scala;
src/test/scala/TestRowIdentity.scala, TestDistinct.scala; the 23
annotation sites (okay-blob 5, okay-stream 7, compare 6, okay-sql,
okay-jdbc, okay-kafka, okay-outbox, okay-demo); okay-http Http.scala,
TestHttp; okay-netty TestNetty; okay-jetty TestJetty;
docs/many-instances.md.

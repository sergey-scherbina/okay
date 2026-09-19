- [ ] chunks-foldwriter-docs-condense — `Chunks.foldLeftWriter` and
      `Chunks.foldWriter` (okay-stream/src/main/scala/Chunks.scala,
      ~351 and ~473) were built for three call sites that turned out
      to fold a PURE `Chunks[A]`, so after producer-to-writer-carrier
      they have zero callers in main (tests + one JMH row only);
      `foldWriter` needs `CanBlock` and does not build on JS. Their
      doc comments are ~120 lines of the investigation's saga in a
      624-line file. Kept by the operator's "nothing deleted" line;
      what remains to do is prose: condense each doc to what the
      combinator IS (a G-effectful chunk fold for `Source[Chunk[A]]`,
      JVM/Native for `foldWriter`) and point at
      specs/producer-to-writer-carrier.md Results for the history.
      Docs-only lane, needs the gate like any other.

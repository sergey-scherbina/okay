## scala2-stores - okay-cache, okay-blob, okay-docs from Scala 2.13

Stage 15.4 of specs/scala2-facade.md (operator: "делай всё что возможно
чтобы работало в скале 2").

- Probed from scalac 2.13.18: every store is built with its own
  constructor (`Cache.memory`, `Fs(root)`, `S3.wired`, `View(...)`), and
  the plain values (`Regime`, `Etag`, `Meta`, `Cond`, `PutResult`,
  `Stats`) are used directly. Every operation answers a program.
- The new module okay-scala2-stores adds `Caches`, `Blobs` and
  `Documents`: the operations over `Eff[Async, A]` and `Source[A]`
  (chunked listings and queries flattened; trait-method defaults such
  as `Cond.Always` restated).
- A new TASTy-reader fact: `new okay.docs.TopicDocs[A](topic)` is
  refused ("Unsupported Scala 3 union in bounds of type +") although its
  constructor mentions no row. `new` completes the whole class, and its
  `query` answers a `Source`. A method answering such a class is fine,
  so `Documents.onTopic` is a factory. Found by bisecting the test file.
  Recorded in docs/scala2.md section 10.
- `TestStoresFromScala2` has 6 tests: single-flight `getOrLoad` under
  two fibers, write-through ordering, cross-node `drain`, blob
  put/get/list/stream/head/delete, `putFile` plus a persist backup and
  restore, and conditional document writes with an indexed query.
- Docs: section 8m of docs/scala2.md (copied from the probe), the
  module page, the API reference, and the spec's stage 15.4.

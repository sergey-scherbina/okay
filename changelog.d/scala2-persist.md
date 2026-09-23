## scala2-persist - okay-persist from Scala 2.13: Persist

Stage 15.2 of specs/scala2-facade.md (operator: "делай всё что возможно
чтобы работало в скале 2").

- Probed from scalac 2.13.18: okay-persist's engine (`MemoryStore`,
  `FileStore`, topics, `Offsets`, `Snapshots`, `Typed`) is synchronous
  and readable, and Scala 2 uses it directly.
- Two reader facts found: the defaults of a TRAIT's abstract method are
  invisible from Scala 2 (`store.topic("t")` asks for every argument),
  and so is an extension (`topic.of[A]`).
- The new module okay-scala2-persist adds `Persist`: `topic` with the
  defaults, `typed`, and `stream`/`tail` as a `Source[Record]` (the
  engine's chunked streams flattened).
- `TestPersistFromScala2` has 4 tests: the engine directly, the typed
  view, a FileStore reopen, and a tail that sees appends made after it
  started.
- Docs: section 8k of docs/scala2.md (copied from the probe), the
  module page, the API reference, and the spec's stage 15.2.

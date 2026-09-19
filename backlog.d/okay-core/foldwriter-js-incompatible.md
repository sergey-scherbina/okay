- [ ] foldwriter-js-incompatible — `Chunks.foldWriter` (okay-stream/
      src/main/scala/Chunks.scala) requires `CanBlock`, which JS does
      not have (`src/main/scala-js/Platform.scala`), and JS has no
      `Handler[Async]` at all — it drives `Async` through a callback
      `Scheduler`/`Fiber` (`Async.PromiseDrive`).
      MOOT FOR THE MIGRATION (2026-09-19,
      producer-writer-carrier-pure-iterator): the three call sites it
      was built for (`Bulk.scala:106`, `Pipeline.scala:92`,
      `Acceptance.scala:29`) all fold a PURE `Chunks[A]` under
      `Handler[Pure]` — they never needed a G-effectful fold, and the
      pure writer stream now has the `iterator` override they will walk
      once `Chunks` retypes (spec Results). What remains is only this:
      IF a `Source[Chunk[A]]` (Async) ever needs a `Fold`-dispatched
      eager fold on JS, `foldWriter`'s blocking walk cannot be it — a
      callback/event-loop-driven walk would. No caller asks today; do
      not build it ahead of one.

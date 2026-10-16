- [ ] ui-js-sse-source — the browser's end of a server push: a server-sent
      events SOURCE for the Scala.js host, so a page can be a pure
      `Ui.run` program whose world is a stream from the server.
      ASKED 2026-10-16 by okay-watch's `coinpath` (okay-watch
      specs/coinpath.md): a free public page, written in okay-ui on the
      DOM host, that follows a job the backend streams step by step.
      WHAT IS THERE: the server half. `Http.streams` answers "does this
      content-type say STREAM", all three backends (jetty, the JDK
      server, netty) write `text/event-stream` chunk by chunk
      (changelog http-streaming-responses), and `okay.ui.Dom` drives a
      tree and turns the user's events into ours. WHAT IS NOT: nothing in
      okay-ui's JS platform turns an `EventSource` (or a `fetch` body
      stream) into a `Source`, so a page that is "fold over merged
      sources" has no way to receive the server's side of the story
      except by hand-written script, which is exactly what the page is
      meant not to need.
      THE SHAPE: `okay.ui.Sse.source(url, decode)` in the JS platform,
      next to `Dom`: opens the connection when the loop subscribes,
      closes it when the loop ends (the bracket shape, like
      `Terminal.raw`), yields each event decoded by `decode` as the
      app's own message, reconnects as `EventSource` does and presents
      the event `id` so a server that replays from `Last-Event-ID` can
      catch a late subscriber up. Like `Dom.host(document, root)` it takes
      the browser's global as a parameter (`js.Dynamic`), never reaches
      for `window` itself, so the whole thing is testable on the JVM
      with a fake. Its sibling: a POLLING source (`Poll.source(url,
      every, decode)` over `fetch`) for the networks and proxies that cut
      a long response, same type, so the page chooses by data and not by
      branch. If a `fetch`-as-effect for the JS platform does not exist
      yet, it is the second half of this item and is filed here rather
      than found at the end. CHECK FIRST how `Ui.run`'s `external` is
      typed and whether a `Source[Event]` or a `Source[Msg]` is what it
      merges.
      TESTS, none needing a browser: a fake `EventSource` that emits
      three events, an error and a reconnect — the source yields the
      three, then the reconnect's replay without duplicates (by id), and
      closes on loop end; a fake `fetch` for the polling source; the
      decoder's failure is a message, not an exception that kills the
      loop. ONE END-TO-END run in `okay-demo-e2e-browser` against a real
      streaming route is the proof that matters, because a fake agrees
      with its author.
      NOT HERE: authentication, or any notion of a job — the source knows
      a URL and a decoder, the application knows what a message means.
      TRIGGER: coinpath stage 1 (okay-watch). It is the one missing
      piece between the page and the engine; everything else the page
      needs is already in okay-ui.

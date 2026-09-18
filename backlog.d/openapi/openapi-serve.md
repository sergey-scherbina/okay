- [x] openapi-serve — DONE 2026-09-11 (4758e8f7). `/openapi.json` and
      a page rendered on the server with no network, plus okay-demo's
      committed document and its drift test — the shape
      okay-demo/deploy already has. The drift test earned its keep on
      its first run: `/app.js` is served only where the linked bundle
      exists, so the document describes the PACKAGED surface.
      What it exposed for the next lane: all six demo operations still
      render `undeclared`, because HTML, server-sent events and a byte
      bundle have no `Schema` to declare them. `out`/`jsonOut` cover
      JSON only, so the answer is an `Answer` that carries a MEDIA
      TYPE and combinators that encode it — same declaration-by-
      construction, wider than JSON.

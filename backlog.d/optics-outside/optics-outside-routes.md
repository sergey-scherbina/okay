- [x] optics-outside-routes — DONE (2026-09-10, dcc1d76e). A route is a prism. One declaration
      answers three questions: does this path match (server), what is
      the path for these parameters (client, reverse routing), and
      what does it look like (OpenAPI, an MCP tool). The prism law
      `unapply(url(a)) == Some(a)` is the feature, not a nicety.
      Motivated by what is in the tree now: every `routes` in the
      repository is `case r if r.method == Get && r.url == "/healthz"`
      (okay-ops, okay-admin, okay-acme, okay-demo) — no route has a
      typed parameter at all, and the one router that does have
      parameters (okay-script's `Site.resolve`) hands them back as an
      untyped `Map[String, String]` and splits on `/` before decoding,
      so an encoded `%2F` inside a parameter becomes a segment
      boundary. Stage 1 of the spec. Landed WITH a caller:
      `Acceptance.routes` became a `Router`, and the conversion found
      two live defects — `startsWith("/person")` answered `/personal`,
      and every route answered every verb.
